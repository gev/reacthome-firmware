{-# LANGUAGE OverloadedStrings #-}

module Build.Formula.DFU where

import Build.Compiler
import Build.Formula
import Build.Shake
import Core.Context
import Core.Formula
import Core.Formula.DFU
import Core.Meta (board, mcu, mkNameDfu, model, version)
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Internal.Builder qualified as B
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Builder.Int qualified as B
import Data.Text.Read qualified as T
import Data.Util (unPack16BE)
import Data.Word
import Development.Shake.FilePath
import Implementation.Dfu qualified as I
import Interface.MCU
import Ivory.Language
import Support.CMSIS.CoreCMFunc
import System.Directory

mkDFU ::
    (Compiler c p, Shake c) =>
    Int ->
    (Word8, Word8) ->
    (forall s. Int -> Ivory (ProcEffects s ()) ()) ->
    (Formula p -> Int -> Int -> c) ->
    DFU p ->
    IO ()
mkDFU maxDfuLength dfuVersion setVectorTable mkCompiler DFU{..} = do
    main <- prepare (convert mainImpl) startMainFirmware maxMainLength "main"
    dfu <- prepare (convert dfuImpl) startDfuFirmware maxDfuLength "dfu"
    combine main dfu firmWarePath
    pack main updatePath
    removeDirectoryRecursive $ "dist" </> "main"
    removeDirectoryRecursive $ "dist" </> "dfu"
  where
    name = mkNameDfu meta dfuVersion
    firmWarePath = "dist" </> "firmware" </> name <.> "hex"
    updatePath = "dist" </> "up" </> name <.> "up"

    mainImpl = fixIRQ $ implementation transport
    dfuImpl = I.dfu startMainFirmware dfuVersion transport

    startDfuFirmware = meta.mcu.startFlash
    startMainFirmware = startDfuFirmware + maxDfuLength
    maxMainLength = meta.mcu.sizeFlash - maxDfuLength

    convert = Formula meta

    prepare formula startFirmware maxLength target = do
        let compiler = mkCompiler formula startFirmware maxLength
            path = target </> name
        T.readFile =<< build compiler formula path name

    combine main dfu path = do
        createDirectoryIfMissing True $
            takeDirectory path
        T.writeFile path (truncateHex dfu <> main)

    pack main path = do
        let main' = upHex main
        createDirectoryIfMissing True $
            takeDirectory path
        T.writeFile path do
            T.intercalate "\n" $ header main' : main'

    header main =
        L.toStrict . B.toLazyText $
            mconcat (toHex <$> unPack16BE (fromIntegral $ length main))
                <> mconcat (toHex <$> unPack16BE meta.model)
                <> toHex meta.board
                <> toHex (fst meta.version)
                <> toHex (snd meta.version)
                <> toHex (fst dfuVersion)
                <> toHex (snd dfuVersion)
                <> mconcat (toHex <$> mcu)

    upHex hex = filterHex $ parseHex <$> T.lines hex

    parseHex hex =
        let
            hex' = T.tail hex
            size = fromHex $ T.take 2 hex'
            head = T.take 6 $ T.drop 2 hex'
            offset = T.take 4 head
            opcode = T.drop 4 head
            payload = T.take (size * 2) $ T.drop 8 hex'
         in
            (offset, opcode, payload)

    filterHex = reverse . filterHex' [] "0000"

    filterHex' acc _ [] = acc
    filterHex' acc base (h : hs) =
        case h of
            (offset, "00", payload) -> filterHex' ((base <> offset <> payload) : acc) base hs
            (_, "04", payload) -> filterHex' acc payload hs
            _ -> filterHex' acc base hs

    mcu = fromEnum . toLower <$> (meta.mcu.model <> meta.mcu.modification)

    fixIRQ impl = do
        addInit "fix_IRQ" do
            setVectorTable startMainFirmware
            enableIRQ
        impl

    truncateHex = T.unlines . init . T.lines

    toHex n
        | n < 16 = B.singleton '0' <> B.hexadecimal n
        | otherwise = B.hexadecimal n

    fromHex t = case T.hexadecimal t of
        Right (val, _) -> val -- Partially parsed (trailing chars ignored)
        Left err -> error err
