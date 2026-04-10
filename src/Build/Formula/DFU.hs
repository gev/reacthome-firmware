module Build.Formula.DFU where

import Build.Compiler
import Build.Formula
import Build.Shake
import Core.Context
import Core.Formula
import Core.Formula.DFU
import Core.Meta (board, mcu, mkName, model, version)
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Internal.Builder qualified as B
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Builder.Int qualified as B
import Data.Util (unPack16BE)
import Development.Shake.FilePath
import Implementation.Dfu qualified as I
import Interface.MCU
import Ivory.Language
import Support.CMSIS.CoreCMFunc
import System.Directory
import Data.Word

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
    name = mkName meta
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

    combine main dfu path = T.writeFile path (truncateHex dfu <> main)

    pack main path = do
        createDirectoryIfMissing True $
            takeDirectory path
        T.writeFile path $
            T.intercalate "\n" [header, main]

    header =
        L.toStrict . B.toLazyText $
            B.singleton '#'
                <> mconcat (hexadecimal <$> unPack16BE meta.model)
                <> hexadecimal meta.board
                <> hexadecimal (fst meta.version)
                <> hexadecimal (snd meta.version)
                <> B.fromString mcu

    mcu = toLower <$> (meta.mcu.model <> meta.mcu.modification)

    fixIRQ impl = do
        addInit "fix_IRQ" do
            setVectorTable startMainFirmware
            enableIRQ
        impl

    truncateHex = T.unlines . init . T.lines

    hexadecimal n
        | n < 16 = B.singleton '0' <> B.hexadecimal n
        | otherwise = B.hexadecimal n
