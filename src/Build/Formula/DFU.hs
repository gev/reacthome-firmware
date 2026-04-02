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
import Development.Shake.FilePath
import Implementation.Dfu qualified as I
import Interface.MCU
import Ivory.Language
import Support.CMSIS.CoreCMFunc
import System.Directory

mkDFU ::
    (Compiler c p, Shake c) =>
    Int ->
    (forall s. Int -> Ivory (ProcEffects s ()) ()) ->
    (Formula p -> Int -> Int -> c) ->
    DFU p ->
    IO ()
mkDFU maxDfuLength setVectorTable mkCompiler DFU{..} = do
    main <- prepare (convert mainImpl) startMainFirmware maxMainLength "main"
    dfu <- prepare (convert dfuImpl) startDfuFirmware maxDfuLength "dfu"

    let firmWareName = mkName meta $ Just "firmware"
        firmWarePath = "dist" </> "firmware" </> firmWareName <.> "hex"
    combine main dfu firmWarePath

    let upName = mkName meta Nothing
        updatePath = "dist" </> "up" </> upName <.> "up"
    pack main updatePath
  where
    mainImpl = fixIRQ $ implementation transport
    dfuImpl = I.dfu startMainFirmware transport

    startDfuFirmware = meta.mcu.startFlash
    startMainFirmware = startDfuFirmware + maxDfuLength
    maxMainLength = meta.mcu.sizeFlash - maxDfuLength

    convert = Formula meta

    prepare formula startFirmware maxLength postfix = do
        let name = mkName formula.meta (Just postfix)
            path = postfix </> name
        build (mkCompiler formula startFirmware maxLength) formula path name
        T.readFile $ "dist" </> path <.> "hex"

    combine main dfu path = T.writeFile path (truncateHex dfu <> main)

    pack main path = do
        let mcu = toLower <$> (meta.mcu.model <> meta.mcu.modification)
            header =
                L.toStrict . B.toLazyText $
                    B.singleton '#'
                        <> hexadecimal meta.model
                        <> hexadecimal meta.board
                        <> hexadecimal (fst meta.version)
                        <> hexadecimal (snd meta.version)
                        <> hexadecimal (length mcu)
                        <> mconcat (B.singleton <$> mcu)
        createDirectoryIfMissing True $
            takeDirectory path
        T.writeFile path $
            T.intercalate "\n" [header, main]

    fixIRQ impl = do
        addInit "fix_IRQ" do
            setVectorTable startMainFirmware
            enableIRQ
        impl

    truncateHex = T.unlines . init . T.lines

    hexadecimal n
        | n < 16 = B.singleton '0' <> B.hexadecimal n
        | otherwise = B.hexadecimal n
