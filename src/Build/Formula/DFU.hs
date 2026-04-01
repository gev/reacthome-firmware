module Build.Formula.DFU where

import Build.Compiler
import Build.Formula
import Build.Shake
import Core.Context
import Core.Formula
import Core.Formula.DFU
import Core.Meta (mcu, mkName)
import Development.Shake.FilePath
import Implementation.Dfu qualified as I
import Interface.MCU
import Ivory.Language
import Support.CMSIS.CoreCMFunc

mkDFU ::
    (Compiler c p, Shake c) =>
    Int ->
    (forall s. Int -> Ivory (ProcEffects s ()) ()) ->
    (Formula p -> Int -> Int -> c) ->
    DFU p ->
    IO ()
mkDFU maxDfuLength setVectorTable mkCompiler DFU{..} = do
    let name = mkName meta $ Just "firmware"
    let path = "dist" </> "firmware" </> name <.> "hex"
    mainPath <- prepare (convert mainImpl) startMainFirmware maxMainLength "main"
    dfuPath <- prepare (convert dfuImpl) startDfuFirmware maxDfuLength "dfu"
    combine mainPath dfuPath path
  where
    -- combine mainPath dfuPath

    mainImpl = fixIRQ $ implementation transport
    dfuImpl = I.dfu startMainFirmware transport

    startDfuFirmware = meta.mcu.startFlash
    startMainFirmware = startDfuFirmware + maxDfuLength
    maxMainLength = meta.mcu.sizeFlash - maxDfuLength

    convert = Formula meta

    fixIRQ impl = do
        addInit "fix_IRQ" do
            setVectorTable startMainFirmware
            enableIRQ
        impl

    prepare formula startFirmware maxLength postfix = do
        let name = mkName formula.meta (Just postfix)
            path = postfix </> name
        build (mkCompiler formula startFirmware maxLength) formula path name
        pure $ "dist" </> path <.> "hex"

    combine mainPath dfuPath path = do
        print mainPath
        print dfuPath
        print path
