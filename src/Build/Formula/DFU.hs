module Build.Formula.DFU where

import Build.Compiler
import Build.Formula
import Build.Shake
import Core.Context
import Core.Formula
import Core.Formula.DFU
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
    -- let name = mkName
    mainPath <- prepare (convert mainImpl) startMainFirmware maxMainLength "main"
    dfuPath <- prepare (convert dfuImpl) startDfuFirmware maxDfuLength "dfu"
  where
    -- combine mainPath dfuPath

    mainImpl = fixIRQ $ implementation transport
    dfuImpl = I.dfu startMainFirmware transport

    startDfuFirmware = startFlash mcu
    startMainFirmware = startDfuFirmware + maxDfuLength
    maxMainLength = sizeFlash mcu - maxDfuLength

    prepare formula startFirmware maxLength postfix = do
        let name = mkName formula (Just postfix)
            path = postfix </> name
        build (mkCompiler formula startFirmware maxLength) formula path name
        pure $ "dist" </> path <.> "hex"

    convert =
        Formula
            name
            model
            version
            shouldInit
            mcu
            quartzFrequency
            systemFrequency

    fixIRQ impl = do
        addInit "fix_IRQ" do
            setVectorTable startMainFirmware
            enableIRQ
        impl
