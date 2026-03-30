{-# LANGUAGE ScopedTypeVariables #-}

module Build.Formula.DFU where

import Build.Compiler
import Build.Formula
import Build.Shake
import Core.Context
import Core.Formula
import Core.Formula.DFU
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
    run (convert "main" mainImpl) startMainFirmware maxMainLength
    run (convert "dfu" dfuImpl) startDfuFirmware maxDfuLength
  where
    mainImpl = fixIRQ $ implementation transport
    dfuImpl = I.dfu startMainFirmware transport
    startMainFirmware = startDfuFirmware + maxDfuLength
    maxMainLength = sizeFlash mcu - maxDfuLength
    startDfuFirmware = startFlash mcu
    run formula startFirmware maxLength = build (mkCompiler formula startFirmware maxLength) formula
    convert postfix =
        Formula
            (name <> "-" <> postfix)
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
