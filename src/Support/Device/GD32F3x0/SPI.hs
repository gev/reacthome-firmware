{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.SPI (
    SPI_PARAM_STRUCT,
    SPI_PARAM,
    device_mode,
    trans_mode,
    frame_size,
    nss,
    endian,
    clock_polarity_phase,
    prescale,
    spiParam,
    SPI_PERIPH,
    spi0,
    SPI_I2S_FLAG,
    spi_flag_trans,
    SPI_MODE,
    spi_slave,
    spi_master,
    SPI_TRANSMODE,
    spi_transmode_fullduplex,
    SPI_FRAMESIZE,
    spi_framesize_8bit,
    spi_framesize_16bit,
    SPI_NSS,
    spi_nss_hard,
    spi_nss_soft,
    SPI_ENDIAN,
    spi_endian_msb,
    SPI_CK_PL_PHASE,
    spi_ck_pl_low_ph_1edge,
    spi_ck_pl_high_ph_1edge,
    SPI_PRESCALE,
    spi_psc_2,
    spi_psc_64,
    transmitSPII2S,
    deinitSPII2S,
    enableSpiNssOutput,
    enableSpiNsspMode,
    enableSPI,
    getFlagSPII2S,
    initSPI,
    inclSPI,
) where

-- spi_i2s_data_transmit
-- spi_i2s_flag_get
-- spi_i2s_deinit
-- spi_struct_para_init
-- spi_init
-- spi_enable

import Ivory.Language
import Ivory.Support
import Ivory.Support.Device.GD32F3x0

type SPI_PARAM_STRUCT = "spi_parameter_struct"
type SPI_PARAM s = Ref s (Struct SPI_PARAM_STRUCT)

[ivory|
    struct spi_parameter_struct
        { device_mode          :: Stored SPI_MODE
        ; trans_mode           :: Stored SPI_TRANSMODE
        ; frame_size           :: Stored SPI_FRAMESIZE
        ; nss                  :: Stored SPI_NSS
        ; endian               :: Stored SPI_ENDIAN
        ; clock_polarity_phase :: Stored SPI_CK_PL_PHASE
        ; prescale             :: Stored SPI_PRESCALE
        }
|]

spiParam ::
    [InitStruct SPI_PARAM_STRUCT] ->
    Init (Struct SPI_PARAM_STRUCT)
spiParam p =
    istruct $ p <+> spiDefaultParam

spiDefaultParam =
    [ device_mode .= ival spi_slave
    , trans_mode .= ival spi_transmode_fullduplex
    , frame_size .= ival spi_framesize_8bit
    , nss .= ival spi_nss_hard
    , endian .= ival spi_endian_msb
    , clock_polarity_phase .= ival spi_ck_pl_low_ph_1edge
    , prescale .= ival spi_psc_2
    ]

newtype SPI_PERIPH = SPI_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol SPI_PERIPH

spi0 = SPI_PERIPH $ ext "SPI0"

newtype SPI_I2S_FLAG = SPI_I2S_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

spi_flag_trans = SPI_I2S_FLAG $ ext "SPI_FLAG_TRANS"

newtype SPI_MODE = SPI_MODE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
spi_slave = SPI_MODE $ ext "SPI_SLAVE"
spi_master = SPI_MODE $ ext "SPI_MASTER"

newtype SPI_TRANSMODE = SPI_TRANSMODE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
spi_transmode_fullduplex = SPI_TRANSMODE $ ext "SPI_TRANSMODE_FULLDUPLEX"

newtype SPI_FRAMESIZE = SPI_FRAMESIZE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
spi_framesize_8bit = SPI_FRAMESIZE $ ext "SPI_FRAMESIZE_8BIT"
spi_framesize_16bit = SPI_FRAMESIZE $ ext "SPI_FRAMESIZE_16BIT"

newtype SPI_NSS = SPI_NSS Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
spi_nss_hard = SPI_NSS $ ext "SPI_NSS_HARD"
spi_nss_soft = SPI_NSS $ ext "SPI_NSS_SOFT"

newtype SPI_ENDIAN = SPI_ENDIAN Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
spi_endian_msb = SPI_ENDIAN $ ext "SPI_ENDIAN_MSB"

newtype SPI_CK_PL_PHASE = SPI_CK_PL_PHASE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
spi_ck_pl_low_ph_1edge = SPI_CK_PL_PHASE $ ext "SPI_CK_PL_LOW_PH_1EDGE"
spi_ck_pl_high_ph_1edge = SPI_CK_PL_PHASE $ ext "SPI_CK_PL_HIGH_PH_1EDGE"

newtype SPI_PRESCALE = SPI_PRESCALE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
spi_psc_2 = SPI_PRESCALE $ ext "SPI_PSC_2"
spi_psc_64 = SPI_PRESCALE $ ext "SPI_PSC_64"

enableSPI :: SPI_PERIPH -> Ivory eff ()
enableSPI = call_ spi_enable

spi_enable :: Def ('[SPI_PERIPH] :-> ())
spi_enable = fun "spi_enable"

deinitSPII2S :: SPI_PERIPH -> Ivory eff ()
deinitSPII2S = call_ spi_i2s_deinit

spi_i2s_deinit :: Def ('[SPI_PERIPH] :-> ())
spi_i2s_deinit = fun "spi_i2s_deinit"

enableSpiNssOutput :: SPI_PERIPH -> Ivory eff ()
enableSpiNssOutput = call_ spi_nss_output_enable

spi_nss_output_enable :: Def ('[SPI_PERIPH] :-> ())
spi_nss_output_enable = fun "spi_nss_output_enable"

enableSpiNsspMode :: SPI_PERIPH -> Ivory eff ()
enableSpiNsspMode = call_ spi_nssp_mode_enable

spi_nssp_mode_enable :: Def ('[SPI_PERIPH] :-> ())
spi_nssp_mode_enable = fun "spi_nssp_mode_enable"

transmitSPII2S :: SPI_PERIPH -> Uint16 -> Ivory eff ()
transmitSPII2S = call_ spi_i2s_data_transmit

spi_i2s_data_transmit :: Def ('[SPI_PERIPH, Uint16] :-> ())
spi_i2s_data_transmit = fun "spi_i2s_data_transmit"

getFlagSPII2S :: SPI_PERIPH -> SPI_I2S_FLAG -> Ivory eff IBool
getFlagSPII2S = call spi_i2s_flag_get

spi_i2s_flag_get :: Def ('[SPI_PERIPH, SPI_I2S_FLAG] :-> IBool)
spi_i2s_flag_get = fun "spi_i2s_flag_get"

initSPI :: SPI_PERIPH -> SPI_PARAM s -> Ivory eff ()
initSPI = call_ spi_init

spi_init :: Def ('[SPI_PERIPH, SPI_PARAM s] :-> ())
spi_init = fun "spi_init"

inclSPI :: ModuleDef
inclSPI = do
    incl spi_enable
    incl spi_i2s_deinit
    incl spi_i2s_data_transmit
    incl spi_i2s_flag_get
    incl spi_init
    incl spi_nss_output_enable
    incl spi_nssp_mode_enable

    inclSym spi0
    inclSym spi_flag_trans
    inclSym spi_slave
    inclSym spi_master
    inclSym spi_transmode_fullduplex
    inclSym spi_framesize_8bit
    inclSym spi_framesize_16bit
    inclSym spi_nss_hard
    inclSym spi_nss_soft
    inclSym spi_endian_msb
    inclSym spi_ck_pl_low_ph_1edge
    inclSym spi_ck_pl_high_ph_1edge
    inclSym spi_psc_2
    inclSym spi_psc_64