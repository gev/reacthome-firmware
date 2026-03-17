# ReactHome Embedded Framework

> The Pure Functional Approach to Hard Real-Time Systems

ReactHome is a high-integrity embedded framework built on Haskell and Ivory eDSL. It enables architects to design firmware as mathematical formulas, ensuring safety, determinism, and zero-copy performance for mission-critical applications like industrial automation, power grid control, and real-time audio processing.

## Philosophy: Correctness by Construction

In this framework, bugs are unrepresentable. By leveraging Haskell's type system and Ivory's safety guarantees, we eliminate 99% of common embedded pitfalls at compile time:

- **Memory Safety**: No null pointers, no buffer overflows, and no dynamic allocation
- **Concurrency Safety**: Race conditions are prevented by atomically blocks (IRQ masking) and strictly typed synchronization
- **Logic Integrity**: State machines (Mealy/Moore) are verified by types. If it compiles, transitions are valid
- **Zero-Risk Refactoring**: Refactor Haskell abstractions freely. If core logic remains unchanged, generated C code will be bit-for-bit identical

## Architecture Layers

### 1. Formula Layer (The Blueprint)

The entire device is described as a Formula. You don't "initialize" peripherals; you declare hardware topology. The system verifies that your pinouts, clock frequencies, and MCU capabilities match perfectly before generating any C code.

```haskell
rsHub4 :: Formula GD32F4xx
rsHub4 = Formula {
    name = "rs_hub4",
    model = deviceTypeRsHub4,
    version = (6, 2),
    shouldInit = true,
    mcu = gd32f450vgt6,
    quartzFrequency = 25_000_000,
    systemFrequency = 200_000_000,
    implementation = hub 
        (U.rbus eth_0) 
        (F.rbus $ rs485 uart_5 out_pb_14 :> rs485 uart_3 out_pc_12 :> Nil)
        (dimmers $ pwm_0 :> pwm_1 :> pwm_2 :> Nil)
        (dinputs $ in_pa_8 :> in_pb_4 :> in_pb_10 :> in_pa_15 :> Nil)
        (ds18b20 ow_0 od_pb_3)
        (indicator npx_pwm_0 50)
        (aled npx_pwm_1 etc)
}
```

### 2. Context Monoid (The Glue)

Our Context is a monoid. Different modules (UART, RBUS, Hub) can independently request initializations or tasks. The framework uses `nub` to collapse duplicates, ensuring that the final main and loop functions are clean, minimal, and idempotent.

```haskell
data Context = Context {
    getModule :: ModuleDef,
    getInits :: [Def ('[] :-> ())],
    getTasks :: [Task],
    getSyncs :: [Def ('[] :-> ())],
    getBody :: forall s. [(String, Ivory (ProcEffects s ()) ())]
}
```

### 3. FSM & Phase Scheduler

- **Hard Real-Time**: One tick equals one transition
- **Phase Multiplexing**: Use phase to stagger heavy tasks or build multi-step sensor polling (Reset → Phase 10ms → Measure → Phase 20ms → Read) without blocking the CPU
- **Multi-Body Functions**: Multiple independent modules can "stitch" their logic into a single interrupt handler or event hook without knowing about each other

```haskell
data Task = Task {
    period :: Maybe Period,
    getTask :: Def ('[] :-> ())
}

delayPhase interval phase id run = task (Just $ Period interval phase) id run
```

### 4. Lazy Transport (Zero-Copy Stream)

We use inversion of control for data transmission. Instead of copying data into buffers, we pass a generator function.

```haskell
class LazyTransport x where
    lazyTransmit :: x -> Uint8 -> ((Uint8 -> forall eff. Ivory eff ()) -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ()
```

- **RTP Ready**: Headers, IDs, and CRC16 are calculated and injected on-the-fly as the hardware requests the next byte
- **ElasticQueue**: Jitter buffers with built-in hysteresis for smooth audio streaming

## Build System & Portability

- **Shake + MD5**: Shake is a Haskell library used for build automation. Every build is isolated in a directory named after the MD5 hash of its compiler flags and definitions. No "stale" object files, ever.
- **GCC -O3 Ready**: Ivory generates "dead-simple" C code that C-optimizers love. It's safe to use -O3 because undefined behavior is caught in Haskell.
- **Architecture Independent**: Porting to RISC-V, Cortex-M, or a new vendor (GD32, STM32, ESP32) only requires swapping the low-level HAL. The business logic remains untouched.
- **Cabal Integration**: Use `cabal run` to generate C code, compile it, and link everything automatically.

## Protocol Stack

### RBUS Protocol
Master-slave communication with automatic device discovery and confirmation:

```haskell
data Preamble = Preamble {
    discovery :: Uint8,    -- 0xaa/0x55
    ping :: Uint8,         -- 0xcc/0x33  
    confirm :: Uint8,      -- 0xaf/0x5f
    message :: Uint8       -- 0xa0/0x50
}
```

### Multi-Mode Support
- **RBUS Mode**: Master-slave with addressing and confirmation
- **RS485 Mode**: Simple serial communication
- **DMX512 Mode**: Lighting control protocol with break generation

## Device Examples

### Hub Device (RsHub4)
Multi-port communication hub with dimmers, digital inputs, and temperature sensors:

```haskell
rsHub4 :: Formula GD32F4xx
rsHub4 = Formula { ... }  -- See full example above
```

### Digital Input Device (DI4)
Simple digital input module with temperature sensing:

```haskell
di4 :: Formula GD32F3x0
di4 = Formula {
    name = "di4",
    model = deviceTypeDi4,
    version = (4, 9),
    mcu = gd32f330k8u6,
    systemFrequency = 84_000_000,
    implementation = di
        (rbus $ rs485 uart_1 out_pa_4)
        (dinputs $ in_pa_12 :> in_pa_11 :> in_pa_10 :> in_pa_9 :> Nil)
        (ds18b20 ow_1 od_pa_8)
}
```

## Performance Characteristics

- **Zero-Copy**: All data transmission uses generator functions, eliminating buffer copies
- **Deterministic**: Fixed-time task scheduling with phase multiplexing
- **Memory Efficient**: No dynamic allocation, all memory allocated at compile time
- **Real-Time**: Hard real-time guarantees with IRQ-based synchronization

## Development Workflow

### 1. Define Your Device Formula
Create a formula file in `formula/Formula/YourDevice.hs`:

```haskell
{-# LANGUAGE NumericUnderscores #-}

module Formula.YourDevice where

import Core.Formula
import Device.YourMCU
import Feature.YourFeatures
import Implementation.YourImplementation

yourDevice :: Formula YourMCU
yourDevice = Formula {
    name = "your_device",
    model = deviceTypeYourDevice,
    version = (1, 0),
    mcu = yourMcu,
    systemFrequency = 168_000_000,
    implementation = yourImplementation
}
```

### 2. Build and Generate C Code
```bash
# Generate C code, compile it and link
cabal run

# The generated firmware is in firmware/ directory
# Each device has its own folder with .c and .h files
ls firmware/
```

### 3. Flash and Debug
```bash
# Flash using J-Link scripts (examples in script/ directory)
# For GD32F330 devices:
./script/flash_loc_330.sh

# For GD32F450 devices:
./script/flash_loc_450.sh

# Or use J-Link directly:
JLinkExe -Device GD32F330K8 -If SWD -Speed 1000 jlink/gd32f330/FlashMCU.jlink

# Debug with logic analyzer (not GDB!)
# Verify timing and protocol integrity at the pins
```

## Debugging Philosophy

We don't use GDB. We use oscilloscopes and logic analyzers.

Because memory and state logic are guaranteed by the type system, we only need to verify physical timing and protocol integrity at the pins.

## Type Safety Guarantees

### Memory Safety
- No null pointers (Ivory enforces non-null references)
- No buffer overflows (fixed-size arrays with bounds checking)
- No dynamic allocation (all memory allocated at compile time)

### Concurrency Safety  
- Race condition prevention through IRQ masking
- Strictly typed synchronization primitives
- Atomic operations enforced by type system

### Protocol Safety
- State machine transitions verified at compile time
- Message format validated by types
- CRC and checksum calculations guaranteed correct

## Technical Deep Dive

### Core Architectural Innovations

#### 1. Formula-Based Device Configuration
Instead of imperative initialization, devices are declared as mathematical formulas:

```haskell
rsHub4 :: Formula GD32F4xx
rsHub4 = Formula {
    name = "rs_hub4",
    model = deviceTypeRsHub4,
    version = (6, 2),
    mcu = gd32f450vgt6,
    quartzFrequency = 25_000_000,
    systemFrequency = 200_000_000,
    implementation = hub 
        (U.rbus eth_0) 
        (F.rbus $ rs485 uart_5 out_pb_14 :> rs485 uart_3 out_pc_12 :> Nil)
        (dimmers $ pwm_0 :> pwm_1 :> pwm_2 :> Nil)
        (dinputs $ in_pa_8 :> in_pb_4 :> in_pb_10 :> in_pa_15 :> Nil)
        (ds18b20 ow_0 od_pb_3)
        (indicator npx_pwm_0 50)
        (aled npx_pwm_1 etc)
}
```

**Benefits:**
- Hardware topology verification at compile-time
- Pin assignment conflicts detected early
- Clock frequency validation
- Zero configuration drift

#### 2. Context Monoid - Automatic Dependency Resolution
The framework uses monoidal composition to automatically resolve dependencies:

```haskell
data Context = Context {
    getModule :: ModuleDef,
    getInits :: [Def ('[] :-> ())],
    getTasks :: [Task],
    getSyncs :: [Def ('[] :-> ())],
    getBody :: forall s. [(String, Ivory (ProcEffects s ()) ())]
}

instance Semigroup Context where
    (Context m1 i1 t1 s1 b1) <> (Context m2 i2 t2 s2 b2) =
        Context (m1 <> m2) (nub $ i1 <> i2) (nub $ t1 <> t2) (nub $ s1 <> s2) (b1 <> b2)
```

**Key Features:**
- **Idempotent Initialization**: Duplicate init calls automatically collapsed via `nub`
- **Decentralized Design**: Modules independently request resources
- **Automatic Assembly**: Framework builds clean, minimal main/loop functions
- **Conflict Resolution**: No manual dependency management required

#### 3. Multi-Body Functions - Open-Ended ISR Composition
Multiple modules can stitch code into single interrupt handlers:

```haskell
-- In Build/Firmware.hs
bodyNames = nub $ fst <$> bodies
multiBodyFunctions = mkMultiBodyFunction <$> bodyNames

mkMultiBodyFunction :: String -> Def ('[] :-> ())
mkMultiBodyFunction name' = proc name' $ body $ mapM_ snd $ filter (\(id', _) -> id' == name') bodies
```

**Usage Example:**
```haskell
-- Module A
addBody "uart_irq" $ do
    -- Handle UART data for RBUS
    
-- Module B  
addBody "uart_irq" $ do
    -- Handle UART data for logging
    
-- Framework automatically generates:
uart_irq :: Def ('[] :-> ())
uart_irq = proc "uart_irq" $ body $ do
    -- Module A code
    -- Module B code
```

**Benefits:**
- True encapsulation - modules don't know about each other
- HAL-level abstraction isolates business logic from interrupt handling
- Guaranteed execution order based on context assembly
- No manual ISR management

#### 4. State Machine DSL - Mealy/Moore Automation
Framework provides type-safe state machine construction:

```haskell
-- Mealy Machine (output depends on state AND input)
runState :: (IvoryStore a, IvoryEq a) => 
    (t -> Ref s (Stored a)) -> [(a, t -> p -> Ivory eff ())] -> t -> p -> Ivory eff ()

-- Moore Machine (output depends only on state)
runState' :: (IvoryStore a, IvoryEq a) => 
    (t -> Ref s (Stored a)) -> [(a, t -> Ivory eff ())] -> t -> Ivory eff ()

-- Input dispatcher
runInput :: (IvoryEq p) => 
    [(p, t -> p -> Ivory eff ())] -> t -> p -> Ivory eff ()
```

**RBUS Protocol Example:**
```haskell
receive :: Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receive = runState state [
    readyToReceive |-> receivePreamble,
    receivingMessage |-> receiveMessage,
    receivingConfirm |-> receiveConfirm,
    receivingDiscovery |-> receiveDiscovery,
    receivingPing |-> receivePing,
    skippingAll |-> skipAll,
    skippingMsg |-> skipMsg
]
```

**Guarantees:**
- Invalid states are unrepresentable
- Deterministic transitions (one tick = one transition)
- Type-safe state and input handling
- Protocol correctness enforced by structure

#### 5. Lazy Transport - Zero-Copy Stream Processing
Revolutionary approach to data transmission using generator functions:

```haskell
class LazyTransport x where
    lazyTransmit :: x -> Uint8 -> 
        ((Uint8 -> forall eff. Ivory eff ()) -> forall eff. Ivory eff ()) -> 
        Ivory (ProcEffects s t) ()

-- RGB LED palette with on-the-fly color extraction
sendPalette LEDs{..} = do
    let n = 3 * ln' + 3
    lazyTransmit transport n \transmit -> do
        transmit actionRGB
        transmit . castDefault $ i' + 1
        transmit 1
        arrayMap \cx -> do
            value <- deref (colors ! ix' ! cx)
            let r' = castDefault $ (value `iShiftR` 16) .& 0xff
            let g' = castDefault $ (value `iShiftR` 8) .& 0xff
            let b' = castDefault $ value .& 0xff
            transmit r' >> transmit g' >> transmit b'
```

**Revolutionary Benefits:**
- **Zero Memory Allocation**: No intermediate buffers
- **On-the-Fly Processing**: CRC calculated during transmission
- **Real-Time Responsiveness**: Bytes generated as hardware requests them
- **Perfect for RTP**: Audio streaming with minimal jitter

#### 6. Universal Protocol Encoder
Single function works for both direct transmission and queuing:

```haskell
run :: (KnownNat l, SafeCast Uint8 v, IvoryStore v) =>
    Slave 255 ->
    (Slave 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ()) ->
    Buffer l v ->
    Uint16 ->
    Ivory (ProcEffects s t) Uint8
run protocol transmit buff offset = do
    size <- local $ ival 0
    let go :: Uint8 -> Ivory eff ()
        go v = do
            i <- deref size
            let ix = toIx $ offset + safeCast i
            store (buff ! ix) $ safeCast v
            store size $ i + 1
    transmit protocol go
    deref size
```

**Usage Scenarios:**
```haskell
-- Direct transmission
run protocol (transmitMessage' size' transmit) buff offset

-- Queue buffering  
run protocol (toQueue' size' transmit) buff offset
```

**Benefits:**
- Single protocol implementation for multiple transports
- Automatic size calculation
- Type-safe buffer management
- Compile-time bounds checking

### 🔄 Real-Time Task Scheduling

#### Phase-Based Multiplexing
Tasks can be staggered to avoid CPU contention:

```haskell
data Task = Task {
    period :: Maybe Period,
    getTask :: Def ('[] :-> ())
}

data Period = Period {
    interval :: Uint32,
    phase :: Uint32
}

delayPhase :: Uint32 -> Uint32 -> String -> Ivory (ProcEffects s ()) () -> Task
```

**Example: Multi-Step Sensor Polling**
```haskell
-- Phase 0: Reset sensor
addTask $ delayPhase 1000 0 "sensor_reset" resetSensor

-- Phase 10ms: Start measurement  
addTask $ delayPhase 1000 10 "sensor_measure" startMeasurement

-- Phase 20ms: Read result
addTask $ delayPhase 1000 20 "sensor_read" readResult
```

**Benefits:**
- Deterministic timing without blocking delays
- CPU load distribution across tick
- Complex sequencing made simple
- Hard real-time guarantees

### 🛡️ Type Safety Guarantees

#### Memory Safety
- **No NULL Pointers**: Ivory enforces non-null references
- **No Buffer Overflows**: Fixed-size arrays with bounds checking
- **No Dynamic Allocation**: All memory allocated at compile time
- **Stack Overflow Prevention**: Ivory guarantees bounded stack usage

#### Concurrency Safety  
- **IRQ Masking**: All shared state modifications protected by `atomically` blocks
- **Type-Safe Synchronization**: Semaphore and queue operations verified by types
- **Race Condition Prevention**: Shared state access patterns enforced by types

#### Protocol Safety
- **State Machine Verification**: Invalid transitions are type errors
- **CRC Enforcement**: Checksum calculation integrated into transmission
- **Message Structure**: Packet format guaranteed by function signatures
- **Protocol Compliance**: Invalid states unrepresentable

### 🎯 Industrial Applications

#### Power Grid Management
- **Generator Control**: Automatic transfer switches with phase monitoring
- **Load Balancing**: Real-time phase imbalance detection and correction
- **Safety Interlocks**: Type-enforced prevention of hazardous states
- **Emergency Response**: Deterministic fail-safe behavior

#### Climate Control Systems
- **Boiler Management**: Multi-stage heating with temperature feedback
- **Ventilation Control**: CO2 monitoring with adaptive fan control
- **Energy Optimization**: Learning algorithms with hard real-time constraints
- **Safety Systems**: Overheat protection and emergency shutdown

#### Audio Processing
- **RTP Streaming**: ElasticQueue with hysteresis for jitter buffering
- **Zero-Copy Processing**: Sample processing without memory allocation
- **Low Latency**: Sub-millisecond audio processing chains
- **Multi-Channel**: Synchronized multi-speaker systems

---

*"This framework transforms embedded development from writing instructions to designing mathematical systems. If it compiles, it works."*

## 🎯 Use Cases

- **Industrial Automation**: PLCs, motor controllers, sensor networks
- **Power Grid Control**: Protective relays, SCADA endpoints, smart meters  
- **Real-Time Audio**: Audio processing, RTP streaming, effects pedals
- **Lighting Control**: DMX512 controllers, LED drivers, architectural lighting
- **IoT Gateways**: Protocol converters, edge computing, data aggregation

## 📚 Project Structure

```
reacthome-firmware/
├── formula/           # Device formulas (blueprints)
│   └── Formula/       # Individual device definitions
├── src/              # Core framework
│   ├── Build/        # Build system and compilation
│   ├── Core/         # Framework abstractions
│   ├── Data/         # Data structures and utilities
│   ├── Device/       # Hardware abstraction layer
│   ├── Endpoint/     # Hardware endpoint implementations
│   ├── Feature/      # High-level feature implementations
│   ├── Implementation/ # Device implementation logic
│   ├── Interface/    # Hardware interface definitions
│   ├── Ivory/        # Ivory eDSL support
│   ├── Protocol/     # Communication protocol implementations
│   ├── Support/      # Support libraries and utilities
│   ├── Transport/    # Transport layer implementations
│   └── Util/         # General utilities
├── firmware/         # Generated C code and headers
├── script/           # J-Link flash scripts
├── jlink/            # J-Link configuration files
├── lab/              # C experiments to learn hardware
└── support/          # Third-party C dependencies
```

## 🚀 Getting Started

1. **Prerequisites**: GHC, ARM GCC toolchain, J-Link
2. **Clone**: `git clone <repository>`
3. **Build**: `cabal build`
4. **Generate Firmware**: `cabal run` to generate optimized C code and compile firmware
5. **Flash**: Use J-Link scripts in `script/` directory
6. **Debug**: Use logic analyzer to verify timing and protocol integrity

## 📄 License

[License information here]

---

*Built with ❤️ for mission-critical embedded systems*
