//! Operations available in Embeem.
//!
//! Operations in Embeem are represented as paths of UPPER_SNAKE_CASE segments.
//! For example, `WRITE(GPIO(pin), value)` is represented as `path: ["WRITE", "GPIO"]`
//! with `args: [pin, value]`.
//!
//! This module provides:
//! - Functions to validate identifier naming conventions
//! - `OpAction` enum for operation actions (READ, WRITE, INIT, etc.)
//! - `OpTarget` enum for operation targets (GPIO, ADC, UART, etc.)
//! - `OpKind` enum combining action and target for path-based operations
//! - Lookup functions for operation metadata

/// Check if an identifier follows UPPER_SNAKE_CASE pattern.
///
/// UPPER_SNAKE_CASE identifiers are reserved for operations and cannot be used
/// as user-defined names (variables, functions, constants, etc.).
///
/// Pattern: starts with [A-Z], contains only [A-Z0-9_]
///
/// # Examples
/// ```
/// use embeem_ast::is_upper_snake_case;
///
/// assert!(is_upper_snake_case("GPIO_READ"));
/// assert!(is_upper_snake_case("ADC_SET_RESOLUTION"));
/// assert!(is_upper_snake_case("NOP"));
/// assert!(is_upper_snake_case("I2C_INIT"));
///
/// assert!(!is_upper_snake_case("gpio_read"));
/// assert!(!is_upper_snake_case("GpioRead"));
/// assert!(!is_upper_snake_case("myVariable"));
/// assert!(!is_upper_snake_case("_PRIVATE"));
/// ```
pub fn is_upper_snake_case(s: &str) -> bool {
    let mut chars = s.chars();

    // Must start with uppercase letter
    match chars.next() {
        Some(c) if c.is_ascii_uppercase() => {}
        _ => return false,
    }

    // Rest must be uppercase letters, digits, or underscores
    for c in chars {
        if !c.is_ascii_uppercase() && !c.is_ascii_digit() && c != '_' {
            return false;
        }
    }
    true
}

/// Check if an identifier is valid for user-defined names.
///
/// User identifiers must NOT be UPPER_SNAKE_CASE (reserved for operations)
/// and must not be a keyword.
///
/// # Examples
/// ```
/// use embeem_ast::is_valid_user_identifier;
///
/// assert!(is_valid_user_identifier("x"));
/// assert!(is_valid_user_identifier("myVariable"));
/// assert!(is_valid_user_identifier("my_variable"));
/// assert!(is_valid_user_identifier("_private"));
/// assert!(is_valid_user_identifier("MyType"));
///
/// assert!(!is_valid_user_identifier("GPIO_READ"));  // Reserved for operations
/// assert!(!is_valid_user_identifier("MY_CONST"));   // UPPER_SNAKE_CASE
/// ```
pub fn is_valid_user_identifier(s: &str) -> bool {
    // Must not be UPPER_SNAKE_CASE (reserved for operations)
    !is_upper_snake_case(s)
}

/// Get the operation segment from its string representation.
///
/// Each segment represents a single element of an operation path.
/// For example, `READ(GPIO(pin))` has path `["READ", "GPIO"]` where
/// each string maps to an `OpKind`.
pub fn op_kind_from_str(name: &str) -> Option<OpKind> {
    match name {
        // Actions (verbs - typically first in path)
        "READ" => Some(OpKind::Read),
        "WRITE" => Some(OpKind::Write),
        "INIT" => Some(OpKind::Init),
        "START" => Some(OpKind::Start),
        "STOP" => Some(OpKind::Stop),
        "RESET" => Some(OpKind::Reset),
        "TOGGLE" => Some(OpKind::Toggle),
        "ENABLE" => Some(OpKind::Enable),
        "DISABLE" => Some(OpKind::Disable),
        "TRANSFER" => Some(OpKind::Transfer),
        "SEND" => Some(OpKind::Send),
        "RECEIVE" => Some(OpKind::Receive),
        "SCAN" => Some(OpKind::Scan),
        "CONNECT" => Some(OpKind::Connect),
        "DISCONNECT" => Some(OpKind::Disconnect),
        "FLUSH" => Some(OpKind::Flush),
        "AVAILABLE" => Some(OpKind::Available),
        "ERASE" => Some(OpKind::Erase),
        "UPDATE" => Some(OpKind::Update),
        "CONFIG" => Some(OpKind::Config),
        "ENTER" => Some(OpKind::Enter),
        "GET" => Some(OpKind::Get),
        "SET" => Some(OpKind::Set),
        "BEGIN" => Some(OpKind::Begin),
        "END" => Some(OpKind::End),
        "FIND" => Some(OpKind::Find),
        "COUNT" => Some(OpKind::Count),
        "CLEAR" => Some(OpKind::Clear),
        "TEST" => Some(OpKind::Test),
        "DELAY" => Some(OpKind::Delay),

        // Targets (nouns - typically second in path)
        "GPIO" => Some(OpKind::Gpio),
        "PORT" => Some(OpKind::Port),
        "ADC" => Some(OpKind::Adc),
        "DAC" => Some(OpKind::Dac),
        "PWM" => Some(OpKind::Pwm),
        "TIMER" => Some(OpKind::Timer),
        "UART" => Some(OpKind::Uart),
        "SPI" => Some(OpKind::Spi),
        "I2C" => Some(OpKind::I2c),
        "CAN" => Some(OpKind::Can),
        "USB" => Some(OpKind::Usb),
        "DMA" => Some(OpKind::Dma),
        "WDT" => Some(OpKind::Wdt),
        "EEPROM" => Some(OpKind::Eeprom),
        "FLASH" => Some(OpKind::Flash),
        "RTC" => Some(OpKind::Rtc),
        "PERIPHERAL" => Some(OpKind::Peripheral),

        // Modifiers/Properties (typically third+ in path)
        "MODE" => Some(OpKind::Mode),
        "FREQUENCY" => Some(OpKind::Frequency),
        "DUTY_CYCLE" => Some(OpKind::DutyCycle),
        "PULSE_WIDTH" => Some(OpKind::PulseWidth),
        "PERIOD" => Some(OpKind::Period),
        "COMPARE" => Some(OpKind::Compare),
        "BAUD_RATE" => Some(OpKind::BaudRate),
        "CLOCK" => Some(OpKind::Clock),
        "CLOCK_SPEED" => Some(OpKind::ClockSpeed),
        "BIT_ORDER" => Some(OpKind::BitOrder),
        "FILTER" => Some(OpKind::Filter),
        "BITRATE" => Some(OpKind::Bitrate),
        "TIMEOUT" => Some(OpKind::Timeout),
        "SOURCE" => Some(OpKind::Source),
        "DESTINATION" => Some(OpKind::Destination),
        "RESOLUTION" => Some(OpKind::Resolution),
        "REFERENCE" => Some(OpKind::Reference),
        "POWER_MODE" => Some(OpKind::PowerMode),
        "TIME" => Some(OpKind::Time),
        "ALARM" => Some(OpKind::Alarm),
        "CALENDAR" => Some(OpKind::Calendar),
        "MILLIS" => Some(OpKind::Millis),
        "MICROS" => Some(OpKind::Micros),
        "MS" => Some(OpKind::Ms),
        "US" => Some(OpKind::Us),
        "BYTE" => Some(OpKind::Byte),
        "BUFFER" => Some(OpKind::Buffer),
        "MULTI" => Some(OpKind::Multi),
        "CONVERSION" => Some(OpKind::Conversion),
        "TRANSACTION" => Some(OpKind::Transaction),
        "TO" => Some(OpKind::To),
        "FROM" => Some(OpKind::From),
        "BIT" => Some(OpKind::Bit),
        "ONES" => Some(OpKind::Ones),
        "ZEROS" => Some(OpKind::Zeros),
        "FIRST" => Some(OpKind::First),
        "STANDBY" => Some(OpKind::Standby),
        "DEEP_SLEEP" => Some(OpKind::DeepSleep),

        // Arithmetic operations (single-segment operations)
        "ADD" => Some(OpKind::Add),
        "SUB" => Some(OpKind::Sub),
        "MUL" => Some(OpKind::Mul),
        "DIV" => Some(OpKind::Div),
        "MOD" => Some(OpKind::Mod),
        "INC" => Some(OpKind::Inc),
        "DEC" => Some(OpKind::Dec),
        "NEG" => Some(OpKind::Neg),
        "ABS" => Some(OpKind::Abs),

        // Logical/Bitwise operations (single-segment)
        "AND" => Some(OpKind::And),
        "OR" => Some(OpKind::Or),
        "XOR" => Some(OpKind::Xor),
        "NOT" => Some(OpKind::Not),
        "SHL" => Some(OpKind::Shl),
        "SHR" => Some(OpKind::Shr),
        "SAR" => Some(OpKind::Sar),
        "ROL" => Some(OpKind::Rol),
        "ROR" => Some(OpKind::Ror),

        // Comparison operations (single-segment)
        "CMP" => Some(OpKind::Cmp),
        "EQ" => Some(OpKind::Eq),
        "NE" => Some(OpKind::Ne),
        "LT" => Some(OpKind::Lt),
        "LE" => Some(OpKind::Le),
        "GT" => Some(OpKind::Gt),
        "GE" => Some(OpKind::Ge),

        // Control flow (single-segment)
        "NOP" => Some(OpKind::Nop),
        "HALT" => Some(OpKind::Halt),
        "SLEEP" => Some(OpKind::Sleep),

        // Floating point prefix
        "FADD" => Some(OpKind::FAdd),
        "FSUB" => Some(OpKind::FSub),
        "FMUL" => Some(OpKind::FMul),
        "FDIV" => Some(OpKind::FDiv),
        "FSQRT" => Some(OpKind::FSqrt),
        "FABS" => Some(OpKind::FAbs),
        "FCMP" => Some(OpKind::FCmp),

        _ => None,
    }
}

/// Operation path segment kinds.
///
/// Each variant represents a single segment of an operation path.
/// Operations are built by combining segments into paths, e.g.:
/// - `["READ", "GPIO"]` for `READ(GPIO(pin))`
/// - `["SET", "DUTY_CYCLE", "PWM"]` for `SET(DUTY_CYCLE(PWM(ch)), val)`
/// - `["ADD"]` for `ADD(a, b)` (single-segment arithmetic)
///
/// Segments are categorized as:
/// - **Actions**: Verbs like READ, WRITE, INIT (typically first in path)
/// - **Targets**: Peripherals like GPIO, ADC, UART (typically second)
/// - **Modifiers**: Properties like MODE, FREQUENCY (typically third+)
/// - **Primitives**: Single-segment operations like ADD, SUB
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpKind {
    // ===== Actions (verbs) =====
    /// Read operation
    Read,
    /// Write operation
    Write,
    /// Initialize
    Init,
    /// Start
    Start,
    /// Stop
    Stop,
    /// Reset
    Reset,
    /// Toggle state
    Toggle,
    /// Enable
    Enable,
    /// Disable
    Disable,
    /// Transfer data
    Transfer,
    /// Send data
    Send,
    /// Receive data
    Receive,
    /// Scan for devices
    Scan,
    /// Connect
    Connect,
    /// Disconnect
    Disconnect,
    /// Flush buffers
    Flush,
    /// Check availability
    Available,
    /// Erase
    Erase,
    /// Update
    Update,
    /// Configure
    Config,
    /// Enter a state/mode
    Enter,
    /// Get a value
    Get,
    /// Set a value
    Set,
    /// Begin transaction/operation
    Begin,
    /// End transaction/operation
    End,
    /// Find something
    Find,
    /// Count something
    Count,
    /// Clear something
    Clear,
    /// Test something
    Test,
    /// Delay execution
    Delay,

    // ===== Targets (peripherals/nouns) =====
    /// GPIO pin
    Gpio,
    /// GPIO port
    Port,
    /// Analog-to-digital converter
    Adc,
    /// Digital-to-analog converter
    Dac,
    /// Pulse-width modulation
    Pwm,
    /// Hardware timer
    Timer,
    /// UART serial
    Uart,
    /// SPI bus
    Spi,
    /// I2C bus
    I2c,
    /// CAN bus
    Can,
    /// USB
    Usb,
    /// DMA controller
    Dma,
    /// Watchdog timer
    Wdt,
    /// EEPROM memory
    Eeprom,
    /// Flash memory
    Flash,
    /// Real-time clock
    Rtc,
    /// Generic peripheral
    Peripheral,

    // ===== Modifiers/Properties =====
    /// Operating mode
    Mode,
    /// Frequency setting
    Frequency,
    /// PWM duty cycle
    DutyCycle,
    /// Pulse width
    PulseWidth,
    /// Timer period
    Period,
    /// Compare value
    Compare,
    /// UART baud rate
    BaudRate,
    /// Clock signal
    Clock,
    /// Clock speed
    ClockSpeed,
    /// Bit order (MSB/LSB first)
    BitOrder,
    /// CAN filter
    Filter,
    /// CAN bitrate
    Bitrate,
    /// Timeout value
    Timeout,
    /// DMA source
    Source,
    /// DMA destination
    Destination,
    /// ADC resolution
    Resolution,
    /// ADC reference voltage
    Reference,
    /// Power mode
    PowerMode,
    /// Time value
    Time,
    /// Alarm setting
    Alarm,
    /// Calendar setting
    Calendar,
    /// Milliseconds
    Millis,
    /// Microseconds
    Micros,
    /// Milliseconds (short form)
    Ms,
    /// Microseconds (short form)
    Us,
    /// Single byte
    Byte,
    /// Data buffer
    Buffer,
    /// Multiple channels
    Multi,
    /// ADC conversion
    Conversion,
    /// SPI transaction
    Transaction,
    /// Direction: to
    To,
    /// Direction: from
    From,
    /// Single bit
    Bit,
    /// Count of ones
    Ones,
    /// Count of zeros
    Zeros,
    /// First occurrence
    First,
    /// Standby mode
    Standby,
    /// Deep sleep mode
    DeepSleep,

    // ===== Arithmetic (single-segment) =====
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Modulo
    Mod,
    /// Increment
    Inc,
    /// Decrement
    Dec,
    /// Negate
    Neg,
    /// Absolute value
    Abs,

    // ===== Logical/Bitwise (single-segment) =====
    /// Bitwise AND
    And,
    /// Bitwise OR
    Or,
    /// Bitwise XOR
    Xor,
    /// Bitwise NOT
    Not,
    /// Shift left
    Shl,
    /// Shift right (logical)
    Shr,
    /// Shift arithmetic right
    Sar,
    /// Rotate left
    Rol,
    /// Rotate right
    Ror,

    // ===== Comparison (single-segment) =====
    /// Compare
    Cmp,
    /// Equal
    Eq,
    /// Not equal
    Ne,
    /// Less than
    Lt,
    /// Less than or equal
    Le,
    /// Greater than
    Gt,
    /// Greater than or equal
    Ge,

    // ===== Control Flow (single-segment) =====
    /// No operation
    Nop,
    /// Halt execution
    Halt,
    /// Sleep/low power
    Sleep,

    // ===== Floating Point (single-segment) =====
    /// Floating-point addition
    FAdd,
    /// Floating-point subtraction
    FSub,
    /// Floating-point multiplication
    FMul,
    /// Floating-point division
    FDiv,
    /// Floating-point square root
    FSqrt,
    /// Floating-point absolute value
    FAbs,
    /// Floating-point compare
    FCmp,
}

impl OpKind {
    /// Get the string representation of this operation segment.
    pub fn as_str(self) -> &'static str {
        match self {
            // Actions
            OpKind::Read => "READ",
            OpKind::Write => "WRITE",
            OpKind::Init => "INIT",
            OpKind::Start => "START",
            OpKind::Stop => "STOP",
            OpKind::Reset => "RESET",
            OpKind::Toggle => "TOGGLE",
            OpKind::Enable => "ENABLE",
            OpKind::Disable => "DISABLE",
            OpKind::Transfer => "TRANSFER",
            OpKind::Send => "SEND",
            OpKind::Receive => "RECEIVE",
            OpKind::Scan => "SCAN",
            OpKind::Connect => "CONNECT",
            OpKind::Disconnect => "DISCONNECT",
            OpKind::Flush => "FLUSH",
            OpKind::Available => "AVAILABLE",
            OpKind::Erase => "ERASE",
            OpKind::Update => "UPDATE",
            OpKind::Config => "CONFIG",
            OpKind::Enter => "ENTER",
            OpKind::Get => "GET",
            OpKind::Set => "SET",
            OpKind::Begin => "BEGIN",
            OpKind::End => "END",
            OpKind::Find => "FIND",
            OpKind::Count => "COUNT",
            OpKind::Clear => "CLEAR",
            OpKind::Test => "TEST",
            OpKind::Delay => "DELAY",

            // Targets
            OpKind::Gpio => "GPIO",
            OpKind::Port => "PORT",
            OpKind::Adc => "ADC",
            OpKind::Dac => "DAC",
            OpKind::Pwm => "PWM",
            OpKind::Timer => "TIMER",
            OpKind::Uart => "UART",
            OpKind::Spi => "SPI",
            OpKind::I2c => "I2C",
            OpKind::Can => "CAN",
            OpKind::Usb => "USB",
            OpKind::Dma => "DMA",
            OpKind::Wdt => "WDT",
            OpKind::Eeprom => "EEPROM",
            OpKind::Flash => "FLASH",
            OpKind::Rtc => "RTC",
            OpKind::Peripheral => "PERIPHERAL",

            // Modifiers
            OpKind::Mode => "MODE",
            OpKind::Frequency => "FREQUENCY",
            OpKind::DutyCycle => "DUTY_CYCLE",
            OpKind::PulseWidth => "PULSE_WIDTH",
            OpKind::Period => "PERIOD",
            OpKind::Compare => "COMPARE",
            OpKind::BaudRate => "BAUD_RATE",
            OpKind::Clock => "CLOCK",
            OpKind::ClockSpeed => "CLOCK_SPEED",
            OpKind::BitOrder => "BIT_ORDER",
            OpKind::Filter => "FILTER",
            OpKind::Bitrate => "BITRATE",
            OpKind::Timeout => "TIMEOUT",
            OpKind::Source => "SOURCE",
            OpKind::Destination => "DESTINATION",
            OpKind::Resolution => "RESOLUTION",
            OpKind::Reference => "REFERENCE",
            OpKind::PowerMode => "POWER_MODE",
            OpKind::Time => "TIME",
            OpKind::Alarm => "ALARM",
            OpKind::Calendar => "CALENDAR",
            OpKind::Millis => "MILLIS",
            OpKind::Micros => "MICROS",
            OpKind::Ms => "MS",
            OpKind::Us => "US",
            OpKind::Byte => "BYTE",
            OpKind::Buffer => "BUFFER",
            OpKind::Multi => "MULTI",
            OpKind::Conversion => "CONVERSION",
            OpKind::Transaction => "TRANSACTION",
            OpKind::To => "TO",
            OpKind::From => "FROM",
            OpKind::Bit => "BIT",
            OpKind::Ones => "ONES",
            OpKind::Zeros => "ZEROS",
            OpKind::First => "FIRST",
            OpKind::Standby => "STANDBY",
            OpKind::DeepSleep => "DEEP_SLEEP",

            // Arithmetic
            OpKind::Add => "ADD",
            OpKind::Sub => "SUB",
            OpKind::Mul => "MUL",
            OpKind::Div => "DIV",
            OpKind::Mod => "MOD",
            OpKind::Inc => "INC",
            OpKind::Dec => "DEC",
            OpKind::Neg => "NEG",
            OpKind::Abs => "ABS",

            // Logical/Bitwise
            OpKind::And => "AND",
            OpKind::Or => "OR",
            OpKind::Xor => "XOR",
            OpKind::Not => "NOT",
            OpKind::Shl => "SHL",
            OpKind::Shr => "SHR",
            OpKind::Sar => "SAR",
            OpKind::Rol => "ROL",
            OpKind::Ror => "ROR",

            // Comparison
            OpKind::Cmp => "CMP",
            OpKind::Eq => "EQ",
            OpKind::Ne => "NE",
            OpKind::Lt => "LT",
            OpKind::Le => "LE",
            OpKind::Gt => "GT",
            OpKind::Ge => "GE",

            // Control flow
            OpKind::Nop => "NOP",
            OpKind::Halt => "HALT",
            OpKind::Sleep => "SLEEP",

            // Floating point
            OpKind::FAdd => "FADD",
            OpKind::FSub => "FSUB",
            OpKind::FMul => "FMUL",
            OpKind::FDiv => "FDIV",
            OpKind::FSqrt => "FSQRT",
            OpKind::FAbs => "FABS",
            OpKind::FCmp => "FCMP",
        }
    }

    /// Check if this is an action (verb) segment.
    pub fn is_action(self) -> bool {
        matches!(
            self,
            OpKind::Read
                | OpKind::Write
                | OpKind::Init
                | OpKind::Start
                | OpKind::Stop
                | OpKind::Reset
                | OpKind::Toggle
                | OpKind::Enable
                | OpKind::Disable
                | OpKind::Transfer
                | OpKind::Send
                | OpKind::Receive
                | OpKind::Scan
                | OpKind::Connect
                | OpKind::Disconnect
                | OpKind::Flush
                | OpKind::Available
                | OpKind::Erase
                | OpKind::Update
                | OpKind::Config
                | OpKind::Enter
                | OpKind::Get
                | OpKind::Set
                | OpKind::Begin
                | OpKind::End
                | OpKind::Find
                | OpKind::Count
                | OpKind::Clear
                | OpKind::Test
                | OpKind::Delay
        )
    }

    /// Check if this is a target (peripheral) segment.
    pub fn is_target(self) -> bool {
        matches!(
            self,
            OpKind::Gpio
                | OpKind::Port
                | OpKind::Adc
                | OpKind::Dac
                | OpKind::Pwm
                | OpKind::Timer
                | OpKind::Uart
                | OpKind::Spi
                | OpKind::I2c
                | OpKind::Can
                | OpKind::Usb
                | OpKind::Dma
                | OpKind::Wdt
                | OpKind::Eeprom
                | OpKind::Flash
                | OpKind::Rtc
                | OpKind::Peripheral
        )
    }

    /// Check if this is a primitive single-segment operation.
    pub fn is_primitive(self) -> bool {
        matches!(
            self,
            // Arithmetic
            OpKind::Add
                | OpKind::Sub
                | OpKind::Mul
                | OpKind::Div
                | OpKind::Mod
                | OpKind::Inc
                | OpKind::Dec
                | OpKind::Neg
                | OpKind::Abs
                // Logical/Bitwise
                | OpKind::And
                | OpKind::Or
                | OpKind::Xor
                | OpKind::Not
                | OpKind::Shl
                | OpKind::Shr
                | OpKind::Sar
                | OpKind::Rol
                | OpKind::Ror
                // Comparison
                | OpKind::Cmp
                | OpKind::Eq
                | OpKind::Ne
                | OpKind::Lt
                | OpKind::Le
                | OpKind::Gt
                | OpKind::Ge
                // Control flow
                | OpKind::Nop
                | OpKind::Halt
                | OpKind::Sleep
                // Floating point
                | OpKind::FAdd
                | OpKind::FSub
                | OpKind::FMul
                | OpKind::FDiv
                | OpKind::FSqrt
                | OpKind::FAbs
                | OpKind::FCmp
        )
    }

    /// Get the arity of this operation segment.
    ///
    /// The arity represents how many arguments this segment consumes:
    /// - For primitive operations (ADD, SUB, etc.), this is the total argument count
    /// - For actions (READ, WRITE, etc.), this is the number of *extra* arguments
    ///   beyond the nested target (0 for READ, 1 for WRITE, etc.)
    /// - For targets (GPIO, ADC, etc.), this is typically 1 (the channel/pin)
    /// - For modifiers, this varies based on the property
    ///
    /// # Examples
    /// - `READ(GPIO(pin))`: READ has arity 0 (no extra args), GPIO has arity 1
    /// - `WRITE(GPIO(pin), value)`: WRITE has arity 1 (the value), GPIO has arity 1
    /// - `ADD(a, b)`: ADD has arity 2 (both operands)
    pub fn arity(self) -> usize {
        match self {
            // ===== Actions =====
            // Actions that only take a target, no extra args
            OpKind::Read
            | OpKind::Init
            | OpKind::Start
            | OpKind::Stop
            | OpKind::Reset
            | OpKind::Toggle
            | OpKind::Enable
            | OpKind::Disable
            | OpKind::Receive
            | OpKind::Scan
            | OpKind::Connect
            | OpKind::Disconnect
            | OpKind::Flush
            | OpKind::Available
            | OpKind::Erase
            | OpKind::Begin
            | OpKind::End
            | OpKind::Get
            | OpKind::Find
            | OpKind::Count
            | OpKind::Clear => 0,

            // Actions that take a target + 1 extra arg (value, data, etc.)
            OpKind::Write
            | OpKind::Transfer
            | OpKind::Send
            | OpKind::Update
            | OpKind::Config
            | OpKind::Enter
            | OpKind::Set
            | OpKind::Test
            | OpKind::Delay => 1,

            // ===== Targets (peripherals) =====
            // Most targets take 1 arg (pin, channel, bus number, etc.)
            OpKind::Gpio
            | OpKind::Port
            | OpKind::Adc
            | OpKind::Dac
            | OpKind::Pwm
            | OpKind::Timer
            | OpKind::Uart
            | OpKind::Spi
            | OpKind::I2c
            | OpKind::Can
            | OpKind::Usb
            | OpKind::Dma
            | OpKind::Eeprom
            | OpKind::Flash
            | OpKind::Rtc
            | OpKind::Peripheral => 1,

            // Watchdog typically has no channel argument
            OpKind::Wdt => 0,

            // ===== Modifiers/Properties =====
            // Properties that are just qualifiers (no extra args)
            OpKind::Mode
            | OpKind::Frequency
            | OpKind::DutyCycle
            | OpKind::PulseWidth
            | OpKind::Period
            | OpKind::Compare
            | OpKind::BaudRate
            | OpKind::Clock
            | OpKind::ClockSpeed
            | OpKind::BitOrder
            | OpKind::Filter
            | OpKind::Bitrate
            | OpKind::Timeout
            | OpKind::Source
            | OpKind::Destination
            | OpKind::Resolution
            | OpKind::Reference
            | OpKind::PowerMode
            | OpKind::Time
            | OpKind::Alarm
            | OpKind::Calendar
            | OpKind::Millis
            | OpKind::Micros
            | OpKind::Ms
            | OpKind::Us
            | OpKind::Byte
            | OpKind::Buffer
            | OpKind::Multi
            | OpKind::Conversion
            | OpKind::Transaction
            | OpKind::Bit
            | OpKind::Ones
            | OpKind::Zeros
            | OpKind::First
            | OpKind::Standby
            | OpKind::DeepSleep => 0,

            // Direction modifiers that take an address/target
            OpKind::To | OpKind::From => 1,

            // ===== Primitives (single-segment operations) =====
            // Nullary
            OpKind::Nop | OpKind::Halt | OpKind::Sleep => 0,

            // Unary
            OpKind::Inc
            | OpKind::Dec
            | OpKind::Neg
            | OpKind::Abs
            | OpKind::Not
            | OpKind::FSqrt
            | OpKind::FAbs => 1,

            // Binary
            OpKind::Add
            | OpKind::Sub
            | OpKind::Mul
            | OpKind::Div
            | OpKind::Mod
            | OpKind::And
            | OpKind::Or
            | OpKind::Xor
            | OpKind::Shl
            | OpKind::Shr
            | OpKind::Sar
            | OpKind::Rol
            | OpKind::Ror
            | OpKind::Cmp
            | OpKind::Eq
            | OpKind::Ne
            | OpKind::Lt
            | OpKind::Le
            | OpKind::Gt
            | OpKind::Ge
            | OpKind::FAdd
            | OpKind::FSub
            | OpKind::FMul
            | OpKind::FDiv
            | OpKind::FCmp => 2,
        }
    }
}