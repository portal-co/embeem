//! Operations available in Embeem.
//!
//! Operations in Embeem are represented as paths of UPPER_SNAKE_CASE segments.
//! For example, `WRITE(GPIO(pin), value)` is represented as `path: ["WRITE", "GPIO"]`
//! with `args: [pin, value]`.
//!
//! This module provides:
//! - Functions to validate identifier naming conventions
//! - `OpKind` enum for known built-in operations with their arities
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

/// Get the operation name from its string representation.
pub fn op_name_from_str(name: &str) -> Option<OpKind> {
    match name {
        "ADD" => Some(OpKind::Add),
        "SUB" => Some(OpKind::Sub),
        "MUL" => Some(OpKind::Mul),
        "DIV" => Some(OpKind::Div),
        "MOD" => Some(OpKind::Mod),
        "INC" => Some(OpKind::Inc),
        "DEC" => Some(OpKind::Dec),
        "NEG" => Some(OpKind::Neg),
        "ABS" => Some(OpKind::Abs),
        "AND" => Some(OpKind::And),
        "OR" => Some(OpKind::Or),
        "XOR" => Some(OpKind::Xor),
        "NOT" => Some(OpKind::Not),
        "SHL" => Some(OpKind::Shl),
        "SHR" => Some(OpKind::Shr),
        "SAR" => Some(OpKind::Sar),
        "ROL" => Some(OpKind::Rol),
        "ROR" => Some(OpKind::Ror),
        "CMP" => Some(OpKind::Cmp),
        "TEST" => Some(OpKind::Test),
        "EQ" => Some(OpKind::Eq),
        "NE" => Some(OpKind::Ne),
        "LT" => Some(OpKind::Lt),
        "LE" => Some(OpKind::Le),
        "GT" => Some(OpKind::Gt),
        "GE" => Some(OpKind::Ge),
        "NOP" => Some(OpKind::Nop),
        "HALT" => Some(OpKind::Halt),
        "SLEEP" => Some(OpKind::Sleep),
        "SET_BIT" => Some(OpKind::SetBit),
        "CLEAR_BIT" => Some(OpKind::ClearBit),
        "TOGGLE_BIT" => Some(OpKind::ToggleBit),
        "TEST_BIT" => Some(OpKind::TestBit),
        "COUNT_ONES" => Some(OpKind::CountOnes),
        "COUNT_ZEROS" => Some(OpKind::CountZeros),
        "FIND_FIRST_SET" => Some(OpKind::FindFirstSet),
        "FIND_FIRST_ZERO" => Some(OpKind::FindFirstZero),
        "FADD" => Some(OpKind::FAdd),
        "FSUB" => Some(OpKind::FSub),
        "FMUL" => Some(OpKind::FMul),
        "FDIV" => Some(OpKind::FDiv),
        "FSQRT" => Some(OpKind::FSqrt),
        "FABS" => Some(OpKind::FAbs),
        "FCMP" => Some(OpKind::FCmp),
        "GPIO_READ" => Some(OpKind::GpioRead),
        "GPIO_WRITE" => Some(OpKind::GpioWrite),
        "GPIO_TOGGLE" => Some(OpKind::GpioToggle),
        "GPIO_SET_MODE" => Some(OpKind::GpioSetMode),
        "GPIO_READ_PORT" => Some(OpKind::GpioReadPort),
        "GPIO_WRITE_PORT" => Some(OpKind::GpioWritePort),
        "ADC_READ" => Some(OpKind::AdcRead),
        "ADC_START_CONVERSION" => Some(OpKind::AdcStartConversion),
        "ADC_READ_MULTI" => Some(OpKind::AdcReadMulti),
        "DAC_WRITE" => Some(OpKind::DacWrite),
        "ADC_SET_RESOLUTION" => Some(OpKind::AdcSetResolution),
        "ADC_SET_REFERENCE" => Some(OpKind::AdcSetReference),
        "PWM_START" => Some(OpKind::PwmStart),
        "PWM_STOP" => Some(OpKind::PwmStop),
        "PWM_SET_DUTY_CYCLE" => Some(OpKind::PwmSetDutyCycle),
        "PWM_SET_FREQUENCY" => Some(OpKind::PwmSetFrequency),
        "PWM_SET_PULSE_WIDTH" => Some(OpKind::PwmSetPulseWidth),
        "TIMER_START" => Some(OpKind::TimerStart),
        "TIMER_STOP" => Some(OpKind::TimerStop),
        "TIMER_RESET" => Some(OpKind::TimerReset),
        "TIMER_READ" => Some(OpKind::TimerRead),
        "TIMER_SET_PERIOD" => Some(OpKind::TimerSetPeriod),
        "TIMER_SET_COMPARE" => Some(OpKind::TimerSetCompare),
        "GET_MILLIS" => Some(OpKind::GetMillis),
        "GET_MICROS" => Some(OpKind::GetMicros),
        "DELAY_MS" => Some(OpKind::DelayMs),
        "DELAY_US" => Some(OpKind::DelayUs),
        "UART_INIT" => Some(OpKind::UartInit),
        "UART_WRITE_BYTE" => Some(OpKind::UartWriteByte),
        "UART_WRITE_BUFFER" => Some(OpKind::UartWriteBuffer),
        "UART_READ_BYTE" => Some(OpKind::UartReadByte),
        "UART_READ_BUFFER" => Some(OpKind::UartReadBuffer),
        "UART_AVAILABLE" => Some(OpKind::UartAvailable),
        "UART_FLUSH" => Some(OpKind::UartFlush),
        "UART_SET_BAUD_RATE" => Some(OpKind::UartSetBaudRate),
        "SPI_INIT" => Some(OpKind::SpiInit),
        "SPI_TRANSFER" => Some(OpKind::SpiTransfer),
        "SPI_TRANSFER_BUFFER" => Some(OpKind::SpiTransferBuffer),
        "SPI_SET_MODE" => Some(OpKind::SpiSetMode),
        "SPI_SET_CLOCK" => Some(OpKind::SpiSetClock),
        "SPI_SET_BIT_ORDER" => Some(OpKind::SpiSetBitOrder),
        "SPI_BEGIN_TRANSACTION" => Some(OpKind::SpiBeginTransaction),
        "SPI_END_TRANSACTION" => Some(OpKind::SpiEndTransaction),
        "I2C_INIT" => Some(OpKind::I2cInit),
        "I2C_START" => Some(OpKind::I2cStart),
        "I2C_STOP" => Some(OpKind::I2cStop),
        "I2C_WRITE" => Some(OpKind::I2cWrite),
        "I2C_READ" => Some(OpKind::I2cRead),
        "I2C_WRITE_TO" => Some(OpKind::I2cWriteTo),
        "I2C_READ_FROM" => Some(OpKind::I2cReadFrom),
        "I2C_SET_CLOCK" => Some(OpKind::I2cSetClock),
        "I2C_SCAN" => Some(OpKind::I2cScan),
        "CAN_INIT" => Some(OpKind::CanInit),
        "CAN_SEND" => Some(OpKind::CanSend),
        "CAN_RECEIVE" => Some(OpKind::CanReceive),
        "CAN_SET_FILTER" => Some(OpKind::CanSetFilter),
        "CAN_SET_BITRATE" => Some(OpKind::CanSetBitrate),
        "USB_INIT" => Some(OpKind::UsbInit),
        "USB_CONNECT" => Some(OpKind::UsbConnect),
        "USB_DISCONNECT" => Some(OpKind::UsbDisconnect),
        "USB_WRITE" => Some(OpKind::UsbWrite),
        "USB_READ" => Some(OpKind::UsbRead),
        "USB_AVAILABLE" => Some(OpKind::UsbAvailable),
        "WDT_ENABLE" => Some(OpKind::WdtEnable),
        "WDT_DISABLE" => Some(OpKind::WdtDisable),
        "WDT_RESET" => Some(OpKind::WdtReset),
        "WDT_SET_TIMEOUT" => Some(OpKind::WdtSetTimeout),
        "DMA_INIT" => Some(OpKind::DmaInit),
        "DMA_START" => Some(OpKind::DmaStart),
        "DMA_STOP" => Some(OpKind::DmaStop),
        "DMA_CONFIG" => Some(OpKind::DmaConfig),
        "DMA_SET_SOURCE" => Some(OpKind::DmaSetSource),
        "DMA_SET_DESTINATION" => Some(OpKind::DmaSetDestination),
        "EEPROM_READ" => Some(OpKind::EepromRead),
        "EEPROM_WRITE" => Some(OpKind::EepromWrite),
        "EEPROM_UPDATE" => Some(OpKind::EepromUpdate),
        "FLASH_READ" => Some(OpKind::FlashRead),
        "FLASH_WRITE" => Some(OpKind::FlashWrite),
        "FLASH_ERASE" => Some(OpKind::FlashErase),
        "SET_POWER_MODE" => Some(OpKind::SetPowerMode),
        "DISABLE_PERIPHERAL" => Some(OpKind::DisablePeripheral),
        "ENABLE_PERIPHERAL" => Some(OpKind::EnablePeripheral),
        "SET_CLOCK_SPEED" => Some(OpKind::SetClockSpeed),
        "ENTER_STANDBY" => Some(OpKind::EnterStandby),
        "ENTER_DEEP_SLEEP" => Some(OpKind::EnterDeepSleep),
        "RTC_INIT" => Some(OpKind::RtcInit),
        "RTC_SET_TIME" => Some(OpKind::RtcSetTime),
        "RTC_GET_TIME" => Some(OpKind::RtcGetTime),
        "RTC_SET_ALARM" => Some(OpKind::RtcSetAlarm),
        "RTC_SET_CALENDAR" => Some(OpKind::RtcSetCalendar),
        _ => None,
    }
}

/// Operation kinds (for parsing).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpKind {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Inc,
    Dec,
    Neg,
    Abs,
    // Logical/Bitwise
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
    Sar,
    Rol,
    Ror,
    // Comparison
    Cmp,
    Test,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Control Flow
    Nop,
    Halt,
    Sleep,
    // Bit Manipulation
    SetBit,
    ClearBit,
    ToggleBit,
    TestBit,
    CountOnes,
    CountZeros,
    FindFirstSet,
    FindFirstZero,
    // Floating Point
    FAdd,
    FSub,
    FMul,
    FDiv,
    FSqrt,
    FAbs,
    FCmp,
    // GPIO
    GpioRead,
    GpioWrite,
    GpioToggle,
    GpioSetMode,
    GpioReadPort,
    GpioWritePort,
    // Analog
    AdcRead,
    AdcStartConversion,
    AdcReadMulti,
    DacWrite,
    AdcSetResolution,
    AdcSetReference,
    // PWM
    PwmStart,
    PwmStop,
    PwmSetDutyCycle,
    PwmSetFrequency,
    PwmSetPulseWidth,
    // Timer
    TimerStart,
    TimerStop,
    TimerReset,
    TimerRead,
    TimerSetPeriod,
    TimerSetCompare,
    GetMillis,
    GetMicros,
    DelayMs,
    DelayUs,
    // UART
    UartInit,
    UartWriteByte,
    UartWriteBuffer,
    UartReadByte,
    UartReadBuffer,
    UartAvailable,
    UartFlush,
    UartSetBaudRate,
    // SPI
    SpiInit,
    SpiTransfer,
    SpiTransferBuffer,
    SpiSetMode,
    SpiSetClock,
    SpiSetBitOrder,
    SpiBeginTransaction,
    SpiEndTransaction,
    // I2C
    I2cInit,
    I2cStart,
    I2cStop,
    I2cWrite,
    I2cRead,
    I2cWriteTo,
    I2cReadFrom,
    I2cSetClock,
    I2cScan,
    // CAN
    CanInit,
    CanSend,
    CanReceive,
    CanSetFilter,
    CanSetBitrate,
    // USB
    UsbInit,
    UsbConnect,
    UsbDisconnect,
    UsbWrite,
    UsbRead,
    UsbAvailable,
    // Watchdog
    WdtEnable,
    WdtDisable,
    WdtReset,
    WdtSetTimeout,
    // DMA
    DmaInit,
    DmaStart,
    DmaStop,
    DmaConfig,
    DmaSetSource,
    DmaSetDestination,
    // EEPROM/Flash
    EepromRead,
    EepromWrite,
    EepromUpdate,
    FlashRead,
    FlashWrite,
    FlashErase,
    // Power Management
    SetPowerMode,
    DisablePeripheral,
    EnablePeripheral,
    SetClockSpeed,
    EnterStandby,
    EnterDeepSleep,
    // RTC
    RtcInit,
    RtcSetTime,
    RtcGetTime,
    RtcSetAlarm,
    RtcSetCalendar,
}

impl OpKind {
    /// Get the number of arguments expected for this operation.
    pub fn arity(self) -> usize {
        match self {
            // Nullary
            OpKind::Nop
            | OpKind::Halt
            | OpKind::Sleep
            | OpKind::GetMillis
            | OpKind::GetMicros
            | OpKind::WdtEnable
            | OpKind::WdtDisable
            | OpKind::WdtReset
            | OpKind::EnterStandby
            | OpKind::EnterDeepSleep
            | OpKind::RtcGetTime => 0,

            // Unary
            OpKind::Inc
            | OpKind::Dec
            | OpKind::Neg
            | OpKind::Abs
            | OpKind::Not
            | OpKind::CountOnes
            | OpKind::CountZeros
            | OpKind::FindFirstSet
            | OpKind::FindFirstZero
            | OpKind::FSqrt
            | OpKind::FAbs
            | OpKind::GpioRead
            | OpKind::GpioToggle
            | OpKind::GpioReadPort
            | OpKind::AdcRead
            | OpKind::AdcStartConversion
            | OpKind::AdcReadMulti
            | OpKind::AdcSetResolution
            | OpKind::AdcSetReference
            | OpKind::PwmStart
            | OpKind::PwmStop
            | OpKind::TimerStart
            | OpKind::TimerStop
            | OpKind::TimerReset
            | OpKind::TimerRead
            | OpKind::DelayMs
            | OpKind::DelayUs
            | OpKind::UartInit
            | OpKind::UartReadByte
            | OpKind::UartAvailable
            | OpKind::UartFlush
            | OpKind::SpiInit
            | OpKind::SpiBeginTransaction
            | OpKind::SpiEndTransaction
            | OpKind::I2cInit
            | OpKind::I2cStart
            | OpKind::I2cStop
            | OpKind::I2cRead
            | OpKind::I2cScan
            | OpKind::CanInit
            | OpKind::CanReceive
            | OpKind::UsbInit
            | OpKind::UsbConnect
            | OpKind::UsbDisconnect
            | OpKind::UsbRead
            | OpKind::UsbAvailable
            | OpKind::WdtSetTimeout
            | OpKind::DmaInit
            | OpKind::DmaStart
            | OpKind::DmaStop
            | OpKind::EepromRead
            | OpKind::FlashRead
            | OpKind::FlashErase
            | OpKind::SetPowerMode
            | OpKind::DisablePeripheral
            | OpKind::EnablePeripheral
            | OpKind::SetClockSpeed
            | OpKind::RtcInit
            | OpKind::RtcSetTime
            | OpKind::RtcSetAlarm
            | OpKind::RtcSetCalendar => 1,

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
            | OpKind::Test
            | OpKind::Eq
            | OpKind::Ne
            | OpKind::Lt
            | OpKind::Le
            | OpKind::Gt
            | OpKind::Ge
            | OpKind::SetBit
            | OpKind::ClearBit
            | OpKind::ToggleBit
            | OpKind::TestBit
            | OpKind::FAdd
            | OpKind::FSub
            | OpKind::FMul
            | OpKind::FDiv
            | OpKind::FCmp
            | OpKind::GpioWrite
            | OpKind::GpioSetMode
            | OpKind::GpioWritePort
            | OpKind::DacWrite
            | OpKind::PwmSetDutyCycle
            | OpKind::PwmSetFrequency
            | OpKind::PwmSetPulseWidth
            | OpKind::TimerSetPeriod
            | OpKind::TimerSetCompare
            | OpKind::UartWriteByte
            | OpKind::UartWriteBuffer
            | OpKind::UartReadBuffer
            | OpKind::UartSetBaudRate
            | OpKind::SpiTransfer
            | OpKind::SpiTransferBuffer
            | OpKind::SpiSetMode
            | OpKind::SpiSetClock
            | OpKind::SpiSetBitOrder
            | OpKind::I2cWrite
            | OpKind::I2cReadFrom
            | OpKind::I2cSetClock
            | OpKind::CanSend
            | OpKind::CanSetFilter
            | OpKind::CanSetBitrate
            | OpKind::UsbWrite
            | OpKind::DmaConfig
            | OpKind::DmaSetSource
            | OpKind::DmaSetDestination
            | OpKind::EepromWrite
            | OpKind::EepromUpdate
            | OpKind::FlashWrite => 2,

            // Ternary
            OpKind::I2cWriteTo => 3,
        }
    }
}