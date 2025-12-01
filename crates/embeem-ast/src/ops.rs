//! Operations available in Embeem.

use alloc::vec::Vec;

/// All operations available in the Embeem language.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Op<E> {
    // Arithmetic Operations
    Add(E, E),
    Sub(E, E),
    Mul(E, E),
    Div(E, E),
    Mod(E, E),
    Inc(E),
    Dec(E),
    Neg(E),
    Abs(E),

    // Logical/Bitwise Operations
    And(E, E),
    Or(E, E),
    Xor(E, E),
    Not(E),
    Shl(E, E),
    Shr(E, E),
    Sar(E, E),
    Rol(E, E),
    Ror(E, E),

    // Comparison Operations
    Cmp(E, E),
    Test(E, E),
    Eq(E, E),
    Ne(E, E),
    Lt(E, E),
    Le(E, E),
    Gt(E, E),
    Ge(E, E),

    // Control Flow
    Nop,
    Halt,
    Sleep,

    // Bit Manipulation
    SetBit(E, E),
    ClearBit(E, E),
    ToggleBit(E, E),
    TestBit(E, E),
    CountOnes(E),
    CountZeros(E),
    FindFirstSet(E),
    FindFirstZero(E),

    // Floating Point Operations
    FAdd(E, E),
    FSub(E, E),
    FMul(E, E),
    FDiv(E, E),
    FSqrt(E),
    FAbs(E),
    FCmp(E, E),

    // GPIO Operations
    GpioRead(E),
    GpioWrite(E, E),
    GpioToggle(E),
    GpioSetMode(E, E),
    GpioReadPort(E),
    GpioWritePort(E, E),

    // Analog Operations
    AdcRead(E),
    AdcStartConversion(E),
    AdcReadMulti(E),
    DacWrite(E, E),
    AdcSetResolution(E),
    AdcSetReference(E),

    // PWM Operations
    PwmStart(E),
    PwmStop(E),
    PwmSetDutyCycle(E, E),
    PwmSetFrequency(E, E),
    PwmSetPulseWidth(E, E),

    // Timer Operations
    TimerStart(E),
    TimerStop(E),
    TimerReset(E),
    TimerRead(E),
    TimerSetPeriod(E, E),
    TimerSetCompare(E, E),
    GetMillis,
    GetMicros,
    DelayMs(E),
    DelayUs(E),

    // UART Operations
    UartInit(E),
    UartWriteByte(E, E),
    UartWriteBuffer(E, E),
    UartReadByte(E),
    UartReadBuffer(E, E),
    UartAvailable(E),
    UartFlush(E),
    UartSetBaudRate(E, E),

    // SPI Operations
    SpiInit(E),
    SpiTransfer(E, E),
    SpiTransferBuffer(E, E),
    SpiSetMode(E, E),
    SpiSetClock(E, E),
    SpiSetBitOrder(E, E),
    SpiBeginTransaction(E),
    SpiEndTransaction(E),

    // I2C Operations
    I2cInit(E),
    I2cStart(E),
    I2cStop(E),
    I2cWrite(E, E),
    I2cRead(E),
    I2cWriteTo(E, E, E),
    I2cReadFrom(E, E),
    I2cSetClock(E, E),
    I2cScan(E),

    // CAN Operations
    CanInit(E),
    CanSend(E, E),
    CanReceive(E),
    CanSetFilter(E, E),
    CanSetBitrate(E, E),

    // USB Operations
    UsbInit(E),
    UsbConnect(E),
    UsbDisconnect(E),
    UsbWrite(E, E),
    UsbRead(E),
    UsbAvailable(E),

    // Watchdog Operations
    WdtEnable,
    WdtDisable,
    WdtReset,
    WdtSetTimeout(E),

    // DMA Operations
    DmaInit(E),
    DmaStart(E),
    DmaStop(E),
    DmaConfig(E, E),
    DmaSetSource(E, E),
    DmaSetDestination(E, E),

    // EEPROM/Flash Operations
    EepromRead(E),
    EepromWrite(E, E),
    EepromUpdate(E, E),
    FlashRead(E),
    FlashWrite(E, E),
    FlashErase(E),

    // Power Management
    SetPowerMode(E),
    DisablePeripheral(E),
    EnablePeripheral(E),
    SetClockSpeed(E),
    EnterStandby,
    EnterDeepSleep,

    // RTC Operations
    RtcInit(E),
    RtcSetTime(E),
    RtcGetTime,
    RtcSetAlarm(E),
    RtcSetCalendar(E),
}

impl<E> Op<E> {
    /// Map over the inner expressions in this operation.
    pub fn map<F, E2, Err>(self, mut f: F) -> Result<Op<E2>, Err>
    where
        F: FnMut(E) -> Result<E2, Err>,
    {
        Ok(match self {
            // Arithmetic
            Op::Add(a, b) => Op::Add(f(a)?, f(b)?),
            Op::Sub(a, b) => Op::Sub(f(a)?, f(b)?),
            Op::Mul(a, b) => Op::Mul(f(a)?, f(b)?),
            Op::Div(a, b) => Op::Div(f(a)?, f(b)?),
            Op::Mod(a, b) => Op::Mod(f(a)?, f(b)?),
            Op::Inc(a) => Op::Inc(f(a)?),
            Op::Dec(a) => Op::Dec(f(a)?),
            Op::Neg(a) => Op::Neg(f(a)?),
            Op::Abs(a) => Op::Abs(f(a)?),

            // Logical/Bitwise
            Op::And(a, b) => Op::And(f(a)?, f(b)?),
            Op::Or(a, b) => Op::Or(f(a)?, f(b)?),
            Op::Xor(a, b) => Op::Xor(f(a)?, f(b)?),
            Op::Not(a) => Op::Not(f(a)?),
            Op::Shl(a, b) => Op::Shl(f(a)?, f(b)?),
            Op::Shr(a, b) => Op::Shr(f(a)?, f(b)?),
            Op::Sar(a, b) => Op::Sar(f(a)?, f(b)?),
            Op::Rol(a, b) => Op::Rol(f(a)?, f(b)?),
            Op::Ror(a, b) => Op::Ror(f(a)?, f(b)?),

            // Comparison
            Op::Cmp(a, b) => Op::Cmp(f(a)?, f(b)?),
            Op::Test(a, b) => Op::Test(f(a)?, f(b)?),
            Op::Eq(a, b) => Op::Eq(f(a)?, f(b)?),
            Op::Ne(a, b) => Op::Ne(f(a)?, f(b)?),
            Op::Lt(a, b) => Op::Lt(f(a)?, f(b)?),
            Op::Le(a, b) => Op::Le(f(a)?, f(b)?),
            Op::Gt(a, b) => Op::Gt(f(a)?, f(b)?),
            Op::Ge(a, b) => Op::Ge(f(a)?, f(b)?),

            // Control Flow
            Op::Nop => Op::Nop,
            Op::Halt => Op::Halt,
            Op::Sleep => Op::Sleep,

            // Bit Manipulation
            Op::SetBit(a, b) => Op::SetBit(f(a)?, f(b)?),
            Op::ClearBit(a, b) => Op::ClearBit(f(a)?, f(b)?),
            Op::ToggleBit(a, b) => Op::ToggleBit(f(a)?, f(b)?),
            Op::TestBit(a, b) => Op::TestBit(f(a)?, f(b)?),
            Op::CountOnes(a) => Op::CountOnes(f(a)?),
            Op::CountZeros(a) => Op::CountZeros(f(a)?),
            Op::FindFirstSet(a) => Op::FindFirstSet(f(a)?),
            Op::FindFirstZero(a) => Op::FindFirstZero(f(a)?),

            // Floating Point
            Op::FAdd(a, b) => Op::FAdd(f(a)?, f(b)?),
            Op::FSub(a, b) => Op::FSub(f(a)?, f(b)?),
            Op::FMul(a, b) => Op::FMul(f(a)?, f(b)?),
            Op::FDiv(a, b) => Op::FDiv(f(a)?, f(b)?),
            Op::FSqrt(a) => Op::FSqrt(f(a)?),
            Op::FAbs(a) => Op::FAbs(f(a)?),
            Op::FCmp(a, b) => Op::FCmp(f(a)?, f(b)?),

            // GPIO
            Op::GpioRead(a) => Op::GpioRead(f(a)?),
            Op::GpioWrite(a, b) => Op::GpioWrite(f(a)?, f(b)?),
            Op::GpioToggle(a) => Op::GpioToggle(f(a)?),
            Op::GpioSetMode(a, b) => Op::GpioSetMode(f(a)?, f(b)?),
            Op::GpioReadPort(a) => Op::GpioReadPort(f(a)?),
            Op::GpioWritePort(a, b) => Op::GpioWritePort(f(a)?, f(b)?),

            // Analog
            Op::AdcRead(a) => Op::AdcRead(f(a)?),
            Op::AdcStartConversion(a) => Op::AdcStartConversion(f(a)?),
            Op::AdcReadMulti(a) => Op::AdcReadMulti(f(a)?),
            Op::DacWrite(a, b) => Op::DacWrite(f(a)?, f(b)?),
            Op::AdcSetResolution(a) => Op::AdcSetResolution(f(a)?),
            Op::AdcSetReference(a) => Op::AdcSetReference(f(a)?),

            // PWM
            Op::PwmStart(a) => Op::PwmStart(f(a)?),
            Op::PwmStop(a) => Op::PwmStop(f(a)?),
            Op::PwmSetDutyCycle(a, b) => Op::PwmSetDutyCycle(f(a)?, f(b)?),
            Op::PwmSetFrequency(a, b) => Op::PwmSetFrequency(f(a)?, f(b)?),
            Op::PwmSetPulseWidth(a, b) => Op::PwmSetPulseWidth(f(a)?, f(b)?),

            // Timer
            Op::TimerStart(a) => Op::TimerStart(f(a)?),
            Op::TimerStop(a) => Op::TimerStop(f(a)?),
            Op::TimerReset(a) => Op::TimerReset(f(a)?),
            Op::TimerRead(a) => Op::TimerRead(f(a)?),
            Op::TimerSetPeriod(a, b) => Op::TimerSetPeriod(f(a)?, f(b)?),
            Op::TimerSetCompare(a, b) => Op::TimerSetCompare(f(a)?, f(b)?),
            Op::GetMillis => Op::GetMillis,
            Op::GetMicros => Op::GetMicros,
            Op::DelayMs(a) => Op::DelayMs(f(a)?),
            Op::DelayUs(a) => Op::DelayUs(f(a)?),

            // UART
            Op::UartInit(a) => Op::UartInit(f(a)?),
            Op::UartWriteByte(a, b) => Op::UartWriteByte(f(a)?, f(b)?),
            Op::UartWriteBuffer(a, b) => Op::UartWriteBuffer(f(a)?, f(b)?),
            Op::UartReadByte(a) => Op::UartReadByte(f(a)?),
            Op::UartReadBuffer(a, b) => Op::UartReadBuffer(f(a)?, f(b)?),
            Op::UartAvailable(a) => Op::UartAvailable(f(a)?),
            Op::UartFlush(a) => Op::UartFlush(f(a)?),
            Op::UartSetBaudRate(a, b) => Op::UartSetBaudRate(f(a)?, f(b)?),

            // SPI
            Op::SpiInit(a) => Op::SpiInit(f(a)?),
            Op::SpiTransfer(a, b) => Op::SpiTransfer(f(a)?, f(b)?),
            Op::SpiTransferBuffer(a, b) => Op::SpiTransferBuffer(f(a)?, f(b)?),
            Op::SpiSetMode(a, b) => Op::SpiSetMode(f(a)?, f(b)?),
            Op::SpiSetClock(a, b) => Op::SpiSetClock(f(a)?, f(b)?),
            Op::SpiSetBitOrder(a, b) => Op::SpiSetBitOrder(f(a)?, f(b)?),
            Op::SpiBeginTransaction(a) => Op::SpiBeginTransaction(f(a)?),
            Op::SpiEndTransaction(a) => Op::SpiEndTransaction(f(a)?),

            // I2C
            Op::I2cInit(a) => Op::I2cInit(f(a)?),
            Op::I2cStart(a) => Op::I2cStart(f(a)?),
            Op::I2cStop(a) => Op::I2cStop(f(a)?),
            Op::I2cWrite(a, b) => Op::I2cWrite(f(a)?, f(b)?),
            Op::I2cRead(a) => Op::I2cRead(f(a)?),
            Op::I2cWriteTo(a, b, c) => Op::I2cWriteTo(f(a)?, f(b)?, f(c)?),
            Op::I2cReadFrom(a, b) => Op::I2cReadFrom(f(a)?, f(b)?),
            Op::I2cSetClock(a, b) => Op::I2cSetClock(f(a)?, f(b)?),
            Op::I2cScan(a) => Op::I2cScan(f(a)?),

            // CAN
            Op::CanInit(a) => Op::CanInit(f(a)?),
            Op::CanSend(a, b) => Op::CanSend(f(a)?, f(b)?),
            Op::CanReceive(a) => Op::CanReceive(f(a)?),
            Op::CanSetFilter(a, b) => Op::CanSetFilter(f(a)?, f(b)?),
            Op::CanSetBitrate(a, b) => Op::CanSetBitrate(f(a)?, f(b)?),

            // USB
            Op::UsbInit(a) => Op::UsbInit(f(a)?),
            Op::UsbConnect(a) => Op::UsbConnect(f(a)?),
            Op::UsbDisconnect(a) => Op::UsbDisconnect(f(a)?),
            Op::UsbWrite(a, b) => Op::UsbWrite(f(a)?, f(b)?),
            Op::UsbRead(a) => Op::UsbRead(f(a)?),
            Op::UsbAvailable(a) => Op::UsbAvailable(f(a)?),

            // Watchdog
            Op::WdtEnable => Op::WdtEnable,
            Op::WdtDisable => Op::WdtDisable,
            Op::WdtReset => Op::WdtReset,
            Op::WdtSetTimeout(a) => Op::WdtSetTimeout(f(a)?),

            // DMA
            Op::DmaInit(a) => Op::DmaInit(f(a)?),
            Op::DmaStart(a) => Op::DmaStart(f(a)?),
            Op::DmaStop(a) => Op::DmaStop(f(a)?),
            Op::DmaConfig(a, b) => Op::DmaConfig(f(a)?, f(b)?),
            Op::DmaSetSource(a, b) => Op::DmaSetSource(f(a)?, f(b)?),
            Op::DmaSetDestination(a, b) => Op::DmaSetDestination(f(a)?, f(b)?),

            // EEPROM/Flash
            Op::EepromRead(a) => Op::EepromRead(f(a)?),
            Op::EepromWrite(a, b) => Op::EepromWrite(f(a)?, f(b)?),
            Op::EepromUpdate(a, b) => Op::EepromUpdate(f(a)?, f(b)?),
            Op::FlashRead(a) => Op::FlashRead(f(a)?),
            Op::FlashWrite(a, b) => Op::FlashWrite(f(a)?, f(b)?),
            Op::FlashErase(a) => Op::FlashErase(f(a)?),

            // Power Management
            Op::SetPowerMode(a) => Op::SetPowerMode(f(a)?),
            Op::DisablePeripheral(a) => Op::DisablePeripheral(f(a)?),
            Op::EnablePeripheral(a) => Op::EnablePeripheral(f(a)?),
            Op::SetClockSpeed(a) => Op::SetClockSpeed(f(a)?),
            Op::EnterStandby => Op::EnterStandby,
            Op::EnterDeepSleep => Op::EnterDeepSleep,

            // RTC
            Op::RtcInit(a) => Op::RtcInit(f(a)?),
            Op::RtcSetTime(a) => Op::RtcSetTime(f(a)?),
            Op::RtcGetTime => Op::RtcGetTime,
            Op::RtcSetAlarm(a) => Op::RtcSetAlarm(f(a)?),
            Op::RtcSetCalendar(a) => Op::RtcSetCalendar(f(a)?),
        })
    }
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

    /// Build an Op from arguments.
    pub fn build<E>(self, args: Vec<E>) -> Option<Op<E>> {
        if args.len() != self.arity() {
            return None;
        }
        let mut args = args.into_iter();
        Some(match self {
            // Nullary
            OpKind::Nop => Op::Nop,
            OpKind::Halt => Op::Halt,
            OpKind::Sleep => Op::Sleep,
            OpKind::GetMillis => Op::GetMillis,
            OpKind::GetMicros => Op::GetMicros,
            OpKind::WdtEnable => Op::WdtEnable,
            OpKind::WdtDisable => Op::WdtDisable,
            OpKind::WdtReset => Op::WdtReset,
            OpKind::EnterStandby => Op::EnterStandby,
            OpKind::EnterDeepSleep => Op::EnterDeepSleep,
            OpKind::RtcGetTime => Op::RtcGetTime,

            // Unary
            OpKind::Inc => Op::Inc(args.next()?),
            OpKind::Dec => Op::Dec(args.next()?),
            OpKind::Neg => Op::Neg(args.next()?),
            OpKind::Abs => Op::Abs(args.next()?),
            OpKind::Not => Op::Not(args.next()?),
            OpKind::CountOnes => Op::CountOnes(args.next()?),
            OpKind::CountZeros => Op::CountZeros(args.next()?),
            OpKind::FindFirstSet => Op::FindFirstSet(args.next()?),
            OpKind::FindFirstZero => Op::FindFirstZero(args.next()?),
            OpKind::FSqrt => Op::FSqrt(args.next()?),
            OpKind::FAbs => Op::FAbs(args.next()?),
            OpKind::GpioRead => Op::GpioRead(args.next()?),
            OpKind::GpioToggle => Op::GpioToggle(args.next()?),
            OpKind::GpioReadPort => Op::GpioReadPort(args.next()?),
            OpKind::AdcRead => Op::AdcRead(args.next()?),
            OpKind::AdcStartConversion => Op::AdcStartConversion(args.next()?),
            OpKind::AdcReadMulti => Op::AdcReadMulti(args.next()?),
            OpKind::AdcSetResolution => Op::AdcSetResolution(args.next()?),
            OpKind::AdcSetReference => Op::AdcSetReference(args.next()?),
            OpKind::PwmStart => Op::PwmStart(args.next()?),
            OpKind::PwmStop => Op::PwmStop(args.next()?),
            OpKind::TimerStart => Op::TimerStart(args.next()?),
            OpKind::TimerStop => Op::TimerStop(args.next()?),
            OpKind::TimerReset => Op::TimerReset(args.next()?),
            OpKind::TimerRead => Op::TimerRead(args.next()?),
            OpKind::DelayMs => Op::DelayMs(args.next()?),
            OpKind::DelayUs => Op::DelayUs(args.next()?),
            OpKind::UartInit => Op::UartInit(args.next()?),
            OpKind::UartReadByte => Op::UartReadByte(args.next()?),
            OpKind::UartAvailable => Op::UartAvailable(args.next()?),
            OpKind::UartFlush => Op::UartFlush(args.next()?),
            OpKind::SpiInit => Op::SpiInit(args.next()?),
            OpKind::SpiBeginTransaction => Op::SpiBeginTransaction(args.next()?),
            OpKind::SpiEndTransaction => Op::SpiEndTransaction(args.next()?),
            OpKind::I2cInit => Op::I2cInit(args.next()?),
            OpKind::I2cStart => Op::I2cStart(args.next()?),
            OpKind::I2cStop => Op::I2cStop(args.next()?),
            OpKind::I2cRead => Op::I2cRead(args.next()?),
            OpKind::I2cScan => Op::I2cScan(args.next()?),
            OpKind::CanInit => Op::CanInit(args.next()?),
            OpKind::CanReceive => Op::CanReceive(args.next()?),
            OpKind::UsbInit => Op::UsbInit(args.next()?),
            OpKind::UsbConnect => Op::UsbConnect(args.next()?),
            OpKind::UsbDisconnect => Op::UsbDisconnect(args.next()?),
            OpKind::UsbRead => Op::UsbRead(args.next()?),
            OpKind::UsbAvailable => Op::UsbAvailable(args.next()?),
            OpKind::WdtSetTimeout => Op::WdtSetTimeout(args.next()?),
            OpKind::DmaInit => Op::DmaInit(args.next()?),
            OpKind::DmaStart => Op::DmaStart(args.next()?),
            OpKind::DmaStop => Op::DmaStop(args.next()?),
            OpKind::EepromRead => Op::EepromRead(args.next()?),
            OpKind::FlashRead => Op::FlashRead(args.next()?),
            OpKind::FlashErase => Op::FlashErase(args.next()?),
            OpKind::SetPowerMode => Op::SetPowerMode(args.next()?),
            OpKind::DisablePeripheral => Op::DisablePeripheral(args.next()?),
            OpKind::EnablePeripheral => Op::EnablePeripheral(args.next()?),
            OpKind::SetClockSpeed => Op::SetClockSpeed(args.next()?),
            OpKind::RtcInit => Op::RtcInit(args.next()?),
            OpKind::RtcSetTime => Op::RtcSetTime(args.next()?),
            OpKind::RtcSetAlarm => Op::RtcSetAlarm(args.next()?),
            OpKind::RtcSetCalendar => Op::RtcSetCalendar(args.next()?),

            // Binary
            OpKind::Add => Op::Add(args.next()?, args.next()?),
            OpKind::Sub => Op::Sub(args.next()?, args.next()?),
            OpKind::Mul => Op::Mul(args.next()?, args.next()?),
            OpKind::Div => Op::Div(args.next()?, args.next()?),
            OpKind::Mod => Op::Mod(args.next()?, args.next()?),
            OpKind::And => Op::And(args.next()?, args.next()?),
            OpKind::Or => Op::Or(args.next()?, args.next()?),
            OpKind::Xor => Op::Xor(args.next()?, args.next()?),
            OpKind::Shl => Op::Shl(args.next()?, args.next()?),
            OpKind::Shr => Op::Shr(args.next()?, args.next()?),
            OpKind::Sar => Op::Sar(args.next()?, args.next()?),
            OpKind::Rol => Op::Rol(args.next()?, args.next()?),
            OpKind::Ror => Op::Ror(args.next()?, args.next()?),
            OpKind::Cmp => Op::Cmp(args.next()?, args.next()?),
            OpKind::Test => Op::Test(args.next()?, args.next()?),
            OpKind::Eq => Op::Eq(args.next()?, args.next()?),
            OpKind::Ne => Op::Ne(args.next()?, args.next()?),
            OpKind::Lt => Op::Lt(args.next()?, args.next()?),
            OpKind::Le => Op::Le(args.next()?, args.next()?),
            OpKind::Gt => Op::Gt(args.next()?, args.next()?),
            OpKind::Ge => Op::Ge(args.next()?, args.next()?),
            OpKind::SetBit => Op::SetBit(args.next()?, args.next()?),
            OpKind::ClearBit => Op::ClearBit(args.next()?, args.next()?),
            OpKind::ToggleBit => Op::ToggleBit(args.next()?, args.next()?),
            OpKind::TestBit => Op::TestBit(args.next()?, args.next()?),
            OpKind::FAdd => Op::FAdd(args.next()?, args.next()?),
            OpKind::FSub => Op::FSub(args.next()?, args.next()?),
            OpKind::FMul => Op::FMul(args.next()?, args.next()?),
            OpKind::FDiv => Op::FDiv(args.next()?, args.next()?),
            OpKind::FCmp => Op::FCmp(args.next()?, args.next()?),
            OpKind::GpioWrite => Op::GpioWrite(args.next()?, args.next()?),
            OpKind::GpioSetMode => Op::GpioSetMode(args.next()?, args.next()?),
            OpKind::GpioWritePort => Op::GpioWritePort(args.next()?, args.next()?),
            OpKind::DacWrite => Op::DacWrite(args.next()?, args.next()?),
            OpKind::PwmSetDutyCycle => Op::PwmSetDutyCycle(args.next()?, args.next()?),
            OpKind::PwmSetFrequency => Op::PwmSetFrequency(args.next()?, args.next()?),
            OpKind::PwmSetPulseWidth => Op::PwmSetPulseWidth(args.next()?, args.next()?),
            OpKind::TimerSetPeriod => Op::TimerSetPeriod(args.next()?, args.next()?),
            OpKind::TimerSetCompare => Op::TimerSetCompare(args.next()?, args.next()?),
            OpKind::UartWriteByte => Op::UartWriteByte(args.next()?, args.next()?),
            OpKind::UartWriteBuffer => Op::UartWriteBuffer(args.next()?, args.next()?),
            OpKind::UartReadBuffer => Op::UartReadBuffer(args.next()?, args.next()?),
            OpKind::UartSetBaudRate => Op::UartSetBaudRate(args.next()?, args.next()?),
            OpKind::SpiTransfer => Op::SpiTransfer(args.next()?, args.next()?),
            OpKind::SpiTransferBuffer => Op::SpiTransferBuffer(args.next()?, args.next()?),
            OpKind::SpiSetMode => Op::SpiSetMode(args.next()?, args.next()?),
            OpKind::SpiSetClock => Op::SpiSetClock(args.next()?, args.next()?),
            OpKind::SpiSetBitOrder => Op::SpiSetBitOrder(args.next()?, args.next()?),
            OpKind::I2cWrite => Op::I2cWrite(args.next()?, args.next()?),
            OpKind::I2cReadFrom => Op::I2cReadFrom(args.next()?, args.next()?),
            OpKind::I2cSetClock => Op::I2cSetClock(args.next()?, args.next()?),
            OpKind::CanSend => Op::CanSend(args.next()?, args.next()?),
            OpKind::CanSetFilter => Op::CanSetFilter(args.next()?, args.next()?),
            OpKind::CanSetBitrate => Op::CanSetBitrate(args.next()?, args.next()?),
            OpKind::UsbWrite => Op::UsbWrite(args.next()?, args.next()?),
            OpKind::DmaConfig => Op::DmaConfig(args.next()?, args.next()?),
            OpKind::DmaSetSource => Op::DmaSetSource(args.next()?, args.next()?),
            OpKind::DmaSetDestination => Op::DmaSetDestination(args.next()?, args.next()?),
            OpKind::EepromWrite => Op::EepromWrite(args.next()?, args.next()?),
            OpKind::EepromUpdate => Op::EepromUpdate(args.next()?, args.next()?),
            OpKind::FlashWrite => Op::FlashWrite(args.next()?, args.next()?),

            // Ternary
            OpKind::I2cWriteTo => Op::I2cWriteTo(args.next()?, args.next()?, args.next()?),
        })
    }
}
