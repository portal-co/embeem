// Example 6: I2C Temperature Sensor
// Reads temperature from a common I2C sensor (e.g., TMP102)

const I2cNum: u8 = 0;
const SensorAddr: u8 = 0x48;  // TMP102 default address
const TempReg: u8 = 0x00;

fn read_temperature() -> i16 {
    // Start I2C transaction
    START(I2C(I2cNum));
    
    // Write register address
    I2C_WRITE_TO(I2cNum, SensorAddr, TempReg);
    
    // Read 2 bytes (12-bit temperature)
    let high = I2C_READ_FROM(I2cNum, SensorAddr);
    let low = I2C_READ_FROM(I2cNum, SensorAddr);
    
    STOP(I2C(I2cNum));
    
    // Combine bytes and convert to temperature
    // TMP102 format: 12 bits, MSB first, 0.0625°C per bit
    let raw = SHL(high, 4) | SHR(low, 4);
    
    // Return temperature in 0.1°C units
    // raw * 0.625 = raw * 5 / 8
    let temp = MUL(raw, 5) / 8;
    temp
}

fn main() {
    // Initialize I2C
    INIT(I2C(I2cNum));
    SET_CLOCK(I2C(I2cNum), 100000);  // 100kHz standard mode
    
    // Setup LED indicator
    SET_MODE(GPIO(13), 1);
    
    // Read temperature 100 times
    repeat 100 {
        let temp = read_temperature();
        
        // LED on if temp > 25.0°C (250 in 0.1°C units)
        if temp > 250 {
            WRITE(GPIO(13), 1);
        } else {
            WRITE(GPIO(13), 0);
        }
        
        DELAY_MS(1000);  // Read once per second
    }
}
