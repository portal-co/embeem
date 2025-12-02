// Example 4: Analog Sensor Reading
// Reads an analog sensor and lights LEDs based on value

const SensorChannel: u8 = 0;
const LedLow: u8 = 10;
const LedMed: u8 = 11;
const LedHigh: u8 = 12;

// Threshold values (0-1023 for 10-bit ADC)
const ThreshLow: u16 = 341;
const ThreshHigh: u16 = 682;

fn setup_leds() {
    SET_MODE(GPIO(LedLow), 1);
    SET_MODE(GPIO(LedMed), 1);
    SET_MODE(GPIO(LedHigh), 1);
}

fn update_leds(value: u16) {
    // All LEDs off initially
    WRITE(GPIO(LedLow), 0);
    WRITE(GPIO(LedMed), 0);
    WRITE(GPIO(LedHigh), 0);
    
    // Light appropriate LED(s)
    if value >= ThreshHigh {
        WRITE(GPIO(LedHigh), 1);
        WRITE(GPIO(LedMed), 1);
        WRITE(GPIO(LedLow), 1);
    } else if value >= ThreshLow {
        WRITE(GPIO(LedMed), 1);
        WRITE(GPIO(LedLow), 1);
    } else {
        WRITE(GPIO(LedLow), 1);
    }
}

fn main() {
    // Initialize
    SET_RESOLUTION(ADC(SensorChannel), 10);  // 10-bit resolution
    setup_leds();
    
    // Main loop - 500 readings
    repeat 500 {
        let reading = READ(ADC(SensorChannel));
        update_leds(reading);
        DELAY_MS(100);  // Read 10 times per second
    }
}
