// Example 4: Analog Sensor Reading
// Reads an analog sensor and lights LEDs based on value

const SENSOR_CHANNEL: u8 = 0;
const LED_LOW: u8 = 10;
const LED_MED: u8 = 11;
const LED_HIGH: u8 = 12;

// Threshold values (0-1023 for 10-bit ADC)
const THRESH_LOW: u16 = 341;
const THRESH_HIGH: u16 = 682;

fn setup_leds() {
    SET_MODE(GPIO(LED_LOW), 1);
    SET_MODE(GPIO(LED_MED), 1);
    SET_MODE(GPIO(LED_HIGH), 1);
}

fn update_leds(value: u16) {
    // All LEDs off initially
    WRITE(GPIO(LED_LOW), 0);
    WRITE(GPIO(LED_MED), 0);
    WRITE(GPIO(LED_HIGH), 0);
    
    // Light appropriate LED(s)
    if value >= THRESH_HIGH {
        WRITE(GPIO(LED_HIGH), 1);
        WRITE(GPIO(LED_MED), 1);
        WRITE(GPIO(LED_LOW), 1);
    } else if value >= THRESH_LOW {
        WRITE(GPIO(LED_MED), 1);
        WRITE(GPIO(LED_LOW), 1);
    } else {
        WRITE(GPIO(LED_LOW), 1);
    }
}

fn main() {
    // Initialize
    SET_RESOLUTION(ADC(SENSOR_CHANNEL), 10);  // 10-bit resolution
    setup_leds();
    
    // Main loop - 500 readings
    repeat 500 {
        let reading = READ(ADC(SENSOR_CHANNEL));
        update_leds(reading);
        DELAY_MS(100);  // Read 10 times per second
    }
}
