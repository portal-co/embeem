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
    GPIO_SET_MODE(LED_LOW, 1);
    GPIO_SET_MODE(LED_MED, 1);
    GPIO_SET_MODE(LED_HIGH, 1);
}

fn update_leds(value: u16) {
    // All LEDs off initially
    GPIO_WRITE(LED_LOW, 0);
    GPIO_WRITE(LED_MED, 0);
    GPIO_WRITE(LED_HIGH, 0);
    
    // Light appropriate LED(s)
    if value >= THRESH_HIGH {
        GPIO_WRITE(LED_HIGH, 1);
        GPIO_WRITE(LED_MED, 1);
        GPIO_WRITE(LED_LOW, 1);
    } else if value >= THRESH_LOW {
        GPIO_WRITE(LED_MED, 1);
        GPIO_WRITE(LED_LOW, 1);
    } else {
        GPIO_WRITE(LED_LOW, 1);
    }
}

fn main() {
    // Initialize
    ADC_SET_RESOLUTION(10);  // 10-bit resolution
    setup_leds();
    
    // Main loop - 500 readings
    repeat 500 {
        let reading = ADC_READ(SENSOR_CHANNEL);
        update_leds(reading);
        DELAY_MS(100);  // Read 10 times per second
    }
}
