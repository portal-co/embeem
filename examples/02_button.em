// Example 2: Button Input
// Reads a button and controls an LED

const BUTTON_PIN: u8 = 2;
const LED_PIN: u8 = 13;
const INPUT: u8 = 0;
const OUTPUT: u8 = 1;
const PULLUP: u8 = 2;

fn main() {
    // Configure pins
    GPIO_SET_MODE(BUTTON_PIN, PULLUP);  // Button with pull-up
    GPIO_SET_MODE(LED_PIN, OUTPUT);
    
    // Main loop - runs for 1000 iterations
    repeat 1000 {
        let button_state = GPIO_READ(BUTTON_PIN);
        
        // Button is active LOW (pulled up, grounded when pressed)
        if button_state == 0 {
            GPIO_WRITE(LED_PIN, 1);
        } else {
            GPIO_WRITE(LED_PIN, 0);
        }
        
        DELAY_MS(10);  // Debounce delay
    }
}
