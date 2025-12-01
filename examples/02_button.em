// Example 2: Button Input
// Reads a button and controls an LED

const BUTTON_PIN: u8 = 2;
const LED_PIN: u8 = 13;
const INPUT: u8 = 0;
const OUTPUT: u8 = 1;
const PULLUP: u8 = 2;

fn main() {
    // Configure pins
    SET_MODE(GPIO(BUTTON_PIN), PULLUP);  // Button with pull-up
    SET_MODE(GPIO(LED_PIN), OUTPUT);
    
    // Main loop - runs for 1000 iterations
    repeat 1000 {
        let button_state = READ(GPIO(BUTTON_PIN));
        
        // Button is active LOW (pulled up, grounded when pressed)
        if button_state == 0 {
            WRITE(GPIO(LED_PIN), 1);
        } else {
            WRITE(GPIO(LED_PIN), 0);
        }
        
        DELAY_MS(10);  // Debounce delay
    }
}
