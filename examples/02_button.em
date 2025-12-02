// Example 2: Button Input
// Reads a button and controls an LED

const ButtonPin: u8 = 2;
const LedPin: u8 = 13;
const Input: u8 = 0;
const Output: u8 = 1;
const Pullup: u8 = 2;

fn main() {
    // Configure pins
    SET_MODE(GPIO(ButtonPin), Pullup);  // Button with pull-up
    SET_MODE(GPIO(LedPin), Output);
    
    // Main loop - runs for 1000 iterations
    repeat 1000 {
        let button_state = READ(GPIO(ButtonPin));
        
        // Button is active LOW (pulled up, grounded when pressed)
        if button_state == 0 {
            WRITE(GPIO(LedPin), 1);
        } else {
            WRITE(GPIO(LedPin), 0);
        }
        
        DELAY_MS(10);  // Debounce delay
    }
}
