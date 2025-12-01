// Example 1: LED Blinker
// Blinks an LED on pin 13 exactly 10 times

fn main() {
    // Configure pin 13 as output
    GPIO_SET_MODE(13, 1);  // 1 = OUTPUT
    
    // Blink 10 times
    repeat 10 {
        GPIO_WRITE(13, 1);   // HIGH
        DELAY_MS(500);
        GPIO_WRITE(13, 0);   // LOW
        DELAY_MS(500);
    }
}
