// Example 1: LED Blinker
// Blinks an LED on pin 13 exactly 10 times

fn main() {
    // Configure pin 13 as output
    SET_MODE(GPIO(13), 1);  // 1 = OUTPUT
    
    // Blink 10 times
    repeat 10 {
        WRITE(GPIO(13), 1);   // HIGH
        DELAY_MS(500);
        WRITE(GPIO(13), 0);   // LOW
        DELAY_MS(500);
    }
}
