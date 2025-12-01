// Example 9: Timer and Watchdog
// Demonstrates timer operations and watchdog feeding

const TIMER_NUM: u8 = 0;
const WATCHDOG_TIMEOUT: u16 = 2000;  // 2 seconds

fn main() {
    // Initialize timer
    TIMER_SET_PERIOD(TIMER_NUM, 1000);  // 1ms period
    TIMER_RESET(TIMER_NUM);
    TIMER_START(TIMER_NUM);
    
    // Enable watchdog
    WDT_SET_TIMEOUT(WATCHDOG_TIMEOUT);
    WDT_ENABLE();
    
    // Setup LED
    GPIO_SET_MODE(13, 1);
    
    let mut last_blink: u32 = 0;
    let mut led_state: u8 = 0;
    
    // Main loop - 10000 iterations
    repeat 10000 {
        // Read current time
        let current_time = TIMER_READ(TIMER_NUM);
        
        // Blink LED every 500ms
        if current_time - last_blink >= 500 {
            led_state = if led_state == 0 { 1 } else { 0 };
            GPIO_WRITE(13, led_state);
            last_blink = current_time;
        }
        
        // Feed the watchdog to prevent reset
        WDT_RESET();
        
        // Small delay
        DELAY_MS(1);
    }
    
    // Cleanup
    TIMER_STOP(TIMER_NUM);
    WDT_DISABLE();
}
