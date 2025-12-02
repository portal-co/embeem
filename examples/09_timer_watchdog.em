// Example 9: Timer and Watchdog
// Demonstrates timer operations and watchdog feeding

const TimerNum: u8 = 0;
const WatchdogTimeout: u16 = 2000;  // 2 seconds

fn main() {
    // Initialize timer
    SET_PERIOD(TIMER(TimerNum), 1000);  // 1ms period
    RESET(TIMER(TimerNum));
    START(TIMER(TimerNum));
    
    // Enable watchdog
    SET_TIMEOUT(WDT(), WatchdogTimeout);
    ENABLE(WDT());
    
    // Setup LED
    SET_MODE(GPIO(13), 1);
    
    let mut last_blink: u32 = 0;
    let mut led_state: u8 = 0;
    
    // Main loop - 10000 iterations
    repeat 10000 {
        // Read current time
        let current_time = READ(TIMER(TimerNum));
        
        // Blink LED every 500ms
        if current_time - last_blink >= 500 {
            led_state = if led_state == 0 { 1 } else { 0 };
            WRITE(GPIO(13), led_state);
            last_blink = current_time;
        }
        
        // Feed the watchdog to prevent reset
        RESET(WDT());
        
        // Small delay
        DELAY_MS(1);
    }
    
    // Cleanup
    STOP(TIMER(TimerNum));
    DISABLE(WDT());
}
