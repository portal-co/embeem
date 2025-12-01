// Example 3: PWM LED Fade
// Smoothly fades an LED up and down using PWM

const PWM_CHANNEL: u8 = 0;

fn main() {
    // Initialize PWM on channel 0
    PWM_SET_FREQUENCY(PWM_CHANNEL, 1000);  // 1kHz PWM frequency
    PWM_START(PWM_CHANNEL);
    
    // Fade cycle - repeat 5 times
    repeat 5 {
        // Fade up: 0 to 255
        for brightness in 0 to 255 {
            PWM_SET_DUTY_CYCLE(PWM_CHANNEL, brightness);
            DELAY_MS(4);  // ~1 second for full fade
        }
        
        // Fade down: 255 to 0
        for brightness in 255 downto 0 {
            PWM_SET_DUTY_CYCLE(PWM_CHANNEL, brightness);
            DELAY_MS(4);
        }
    }
    
    PWM_STOP(PWM_CHANNEL);
}
