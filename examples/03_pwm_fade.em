// Example 3: PWM LED Fade
// Smoothly fades an LED up and down using PWM

const PwmChannel: u8 = 0;

fn main() {
    // Initialize PWM on channel 0
    SET_FREQUENCY(PWM(PwmChannel), 1000);  // 1kHz PWM frequency
    START(PWM(PwmChannel));
    
    // Fade cycle - repeat 5 times
    repeat 5 {
        // Fade up: 0 to 255
        for brightness in 0 to 255 {
            SET_DUTY_CYCLE(PWM(PwmChannel), brightness);
            DELAY_MS(4);  // ~1 second for full fade
        }
        
        // Fade down: 255 to 0
        for brightness in 255 downto 0 {
            SET_DUTY_CYCLE(PWM(PwmChannel), brightness);
            DELAY_MS(4);
        }
    }
    
    STOP(PWM(PwmChannel));
}
