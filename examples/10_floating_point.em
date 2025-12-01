// Example 10: Floating Point Math
// Demonstrates floating point operations for signal processing

// Constants for a simple low-pass filter
const ALPHA: f32 = 0.1;  // Filter coefficient

fn low_pass_filter(current: f32, previous: f32) -> f32 {
    // y[n] = alpha * x[n] + (1 - alpha) * y[n-1]
    let weighted_current = FMUL(ALPHA, current);
    let one_minus_alpha = FSUB(1.0, ALPHA);
    let weighted_previous = FMUL(one_minus_alpha, previous);
    FADD(weighted_current, weighted_previous)
}

fn calculate_rms(samples: [f32; 16]) -> f32 {
    // Root Mean Square calculation
    let mut sum_squares: f32 = 0.0;
    
    for i in 0 to 15 {
        let sample = samples[i];
        let squared = FMUL(sample, sample);
        sum_squares = FADD(sum_squares, squared);
    }
    
    let mean = FDIV(sum_squares, 16.0);
    FSQRT(mean)
}

fn normalize_angle(angle: f32) -> f32 {
    // Normalize angle to 0-360 range
    let pi2: f32 = 6.28318;  // 2 * PI
    
    // Simple modulo using subtraction (for positive angles)
    let mut result = angle;
    
    // Handle up to 10 full rotations
    for i in 0 to 9 {
        if FCMP(result, pi2) > 0 {
            result = FSUB(result, pi2);
        }
    }
    
    result
}

fn main() {
    // Initialize ADC for sensor input
    ADC_SET_RESOLUTION(12);  // 12-bit for better precision
    
    // Buffer for samples
    let mut samples: [f32; 16] = [0.0; 16];
    let mut filtered: f32 = 0.0;
    let mut sample_index: u8 = 0;
    
    // Main processing loop
    repeat 1000 {
        // Read raw ADC value and convert to voltage (0-3.3V)
        let raw = ADC_READ(0);
        let voltage: f32 = FDIV(FMUL(raw as f32, 3.3), 4095.0);
        
        // Apply low-pass filter
        filtered = low_pass_filter(voltage, filtered);
        
        // Store in circular buffer
        samples[sample_index] = voltage;
        sample_index = if sample_index == 15 { 0 } else { sample_index + 1 };
        
        // Calculate RMS every 16 samples
        if sample_index == 0 {
            let rms = calculate_rms(samples);
            
            // Output RMS as PWM duty cycle (assuming 0-3.3V range)
            let duty = FMUL(FDIV(rms, 3.3), 255.0);
            let duty_int: u8 = if duty > 255.0 { 255 } else { duty as u8 };
            PWM_SET_DUTY_CYCLE(0, duty_int);
        }
        
        DELAY_US(100);  // 10kHz sample rate
    }
}
