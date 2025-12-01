// Example 8: Bit Manipulation
// Demonstrates bit operations for register manipulation

const CONTROL_REG: u8 = 0x40;
const STATUS_REG: u8 = 0x41;

// Bit positions
const ENABLE_BIT: u8 = 0;
const RUN_BIT: u8 = 1;
const INT_ENABLE_BIT: u8 = 2;
const MODE_BITS: u8 = 4;  // Bits 4-5

fn read_register(addr: u8) -> u8 {
    // Simulated register read via GPIO port
    GPIO_READ_PORT(addr)
}

fn write_register(addr: u8, value: u8) {
    GPIO_WRITE_PORT(addr, value);
}

fn set_bit_in_reg(addr: u8, bit: u8) {
    let current = read_register(addr);
    let new_value = SET_BIT(current, bit);
    write_register(addr, new_value);
}

fn clear_bit_in_reg(addr: u8, bit: u8) {
    let current = read_register(addr);
    let new_value = CLEAR_BIT(current, bit);
    write_register(addr, new_value);
}

fn set_mode(mode: u8) {
    // Mode is 2 bits (0-3) at bit position 4-5
    let current = read_register(CONTROL_REG);
    
    // Clear mode bits
    let mask = NOT(SHL(0x03, MODE_BITS));
    let cleared = AND(current, mask);
    
    // Set new mode
    let mode_shifted = SHL(AND(mode, 0x03), MODE_BITS);
    let new_value = OR(cleared, mode_shifted);
    
    write_register(CONTROL_REG, new_value);
}

fn wait_for_ready() -> bool {
    let mut ready = false;
    
    // Wait with timeout
    while not ready max 100 {
        let status = read_register(STATUS_REG);
        ready = TEST_BIT(status, 7) == 1;  // Ready bit is bit 7
        
        if not ready {
            DELAY_MS(10);
        }
    }
    
    ready
}

fn count_active_flags() -> u8 {
    let status = read_register(STATUS_REG);
    COUNT_ONES(status)
}

fn main() {
    // Initialize - disable everything
    write_register(CONTROL_REG, 0x00);
    
    // Configure: set mode 2, enable interrupts
    set_mode(2);
    set_bit_in_reg(CONTROL_REG, INT_ENABLE_BIT);
    
    // Enable the device
    set_bit_in_reg(CONTROL_REG, ENABLE_BIT);
    
    // Wait for ready
    let is_ready = wait_for_ready();
    
    if is_ready {
        // Start operation
        set_bit_in_reg(CONTROL_REG, RUN_BIT);
        
        // Monitor status for 100 cycles
        repeat 100 {
            let flags = count_active_flags();
            
            // If more than 4 flags, there might be an issue
            if flags > 4 {
                WRITE(GPIO(13), 1);  // Warning LED
            } else {
                WRITE(GPIO(13), 0);
            }
            
            DELAY_MS(100);
        }
        
        // Stop operation
        clear_bit_in_reg(CONTROL_REG, RUN_BIT);
    }
    
    // Disable device
    clear_bit_in_reg(CONTROL_REG, ENABLE_BIT);
}
