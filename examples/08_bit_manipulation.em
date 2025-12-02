// Example 8: Bit Manipulation
// Demonstrates bit operations for register manipulation

const ControlReg: u8 = 0x40;
const StatusReg: u8 = 0x41;

// Bit positions
const EnableBit: u8 = 0;
const RunBit: u8 = 1;
const IntEnableBit: u8 = 2;
const ModeBits: u8 = 4;  // Bits 4-5

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
    let current = read_register(ControlReg);
    
    // Clear mode bits
    let mask = NOT(SHL(0x03, ModeBits));
    let cleared = AND(current, mask);
    
    // Set new mode
    let mode_shifted = SHL(AND(mode, 0x03), ModeBits);
    let new_value = OR(cleared, mode_shifted);
    
    write_register(ControlReg, new_value);
}

fn wait_for_ready() -> bool {
    let mut ready = false;
    
    // Wait with timeout
    while not ready max 100 {
        let status = read_register(StatusReg);
        ready = TEST_BIT(status, 7) == 1;  // Ready bit is bit 7
        
        if not ready {
            DELAY_MS(10);
        }
    }
    
    ready
}

fn count_active_flags() -> u8 {
    let status = read_register(StatusReg);
    COUNT_ONES(status)
}

fn main() {
    // Initialize - disable everything
    write_register(ControlReg, 0x00);
    
    // Configure: set mode 2, enable interrupts
    set_mode(2);
    set_bit_in_reg(ControlReg, IntEnableBit);
    
    // Enable the device
    set_bit_in_reg(ControlReg, EnableBit);
    
    // Wait for ready
    let is_ready = wait_for_ready();
    
    if is_ready {
        // Start operation
        set_bit_in_reg(ControlReg, RunBit);
        
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
        clear_bit_in_reg(ControlReg, RunBit);
    }
    
    // Disable device
    clear_bit_in_reg(ControlReg, EnableBit);
}
