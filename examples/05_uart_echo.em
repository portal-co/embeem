// Example 5: UART Communication
// Simple serial echo with timeout

const UART_NUM: u8 = 0;
const BAUD_RATE: u32 = 9600;
const MAX_MESSAGE_SIZE: u16 = 64;

fn main() {
    // Initialize UART
    INIT(UART(UART_NUM));
    SET_BAUD_RATE(UART(UART_NUM), BAUD_RATE);
    
    // Send startup message
    let startup: [u8; 7] = [82, 101, 97, 100, 121, 13, 10];  // "Ready\r\n"
    for i in 0 to 6 {
        WRITE(UART(UART_NUM), startup[i]);
    }
    
    // Echo loop - process up to 1000 messages
    repeat 1000 {
        // Wait for data with timeout
        let mut received = false;
        
        while AVAILABLE(UART(UART_NUM)) == 0 max 100 {
            DELAY_MS(10);  // 1 second total timeout
        }
        
        // Check if we have data
        if AVAILABLE(UART(UART_NUM)) > 0 {
            // Read and echo up to MAX_MESSAGE_SIZE bytes
            for i in 0 to MAX_MESSAGE_SIZE - 1 {
                if AVAILABLE(UART(UART_NUM)) > 0 {
                    let byte = READ(UART(UART_NUM));
                    WRITE(UART(UART_NUM), byte);  // Echo back
                    
                    // Check for newline (end of message)
                    if byte == 10 {
                        // Break would be nice, but we use conditional execution
                    }
                }
            }
        }
        
        DELAY_MS(10);
    }
    
    // Send goodbye message
    let goodbye: [u8; 6] = [66, 121, 101, 33, 13, 10];  // "Bye!\r\n"
    for i in 0 to 5 {
        WRITE(UART(UART_NUM), goodbye[i]);
    }
}
