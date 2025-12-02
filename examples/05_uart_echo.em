// Example 5: UART Communication
// Simple serial echo with timeout

const UartNum: u8 = 0;
const BaudRate: u32 = 9600;
const MaxMessageSize: u16 = 64;

fn main() {
    // Initialize UART
    INIT(UART(UartNum));
    SET_BAUD_RATE(UART(UartNum), BaudRate);
    
    // Send startup message
    let startup: [u8; 7] = [82, 101, 97, 100, 121, 13, 10];  // "Ready\r\n"
    for i in 0 to 6 {
        WRITE(UART(UartNum), startup[i]);
    }
    
    // Echo loop - process up to 1000 messages
    repeat 1000 {
        // Wait for data with timeout
        let mut received = false;
        
        while AVAILABLE(UART(UartNum)) == 0 max 100 {
            DELAY_MS(10);  // 1 second total timeout
        }
        
        // Check if we have data
        if AVAILABLE(UART(UartNum)) > 0 {
            // Read and echo up to MaxMessageSize bytes
            for i in 0 to MaxMessageSize - 1 {
                if AVAILABLE(UART(UartNum)) > 0 {
                    let byte = READ(UART(UartNum));
                    WRITE(UART(UartNum), byte);  // Echo back
                    
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
        WRITE(UART(UartNum), goodbye[i]);
    }
}
