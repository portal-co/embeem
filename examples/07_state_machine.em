// Example 7: State Machine
// Implements a simple traffic light controller

// States
const STATE_RED: u8 = 0;
const STATE_RED_YELLOW: u8 = 1;
const STATE_GREEN: u8 = 2;
const STATE_YELLOW: u8 = 3;

// Timing (in 100ms units)
const RED_TIME: u16 = 50;        // 5 seconds
const RED_YELLOW_TIME: u16 = 10;  // 1 second
const GREEN_TIME: u16 = 40;       // 4 seconds
const YELLOW_TIME: u16 = 10;      // 1 second

// Pins
const RED_LED: u8 = 10;
const YELLOW_LED: u8 = 11;
const GREEN_LED: u8 = 12;

fn set_lights(red: u8, yellow: u8, green: u8) {
    WRITE(GPIO(RED_LED), red);
    WRITE(GPIO(YELLOW_LED), yellow);
    WRITE(GPIO(GREEN_LED), green);
}

fn get_state_duration(state: u8) -> u16 {
    if state == STATE_RED {
        RED_TIME
    } else if state == STATE_RED_YELLOW {
        RED_YELLOW_TIME
    } else if state == STATE_GREEN {
        GREEN_TIME
    } else {
        YELLOW_TIME
    }
}

fn next_state(current: u8) -> u8 {
    if current == STATE_RED {
        STATE_RED_YELLOW
    } else if current == STATE_RED_YELLOW {
        STATE_GREEN
    } else if current == STATE_GREEN {
        STATE_YELLOW
    } else {
        STATE_RED
    }
}

fn apply_state(state: u8) {
    if state == STATE_RED {
        set_lights(1, 0, 0);
    } else if state == STATE_RED_YELLOW {
        set_lights(1, 1, 0);
    } else if state == STATE_GREEN {
        set_lights(0, 0, 1);
    } else {
        set_lights(0, 1, 0);
    }
}

fn main() {
    // Initialize LED pins
    SET_MODE(GPIO(RED_LED), 1);
    SET_MODE(GPIO(YELLOW_LED), 1);
    SET_MODE(GPIO(GREEN_LED), 1);
    
    let mut state: u8 = STATE_RED;
    let mut timer: u16 = 0;
    
    // Run for 1000 cycles (each cycle is 100ms)
    // Total runtime: 100 seconds
    repeat 1000 {
        apply_state(state);
        
        timer = timer + 1;
        
        if timer >= get_state_duration(state) {
            state = next_state(state);
            timer = 0;
        }
        
        DELAY_MS(100);
    }
    
    // Turn off all lights when done
    set_lights(0, 0, 0);
}
