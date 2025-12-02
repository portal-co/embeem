// Example 7: State Machine
// Implements a simple traffic light controller

// States
const StateRed: u8 = 0;
const StateRedYellow: u8 = 1;
const StateGreen: u8 = 2;
const StateYellow: u8 = 3;

// Timing (in 100ms units)
const RedTime: u16 = 50;        // 5 seconds
const RedYellowTime: u16 = 10;  // 1 second
const GreenTime: u16 = 40;       // 4 seconds
const YellowTime: u16 = 10;      // 1 second

// Pins
const RedLed: u8 = 10;
const YellowLed: u8 = 11;
const GreenLed: u8 = 12;

fn set_lights(red: u8, yellow: u8, green: u8) {
    WRITE(GPIO(RedLed), red);
    WRITE(GPIO(YellowLed), yellow);
    WRITE(GPIO(GreenLed), green);
}

fn get_state_duration(state: u8) -> u16 {
    if state == StateRed {
        RedTime
    } else if state == StateRedYellow {
        RedYellowTime
    } else if state == StateGreen {
        GreenTime
    } else {
        YellowTime
    }
}

fn next_state(current: u8) -> u8 {
    if current == StateRed {
        StateRedYellow
    } else if current == StateRedYellow {
        StateGreen
    } else if current == StateGreen {
        StateYellow
    } else {
        StateRed
    }
}

fn apply_state(state: u8) {
    if state == StateRed {
        set_lights(1, 0, 0);
    } else if state == StateRedYellow {
        set_lights(1, 1, 0);
    } else if state == StateGreen {
        set_lights(0, 0, 1);
    } else {
        set_lights(0, 1, 0);
    }
}

fn main() {
    // Initialize LED pins
    SET_MODE(GPIO(RedLed), 1);
    SET_MODE(GPIO(YellowLed), 1);
    SET_MODE(GPIO(GreenLed), 1);
    
    let mut state: u8 = StateRed;
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
