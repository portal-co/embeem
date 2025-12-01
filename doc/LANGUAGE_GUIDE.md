# Embeem Language Guide

**A Total Language for Embedded Systems**

## What is Embeem?

Embeem is a domain-specific programming language designed for microcontroller and embedded systems programming. The name comes from "**emb**edded" + "s**eem**"—programs that seem simple but do powerful things.

### Why Another Embedded Language?

Embeem solves a critical problem in embedded systems: **guaranteed termination**. Unlike C or Rust, every well-formed Embeem program is mathematically guaranteed to finish executing. This property, called *totality*, is essential for:

- Safety-critical systems
- Real-time constraints
- Formal verification
- Predictable worst-case execution time (WCET) analysis

## Quick Start

### Hello Blink

The classic embedded "Hello World"—blinking an LED:

```embeem
fn main() {
    GPIO_SET_MODE(13, OUTPUT);
    
    repeat 10 {
        GPIO_WRITE(13, HIGH);
        DELAY_MS(500);
        GPIO_WRITE(13, LOW);
        DELAY_MS(500);
    }
}
```

This program blinks an LED on pin 13 exactly 10 times, then exits.

### Reading a Sensor

```embeem
fn main() {
    let reading = ADC_READ(0);
    
    if reading > 512 {
        GPIO_WRITE(LED_PIN, HIGH);
    } else {
        GPIO_WRITE(LED_PIN, LOW);
    }
}
```

## Core Concepts

### Variables

Declare variables with `let`:

```embeem
let x = 42;           // Immutable, type inferred as u64
let y: u16 = 1000;    // Explicit type
let mut z = 0;        // Mutable variable
z = z + 1;            // Assignment (only for mut)
```

### Expressions

Everything in Embeem is an expression that returns a value:

```embeem
let max = if a > b { a } else { b };

let result = {
    let temp = x * 2;
    temp + 1
};  // Block expressions return their last value
```

### Operations

Embeem provides direct access to microcontroller operations:

```embeem
// Arithmetic
let sum = ADD(a, b);      // or: a + b
let diff = SUB(a, b);     // or: a - b

// Bitwise
let masked = AND(value, 0xFF);
let shifted = SHL(value, 4);

// Comparisons
let is_equal = EQ(a, b);
let is_less = LT(a, b);

// GPIO
GPIO_WRITE(pin, value);
let state = GPIO_READ(pin);

// PWM
PWM_SET_DUTY_CYCLE(channel, 128);

// Communication
UART_WRITE_BYTE(0, data);
let received = SPI_TRANSFER(0, command);
```

## Control Flow

### Conditionals

Standard if-else with mandatory braces:

```embeem
if temperature > 30 {
    FAN_ON();
} else if temperature < 20 {
    HEATER_ON();
} else {
    IDLE();
}
```

### Loops (Always Bounded!)

This is where Embeem is special. **All loops must have known bounds.**

#### Counted For Loop

```embeem
// Execute exactly 100 times
for i in 0 to 99 {
    process(i);
}

// Count down
for i in 10 downto 1 {
    countdown(i);
}
```

#### Repeat Loop

```embeem
// Execute exactly N times
repeat 50 {
    sample_sensor();
}
```

#### Bounded While Loop

```embeem
// While with maximum iterations
let mut found = false;
while not found max 1000 {
    if check_condition() {
        found = true;
    }
}
// Guaranteed to exit after at most 1000 iterations
```

### Why Bounded Loops?

Traditional `while (true)` loops can run forever. Embeem requires you to think about the maximum iterations, which:

1. **Guarantees termination** - No infinite loops possible
2. **Enables WCET analysis** - Know exactly how long code takes
3. **Prevents bugs** - Catches logic errors at compile time
4. **Suits embedded systems** - Most real loops have natural bounds

## Functions

```embeem
fn calculate_average(readings: [u16; 10]) -> u16 {
    let mut sum: u32 = 0;
    
    for i in 0 to 9 {
        sum = sum + readings[i];
    }
    
    sum / 10
}

fn main() {
    let data: [u16; 10] = [100, 200, 150, 175, 225, 300, 275, 250, 180, 195];
    let avg = calculate_average(data);
    
    if avg > 200 {
        GPIO_WRITE(WARNING_LED, HIGH);
    }
}
```

**Note**: Functions cannot be recursive. This is essential for totality.

## Types

### Integers

| Type | Range |
|------|-------|
| `u8` | 0 to 255 |
| `u16` | 0 to 65,535 |
| `u32` | 0 to 4,294,967,295 |
| `u64` | 0 to 18,446,744,073,709,551,615 |
| `i8` | -128 to 127 |
| `i16` | -32,768 to 32,767 |
| `i32` | -2,147,483,648 to 2,147,483,647 |
| `i64` | -(2^63) to (2^63 - 1) |

### Floating Point

```embeem
let pi: f32 = 3.14159;
let precise: f64 = 3.141592653589793;

let result = FADD(a, b);  // Floating point add
let root = FSQRT(value);  // Square root
```

### Boolean

```embeem
let flag: bool = true;
let check = a > b and c < d;
```

### Arrays

```embeem
let buffer: [u8; 64] = [0; 64];  // 64 bytes, all zero
let pins: [u8; 4] = [2, 3, 4, 5];
```

## Common Patterns

### Debounce Button

```embeem
fn debounced_read(pin: u8) -> bool {
    let mut count = 0;
    
    for i in 0 to 9 {
        if GPIO_READ(pin) == HIGH {
            count = count + 1;
        }
        DELAY_MS(1);
    }
    
    count > 5
}
```

### PWM Fade

```embeem
fn fade_led(channel: u8) {
    // Fade up
    for brightness in 0 to 255 {
        PWM_SET_DUTY_CYCLE(channel, brightness);
        DELAY_MS(10);
    }
    
    // Fade down
    for brightness in 255 downto 0 {
        PWM_SET_DUTY_CYCLE(channel, brightness);
        DELAY_MS(10);
    }
}
```

### State Machine

```embeem
const STATE_IDLE: u8 = 0;
const STATE_RUNNING: u8 = 1;
const STATE_ERROR: u8 = 2;

fn process_state(state: u8, input: u8) -> u8 {
    if state == STATE_IDLE {
        if input == START_CMD {
            STATE_RUNNING
        } else {
            STATE_IDLE
        }
    } else if state == STATE_RUNNING {
        if input == STOP_CMD {
            STATE_IDLE
        } else if input == ERROR_CMD {
            STATE_ERROR
        } else {
            STATE_RUNNING
        }
    } else {
        if input == RESET_CMD {
            STATE_IDLE
        } else {
            STATE_ERROR
        }
    }
}

fn main() {
    let mut state: u8 = STATE_IDLE;
    
    repeat 1000 {
        let input = UART_READ_BYTE(0);
        state = process_state(state, input);
        DELAY_MS(10);
    }
}
```

### Sensor Averaging

```embeem
fn read_averaged(channel: u8, samples: u8) -> u16 {
    let mut sum: u32 = 0;
    
    for i in 0 to 15 {  // Max 16 samples
        if i < samples {
            sum = sum + ADC_READ(channel);
            DELAY_US(100);
        }
    }
    
    (sum / samples) as u16
}
```

## Tips and Best Practices

### 1. Think About Bounds

Before writing a loop, ask: "What's the maximum number of iterations?"

```embeem
// Bad thinking: "loop until data arrives"
// Good thinking: "loop at most 1000 times waiting for data"

while UART_AVAILABLE(0) == 0 max 1000 {
    DELAY_MS(1);
}
```

### 2. Use Constants

```embeem
const BUFFER_SIZE: u16 = 256;
const TIMEOUT_MS: u16 = 5000;
const LED_PIN: u8 = 13;

fn main() {
    for i in 0 to BUFFER_SIZE - 1 {
        // ...
    }
}
```

### 3. Prefer Expressions

```embeem
// Instead of:
let result: u16;
if condition {
    result = value_a;
} else {
    result = value_b;
}

// Write:
let result = if condition { value_a } else { value_b };
```

### 4. Keep Functions Small

Since there's no recursion, complex algorithms need careful structuring:

```embeem
fn step1(input: u32) -> u32 { /* ... */ }
fn step2(input: u32) -> u32 { /* ... */ }
fn step3(input: u32) -> u32 { /* ... */ }

fn algorithm(input: u32) -> u32 {
    let r1 = step1(input);
    let r2 = step2(r1);
    step3(r2)
}
```

## Error Handling

Embeem uses result values rather than exceptions:

```embeem
const OK: u8 = 0;
const ERR_TIMEOUT: u8 = 1;
const ERR_INVALID: u8 = 2;

fn read_with_timeout(channel: u8) -> (u8, u16) {
    let mut attempts = 0;
    
    while UART_AVAILABLE(channel) == 0 max 100 {
        attempts = attempts + 1;
        DELAY_MS(10);
    }
    
    if attempts >= 100 {
        (ERR_TIMEOUT, 0)
    } else {
        (OK, UART_READ_BYTE(channel))
    }
}
```

## Comparison with Other Languages

| Feature | C | Rust | Embeem |
|---------|---|------|--------|
| Infinite loops | ✓ | ✓ | ✗ |
| Recursion | ✓ | ✓ | ✗ |
| Guaranteed termination | ✗ | ✗ | ✓ |
| WCET analyzable | Partial | Partial | ✓ |
| Memory safe | ✗ | ✓ | ✓ |
| No undefined behavior | ✗ | Partial | ✓ |

## Next Steps

- Read the [Formal Specification](SPECIFICATION.md) for complete syntax
- See [Example Programs](examples/) for more patterns
- Check [Totality Proof](TOTALITY.md) for the termination guarantee
- Learn about [Desugaring](DESUGARING.md) to understand compilation
