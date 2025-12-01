# Embeem Language Specification

**Version:** 0.1.0  
**Status:** Draft  
**Last Updated:** 2025-12-01

## Table of Contents

1. [Overview](#1-overview)
2. [Lexical Structure](#2-lexical-structure)
3. [Types](#3-types)
4. [Expressions](#4-expressions)
5. [Statements](#5-statements)
6. [Control Flow](#6-control-flow)
7. [Operations](#7-operations)
8. [Program Structure](#8-program-structure)
9. [Semantics](#9-semantics)
10. [Grammar](#10-grammar)

---

## 1. Overview

Embeem is a **total** embedded systems programming language designed for microcontroller applications. By "total," we mean that every well-formed Embeem program is guaranteed to terminate—there are no infinite loops or unbounded recursion.

### 1.1 Design Goals

- **Totality**: All programs terminate
- **Simplicity**: Minimal syntax for embedded applications
- **Safety**: No undefined behavior
- **Interoperability**: Direct mapping to `emback::ops::Expr` type

### 1.2 Key Constraints

- Loops have statically-bounded iteration counts
- No general recursion
- All operations are pure (side effects via explicit IO operations)

---

## 2. Lexical Structure

### 2.1 Identifiers

```
IDENTIFIER  ::= [a-zA-Z_][a-zA-Z0-9_]*
```

Reserved keywords cannot be used as identifiers.

#### 2.1.1 Identifier Naming Conventions

**UPPER_SNAKE_CASE is reserved exclusively for operations.**

Identifiers matching the pattern `[A-Z][A-Z0-9_]*` (starting with uppercase letter, containing only uppercase letters, digits, and underscores) are reserved for the operation namespace and **cannot** be used as:
- Variable names
- Function names  
- Constant names
- Type names
- Parameter names

This ensures:
1. Future operations can be added without breaking existing programs
2. Operations are visually distinct from user-defined identifiers
3. The parser can unambiguously distinguish operations from function calls

**Valid user identifiers:**
```embeem
let x = 1;              // lowercase
let myVariable = 2;     // camelCase
let my_variable = 3;    // snake_case
let MyType = 4;         // PascalCase
let _private = 5;       // leading underscore
let x1 = 6;             // with digits
```

**Invalid user identifiers (reserved for operations):**
```embeem
let GPIO = 1;           // ERROR: UPPER_SNAKE_CASE
let MY_CONST = 2;       // ERROR: UPPER_SNAKE_CASE  
let ADC_CHANNEL = 3;    // ERROR: UPPER_SNAKE_CASE
let X = 4;              // ERROR: single uppercase letter
```

### 2.2 Keywords

```
let     if      else    for     in      fn      return
true    false   to      downto  and     or      not
```

### 2.3 Literals

```
INTEGER     ::= [0-9]+ | 0x[0-9a-fA-F]+ | 0b[01]+
FLOAT       ::= [0-9]+\.[0-9]+ | [0-9]+\.[0-9]+[eE][+-]?[0-9]+
BOOLEAN     ::= 'true' | 'false'
```

### 2.4 Operators

```
ARITH_OP    ::= '+' | '-' | '*' | '/' | '%'
LOGIC_OP    ::= '&' | '|' | '^' | '~' | '<<' | '>>' | '>>>'
COMP_OP     ::= '==' | '!=' | '<' | '<=' | '>' | '>='
ASSIGN_OP   ::= '='
```

### 2.5 Delimiters

```
DELIMITER   ::= '(' | ')' | '{' | '}' | '[' | ']' | ',' | ';' | ':' | '->'
```

### 2.6 Comments

```
LINE_COMMENT   ::= '//' [^\n]*
BLOCK_COMMENT  ::= '/*' .* '*/'
```

---

## 3. Types

### 3.1 Primitive Types

| Type   | Description                        | Size    |
|--------|------------------------------------|---------|
| `u8`   | Unsigned 8-bit integer             | 1 byte  |
| `u16`  | Unsigned 16-bit integer            | 2 bytes |
| `u32`  | Unsigned 32-bit integer            | 4 bytes |
| `u64`  | Unsigned 64-bit integer            | 8 bytes |
| `i8`   | Signed 8-bit integer               | 1 byte  |
| `i16`  | Signed 16-bit integer              | 2 bytes |
| `i32`  | Signed 32-bit integer              | 4 bytes |
| `i64`  | Signed 64-bit integer              | 8 bytes |
| `f32`  | 32-bit floating point              | 4 bytes |
| `f64`  | 64-bit floating point              | 8 bytes |
| `bool` | Boolean                            | 1 byte  |

### 3.2 Compound Types

```
ARRAY_TYPE  ::= '[' TYPE ';' INTEGER ']'    // Fixed-size array
TUPLE_TYPE  ::= '(' TYPE (',' TYPE)* ')'    // Tuple
```

### 3.3 Type Inference

Types can be inferred from context. When explicit types are needed, use the `:` annotation:

```
let x: u32 = 42;
let y = 42;  // Inferred as u64 (default integer type)
```

---

## 4. Expressions

### 4.1 Primary Expressions

```
PRIMARY ::= IDENTIFIER
          | LITERAL
          | '(' EXPR ')'
          | OPERATION_CALL
```

### 4.2 Binary Expressions

```
BINARY_EXPR ::= EXPR BINARY_OP EXPR

BINARY_OP   ::= '+' | '-' | '*' | '/' | '%'      // Arithmetic
              | '&' | '|' | '^' | '<<' | '>>'    // Bitwise
              | '==' | '!=' | '<' | '<=' | '>' | '>=' // Comparison
              | 'and' | 'or'                      // Logical
```

### 4.3 Unary Expressions

```
UNARY_EXPR ::= UNARY_OP EXPR

UNARY_OP   ::= '-' | '~' | 'not'
```

### 4.4 Conditional Expressions

```
COND_EXPR ::= 'if' EXPR '{' EXPR '}' 'else' '{' EXPR '}'
```

**Semantics**: Both branches must have the same type. The condition must be of type `bool`.

### 4.5 Operation Expressions

Embeem supports all operations from the emback operation set:

```
OP_EXPR ::= OP_NAME '(' EXPR_LIST? ')'
```

See [Section 7](#7-operations) for the complete list.

---

## 5. Statements

### 5.1 Let Statement

```
LET_STMT ::= 'let' IDENTIFIER (':' TYPE)? '=' EXPR ';'
```

### 5.2 Assignment Statement

```
ASSIGN_STMT ::= IDENTIFIER '=' EXPR ';'
```

**Note**: Only mutable variables (declared with `let mut`) can be assigned.

### 5.3 Expression Statement

```
EXPR_STMT ::= EXPR ';'
```

### 5.4 Block Statement

```
BLOCK_STMT ::= '{' STMT* EXPR? '}'
```

A block evaluates to the final expression (if present) or unit.

---

## 6. Control Flow

### 6.1 Conditionals

```
IF_STMT ::= 'if' EXPR BLOCK ('else' BLOCK)?
          | 'if' EXPR BLOCK ('else' IF_STMT)?
```

### 6.2 Bounded Loops

**Critical for Totality**: All loops in Embeem have statically-known bounds.

#### 6.2.1 Counted For Loop

```
FOR_LOOP ::= 'for' IDENTIFIER 'in' RANGE BLOCK

RANGE    ::= CONST_EXPR 'to' CONST_EXPR
           | CONST_EXPR 'downto' CONST_EXPR
```

**Constraints**:
- Range bounds must be compile-time constants or variables with known bounds
- Loop variable is immutable within the loop body

**Examples**:
```embeem
// Count from 0 to 9 (inclusive)
for i in 0 to 9 {
    // i is available here
}

// Count down from 10 to 1
for i in 10 downto 1 {
    // i decrements each iteration
}
```

#### 6.2.2 Repeat Loop

```
REPEAT_LOOP ::= 'repeat' CONST_EXPR BLOCK
```

Executes the block exactly `CONST_EXPR` times.

**Example**:
```embeem
repeat 5 {
    GPIO_TOGGLE(pin);
    DELAY_MS(500);
}
```

#### 6.2.3 Bounded While Loop

```
BOUNDED_WHILE ::= 'while' EXPR 'max' CONST_EXPR BLOCK
```

Executes while condition is true, with a maximum iteration count.

**Semantics**: If the maximum is reached, the loop exits regardless of condition.

**Example**:
```embeem
let mut found = false;
let mut i = 0;
while not found max 100 {
    if ADC_READ(channel) > threshold {
        found = true;
    }
    i = i + 1;
}
```

---

## 7. Operations

Embeem provides direct access to all emback operations. Operations are called with function syntax.

### 7.1 Arithmetic Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `ADD`     | `ADD(a, b)` | Addition |
| `SUB`     | `SUB(a, b)` | Subtraction |
| `MUL`     | `MUL(a, b)` | Multiplication |
| `DIV`     | `DIV(a, b)` | Division |
| `MOD`     | `MOD(a, b)` | Modulo |
| `INC`     | `INC(x)` | Increment |
| `DEC`     | `DEC(x)` | Decrement |
| `NEG`     | `NEG(x)` | Negate |
| `ABS`     | `ABS(x)` | Absolute value |

### 7.2 Logical Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `AND`     | `AND(a, b)` | Bitwise AND |
| `OR`      | `OR(a, b)` | Bitwise OR |
| `XOR`     | `XOR(a, b)` | Bitwise XOR |
| `NOT`     | `NOT(x)` | Bitwise NOT |
| `SHL`     | `SHL(x, n)` | Shift left |
| `SHR`     | `SHR(x, n)` | Shift right (logical) |
| `SAR`     | `SAR(x, n)` | Shift arithmetic right |
| `ROL`     | `ROL(x, n)` | Rotate left |
| `ROR`     | `ROR(x, n)` | Rotate right |

### 7.3 Comparison Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `CMP`     | `CMP(a, b)` | Compare |
| `TEST`    | `TEST(x, mask)` | Test bits |
| `EQ`      | `EQ(a, b)` | Equal |
| `NE`      | `NE(a, b)` | Not equal |
| `LT`      | `LT(a, b)` | Less than |
| `LE`      | `LE(a, b)` | Less or equal |
| `GT`      | `GT(a, b)` | Greater than |
| `GE`      | `GE(a, b)` | Greater or equal |

### 7.4 Bit Manipulation Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `SET_BIT` | `SET_BIT(x, n)` | Set bit n |
| `CLEAR_BIT` | `CLEAR_BIT(x, n)` | Clear bit n |
| `TOGGLE_BIT` | `TOGGLE_BIT(x, n)` | Toggle bit n |
| `TEST_BIT` | `TEST_BIT(x, n)` | Test bit n |
| `COUNT_ONES` | `COUNT_ONES(x)` | Population count |
| `COUNT_ZEROS` | `COUNT_ZEROS(x)` | Count zero bits |
| `FIND_FIRST_SET` | `FIND_FIRST_SET(x)` | Find first set bit |
| `FIND_FIRST_ZERO` | `FIND_FIRST_ZERO(x)` | Find first zero bit |

### 7.5 Floating Point Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `FADD`    | `FADD(a, b)` | FP addition |
| `FSUB`    | `FSUB(a, b)` | FP subtraction |
| `FMUL`    | `FMUL(a, b)` | FP multiplication |
| `FDIV`    | `FDIV(a, b)` | FP division |
| `FSQRT`   | `FSQRT(x)` | FP square root |
| `FABS`    | `FABS(x)` | FP absolute value |
| `FCMP`    | `FCMP(a, b)` | FP compare |

### 7.6 GPIO Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `GPIO_READ` | `GPIO_READ(pin)` | Read pin state |
| `GPIO_WRITE` | `GPIO_WRITE(pin, value)` | Write pin state |
| `GPIO_TOGGLE` | `GPIO_TOGGLE(pin)` | Toggle pin |
| `GPIO_SET_MODE` | `GPIO_SET_MODE(pin, mode)` | Set pin mode |
| `GPIO_READ_PORT` | `GPIO_READ_PORT(port)` | Read port |
| `GPIO_WRITE_PORT` | `GPIO_WRITE_PORT(port, value)` | Write port |

### 7.7 Analog Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `ADC_READ` | `ADC_READ(channel)` | Read ADC |
| `ADC_START_CONVERSION` | `ADC_START_CONVERSION(channel)` | Start conversion |
| `ADC_READ_MULTI` | `ADC_READ_MULTI(channels)` | Read multiple channels |
| `DAC_WRITE` | `DAC_WRITE(channel, value)` | Write DAC |
| `ADC_SET_RESOLUTION` | `ADC_SET_RESOLUTION(bits)` | Set resolution |
| `ADC_SET_REFERENCE` | `ADC_SET_REFERENCE(ref)` | Set voltage reference |

### 7.8 PWM Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `PWM_START` | `PWM_START(channel)` | Start PWM |
| `PWM_STOP` | `PWM_STOP(channel)` | Stop PWM |
| `PWM_SET_DUTY_CYCLE` | `PWM_SET_DUTY_CYCLE(channel, duty)` | Set duty cycle |
| `PWM_SET_FREQUENCY` | `PWM_SET_FREQUENCY(channel, freq)` | Set frequency |
| `PWM_SET_PULSE_WIDTH` | `PWM_SET_PULSE_WIDTH(channel, width)` | Set pulse width |

### 7.9 Timer Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `TIMER_START` | `TIMER_START(timer)` | Start timer |
| `TIMER_STOP` | `TIMER_STOP(timer)` | Stop timer |
| `TIMER_RESET` | `TIMER_RESET(timer)` | Reset timer |
| `TIMER_READ` | `TIMER_READ(timer)` | Read timer value |
| `TIMER_SET_PERIOD` | `TIMER_SET_PERIOD(timer, period)` | Set period |
| `TIMER_SET_COMPARE` | `TIMER_SET_COMPARE(timer, value)` | Set compare value |
| `GET_MILLIS` | `GET_MILLIS()` | Get milliseconds |
| `GET_MICROS` | `GET_MICROS()` | Get microseconds |
| `DELAY_MS` | `DELAY_MS(ms)` | Delay milliseconds |
| `DELAY_US` | `DELAY_US(us)` | Delay microseconds |

### 7.10 UART Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `UART_INIT` | `UART_INIT(uart)` | Initialize UART |
| `UART_WRITE_BYTE` | `UART_WRITE_BYTE(uart, byte)` | Write byte |
| `UART_WRITE_BUFFER` | `UART_WRITE_BUFFER(uart, buf)` | Write buffer |
| `UART_READ_BYTE` | `UART_READ_BYTE(uart)` | Read byte |
| `UART_READ_BUFFER` | `UART_READ_BUFFER(uart, buf)` | Read buffer |
| `UART_AVAILABLE` | `UART_AVAILABLE(uart)` | Check available |
| `UART_FLUSH` | `UART_FLUSH(uart)` | Flush buffer |
| `UART_SET_BAUD_RATE` | `UART_SET_BAUD_RATE(uart, baud)` | Set baud rate |

### 7.11 SPI Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `SPI_INIT` | `SPI_INIT(spi)` | Initialize SPI |
| `SPI_TRANSFER` | `SPI_TRANSFER(spi, byte)` | Transfer byte |
| `SPI_TRANSFER_BUFFER` | `SPI_TRANSFER_BUFFER(spi, buf)` | Transfer buffer |
| `SPI_SET_MODE` | `SPI_SET_MODE(spi, mode)` | Set mode |
| `SPI_SET_CLOCK` | `SPI_SET_CLOCK(spi, clock)` | Set clock |
| `SPI_SET_BIT_ORDER` | `SPI_SET_BIT_ORDER(spi, order)` | Set bit order |
| `SPI_BEGIN_TRANSACTION` | `SPI_BEGIN_TRANSACTION(spi)` | Begin transaction |
| `SPI_END_TRANSACTION` | `SPI_END_TRANSACTION(spi)` | End transaction |

### 7.12 I2C Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| `I2C_INIT` | `I2C_INIT(i2c)` | Initialize I2C |
| `I2C_START` | `I2C_START(i2c)` | Send start condition |
| `I2C_STOP` | `I2C_STOP(i2c)` | Send stop condition |
| `I2C_WRITE` | `I2C_WRITE(i2c, data)` | Write data |
| `I2C_READ` | `I2C_READ(i2c)` | Read data |
| `I2C_WRITE_TO` | `I2C_WRITE_TO(i2c, addr, data)` | Write to address |
| `I2C_READ_FROM` | `I2C_READ_FROM(i2c, addr)` | Read from address |
| `I2C_SET_CLOCK` | `I2C_SET_CLOCK(i2c, clock)` | Set clock |
| `I2C_SCAN` | `I2C_SCAN(i2c)` | Scan bus |

### 7.13 Additional IO Operations

See the complete emback operations documentation for:
- CAN Bus Operations
- USB Operations
- Watchdog Timer Operations
- DMA Operations
- EEPROM/Flash Operations
- Power Management Operations
- RTC Operations

### 7.14 Virtual Function Call Syntax

Embeem supports a "virtual function call" syntax that allows expressing operations in a more object-oriented style. This syntax separates the **target** (the peripheral or resource) from the **operation** being performed.

#### 7.14.1 Syntax

```
VIRTUAL_CALL ::= OP_NAME '(' TARGET '(' TARGET_ARGS ')' (',' EXPR)* ')'
TARGET       ::= 'GPIO' | 'ADC' | 'DAC' | 'PWM' | 'TIMER' | 'UART' | 'SPI' | 'I2C' | 'CAN' | 'USB' | 'DMA' | 'WDT' | 'EEPROM' | 'FLASH' | 'RTC'
```

#### 7.14.2 Target Types

| Target | Description | Target Arguments |
|--------|-------------|------------------|
| `GPIO` | General Purpose I/O | `GPIO(pin)` |
| `ADC` | Analog-to-Digital Converter | `ADC(channel)` |
| `DAC` | Digital-to-Analog Converter | `DAC(channel)` |
| `PWM` | Pulse Width Modulation | `PWM(channel)` |
| `TIMER` | Hardware Timer | `TIMER(timer_num)` |
| `UART` | Serial Communication | `UART(uart_num)` |
| `SPI` | Serial Peripheral Interface | `SPI(spi_num)` |
| `I2C` | Inter-Integrated Circuit | `I2C(i2c_num)` |
| `CAN` | Controller Area Network | `CAN(can_num)` |
| `USB` | Universal Serial Bus | `USB(usb_num)` |
| `DMA` | Direct Memory Access | `DMA(dma_num)` |
| `WDT` | Watchdog Timer | `WDT()` |
| `EEPROM` | EEPROM Memory | `EEPROM(addr)` |
| `FLASH` | Flash Memory | `FLASH(addr)` |
| `RTC` | Real-Time Clock | `RTC(rtc_num)` |

#### 7.14.3 Virtual Operations

| Operation | Description | Example |
|-----------|-------------|---------|
| `READ` | Read from target | `READ(GPIO(13))`, `READ(ADC(0))` |
| `WRITE` | Write to target | `WRITE(GPIO(13), 1)`, `WRITE(DAC(0), 128)` |
| `INIT` | Initialize target | `INIT(UART(0))`, `INIT(I2C(0))` |
| `START` | Start target | `START(PWM(0))`, `START(TIMER(0))` |
| `STOP` | Stop target | `STOP(PWM(0))`, `STOP(TIMER(0))` |
| `RESET` | Reset target | `RESET(TIMER(0))`, `RESET(WDT())` |
| `TOGGLE` | Toggle target state | `TOGGLE(GPIO(13))` |
| `SET_MODE` | Set operation mode | `SET_MODE(GPIO(13), 1)` |
| `SET_FREQUENCY` | Set frequency | `SET_FREQUENCY(PWM(0), 1000)` |
| `SET_DUTY_CYCLE` | Set PWM duty cycle | `SET_DUTY_CYCLE(PWM(0), 128)` |
| `SET_CLOCK` | Set clock speed | `SET_CLOCK(I2C(0), 100000)` |
| `SET_BAUD_RATE` | Set baud rate | `SET_BAUD_RATE(UART(0), 9600)` |
| `TRANSFER` | Transfer data | `TRANSFER(SPI(0), data)` |
| `AVAILABLE` | Check data available | `AVAILABLE(UART(0))` |
| `FLUSH` | Flush buffers | `FLUSH(UART(0))` |
| `ENABLE` | Enable target | `ENABLE(WDT())` |
| `DISABLE` | Disable target | `DISABLE(WDT())` |

#### 7.14.4 Equivalence to Traditional Syntax

The virtual function call syntax is syntactic sugar that desugars to the traditional operation syntax:

| Virtual Syntax | Traditional Syntax |
|----------------|-------------------|
| `READ(GPIO(13))` | `GPIO_READ(13)` |
| `WRITE(GPIO(13), 1)` | `GPIO_WRITE(13, 1)` |
| `TOGGLE(GPIO(13))` | `GPIO_TOGGLE(13)` |
| `SET_MODE(GPIO(13), 1)` | `GPIO_SET_MODE(13, 1)` |
| `READ(ADC(0))` | `ADC_READ(0)` |
| `WRITE(DAC(0), 128)` | `DAC_WRITE(0, 128)` |
| `START(PWM(0))` | `PWM_START(0)` |
| `STOP(PWM(0))` | `PWM_STOP(0)` |
| `SET_DUTY_CYCLE(PWM(0), 128)` | `PWM_SET_DUTY_CYCLE(0, 128)` |
| `SET_FREQUENCY(PWM(0), 1000)` | `PWM_SET_FREQUENCY(0, 1000)` |
| `START(TIMER(0))` | `TIMER_START(0)` |
| `STOP(TIMER(0))` | `TIMER_STOP(0)` |
| `RESET(TIMER(0))` | `TIMER_RESET(0)` |
| `READ(TIMER(0))` | `TIMER_READ(0)` |
| `INIT(UART(0))` | `UART_INIT(0)` |
| `WRITE(UART(0), byte)` | `UART_WRITE_BYTE(0, byte)` |
| `READ(UART(0))` | `UART_READ_BYTE(0)` |
| `AVAILABLE(UART(0))` | `UART_AVAILABLE(0)` |
| `FLUSH(UART(0))` | `UART_FLUSH(0)` |
| `SET_BAUD_RATE(UART(0), 9600)` | `UART_SET_BAUD_RATE(0, 9600)` |
| `INIT(SPI(0))` | `SPI_INIT(0)` |
| `TRANSFER(SPI(0), data)` | `SPI_TRANSFER(0, data)` |
| `INIT(I2C(0))` | `I2C_INIT(0)` |
| `START(I2C(0))` | `I2C_START(0)` |
| `STOP(I2C(0))` | `I2C_STOP(0)` |
| `SET_CLOCK(I2C(0), 100000)` | `I2C_SET_CLOCK(0, 100000)` |
| `ENABLE(WDT())` | `WDT_ENABLE()` |
| `DISABLE(WDT())` | `WDT_DISABLE()` |
| `RESET(WDT())` | `WDT_RESET()` |

#### 7.14.5 Benefits

1. **Readability**: The target is clearly separated from the operation
2. **Consistency**: Same operation name (`READ`, `WRITE`) works across different peripherals
3. **Discoverability**: Easier to find related operations for a peripheral
4. **Extensibility**: New peripherals can reuse standard operation names

**Example Program:**
```embeem
fn main() {
    // Configure pin as output
    SET_MODE(GPIO(13), 1);
    
    // Blink 10 times
    repeat 10 {
        WRITE(GPIO(13), 1);
        DELAY_MS(500);
        WRITE(GPIO(13), 0);
        DELAY_MS(500);
    }
}
```

---

## 8. Program Structure

### 8.1 Program

```
PROGRAM ::= ITEM*

ITEM    ::= FUNCTION
          | CONST_DECL
```

### 8.2 Functions

```
FUNCTION ::= 'fn' IDENTIFIER '(' PARAM_LIST? ')' ('->' TYPE)? BLOCK

PARAM_LIST ::= PARAM (',' PARAM)*
PARAM      ::= IDENTIFIER ':' TYPE
```

### 8.3 Constants

```
CONST_DECL ::= 'const' IDENTIFIER ':' TYPE '=' CONST_EXPR ';'
```

### 8.4 Entry Point

Every Embeem program must have a `main` function:

```embeem
fn main() {
    // Program entry point
}
```

---

## 9. Semantics

### 9.1 Evaluation Order

- Expressions are evaluated left-to-right
- Function arguments are evaluated left-to-right
- Short-circuit evaluation for `and` and `or`

### 9.2 Integer Overflow

Integer operations wrap on overflow (modular arithmetic).

### 9.3 Division by Zero

Division or modulo by zero results in zero (defined behavior).

### 9.4 Memory Model

- All variables have value semantics
- No pointers or references
- Arrays are passed by value (copied)

---

## 10. Grammar

### 10.1 Complete EBNF Grammar

```ebnf
(* Program structure *)
program     = { item } ;
item        = function | const_decl ;

(* Declarations *)
function    = "fn" IDENT "(" [ param_list ] ")" [ "->" type ] block ;
param_list  = param { "," param } ;
param       = IDENT ":" type ;
const_decl  = "const" IDENT ":" type "=" const_expr ";" ;

(* Types *)
type        = prim_type | array_type | tuple_type ;
prim_type   = "u8" | "u16" | "u32" | "u64"
            | "i8" | "i16" | "i32" | "i64"
            | "f32" | "f64" | "bool" ;
array_type  = "[" type ";" INTEGER "]" ;
tuple_type  = "(" type { "," type } ")" ;

(* Statements *)
block       = "{" { statement } [ expr ] "}" ;
statement   = let_stmt | assign_stmt | expr_stmt | if_stmt | for_stmt | repeat_stmt | while_stmt ;
let_stmt    = "let" [ "mut" ] IDENT [ ":" type ] "=" expr ";" ;
assign_stmt = IDENT "=" expr ";" ;
expr_stmt   = expr ";" ;
if_stmt     = "if" expr block [ "else" ( block | if_stmt ) ] ;
for_stmt    = "for" IDENT "in" range block ;
repeat_stmt = "repeat" const_expr block ;
while_stmt  = "while" expr "max" const_expr block ;
range       = const_expr ( "to" | "downto" ) const_expr ;

(* Expressions *)
expr        = or_expr ;
or_expr     = and_expr { "or" and_expr } ;
and_expr    = comp_expr { "and" comp_expr } ;
comp_expr   = bitor_expr { comp_op bitor_expr } ;
bitor_expr  = xor_expr { "|" xor_expr } ;
xor_expr    = bitand_expr { "^" bitand_expr } ;
bitand_expr = shift_expr { "&" shift_expr } ;
shift_expr  = add_expr { shift_op add_expr } ;
add_expr    = mul_expr { add_op mul_expr } ;
mul_expr    = unary_expr { mul_op unary_expr } ;
unary_expr  = unary_op unary_expr | primary ;
primary     = IDENT | literal | "(" expr ")" | op_call | virtual_call | if_expr | block ;
if_expr     = "if" expr block "else" block ;
op_call     = OP_NAME "(" [ expr { "," expr } ] ")" ;
virtual_call = VIRTUAL_OP "(" TARGET "(" [ expr { "," expr } ] ")" { "," expr } ")" ;

(* Virtual function call components *)
VIRTUAL_OP  = "READ" | "WRITE" | "INIT" | "START" | "STOP" | "RESET" | "TOGGLE"
            | "SET_MODE" | "SET_FREQUENCY" | "SET_DUTY_CYCLE" | "SET_CLOCK"
            | "SET_BAUD_RATE" | "TRANSFER" | "AVAILABLE" | "FLUSH"
            | "ENABLE" | "DISABLE" | "SET_TIMEOUT" | "SET_PERIOD" | "SET_COMPARE"
            | "SET_RESOLUTION" | "SET_REFERENCE" | "SET_PULSE_WIDTH" ;
TARGET      = "GPIO" | "ADC" | "DAC" | "PWM" | "TIMER" | "UART" | "SPI" | "I2C"
            | "CAN" | "USB" | "DMA" | "WDT" | "EEPROM" | "FLASH" | "RTC" ;

(* Operators *)
comp_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" ;
shift_op    = "<<" | ">>" | ">>>" ;
add_op      = "+" | "-" ;
mul_op      = "*" | "/" | "%" ;
unary_op    = "-" | "~" | "not" ;

(* Literals *)
literal     = INTEGER | FLOAT | BOOLEAN ;
const_expr  = expr ;  (* Must be evaluable at compile time *)

(* Tokens *)
IDENT       = ? [a-zA-Z_][a-zA-Z0-9_]* except keywords ? ;
INTEGER     = ? decimal, hex, or binary integer literal ? ;
FLOAT       = ? floating point literal ? ;
BOOLEAN     = "true" | "false" ;
OP_NAME     = ? Any operation name from Section 7 ? ;
```

---

## Appendix A: Reserved Words

```
and     bool    const   downto  else    f32     f64     false
fn      for     i8      i16     i32     i64     if      in
let     max     mut     not     or      repeat  return  to
true    u8      u16     u32     u64     while
```

## Appendix B: Operation Names

All operation names from emback are reserved:

```
ADD SUB MUL DIV MOD INC DEC NEG ABS
AND OR XOR NOT SHL SHR SAR ROL ROR
CMP TEST EQ NE LT LE GT GE
CALL RET NOP HALT SLEEP
LOAD STORE MOV PUSH POP SWAP MEMCPY MEMSET
SET_BIT CLEAR_BIT TOGGLE_BIT TEST_BIT COUNT_ONES COUNT_ZEROS FIND_FIRST_SET FIND_FIRST_ZERO
FADD FSUB FMUL FDIV FSQRT FABS FCMP
GPIO_READ GPIO_WRITE GPIO_TOGGLE GPIO_SET_MODE GPIO_READ_PORT GPIO_WRITE_PORT
ADC_READ ADC_START_CONVERSION ADC_READ_MULTI DAC_WRITE ADC_SET_RESOLUTION ADC_SET_REFERENCE
PWM_START PWM_STOP PWM_SET_DUTY_CYCLE PWM_SET_FREQUENCY PWM_SET_PULSE_WIDTH
TIMER_START TIMER_STOP TIMER_RESET TIMER_READ TIMER_SET_PERIOD TIMER_SET_COMPARE GET_MILLIS GET_MICROS DELAY_MS DELAY_US
UART_INIT UART_WRITE_BYTE UART_WRITE_BUFFER UART_READ_BYTE UART_READ_BUFFER UART_AVAILABLE UART_FLUSH UART_SET_BAUD_RATE
SPI_INIT SPI_TRANSFER SPI_TRANSFER_BUFFER SPI_SET_MODE SPI_SET_CLOCK SPI_SET_BIT_ORDER SPI_BEGIN_TRANSACTION SPI_END_TRANSACTION
I2C_INIT I2C_START I2C_STOP I2C_WRITE I2C_READ I2C_WRITE_TO I2C_READ_FROM I2C_SET_CLOCK I2C_SCAN
CAN_INIT CAN_SEND CAN_RECEIVE CAN_SET_FILTER CAN_SET_BITRATE
USB_INIT USB_CONNECT USB_DISCONNECT USB_WRITE USB_READ USB_AVAILABLE
WDT_ENABLE WDT_DISABLE WDT_RESET WDT_SET_TIMEOUT
DMA_INIT DMA_START DMA_STOP DMA_CONFIG DMA_SET_SOURCE DMA_SET_DESTINATION
EEPROM_READ EEPROM_WRITE EEPROM_UPDATE FLASH_READ FLASH_WRITE FLASH_ERASE
SET_POWER_MODE DISABLE_PERIPHERAL ENABLE_PERIPHERAL SET_CLOCK_SPEED ENTER_STANDBY ENTER_DEEP_SLEEP
RTC_INIT RTC_SET_TIME RTC_GET_TIME RTC_SET_ALARM RTC_SET_CALENDAR
```
