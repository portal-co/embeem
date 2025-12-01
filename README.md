# Embeem

**A Total Programming Language for Embedded Systems**

Embeem is a domain-specific programming language designed for microcontroller applications. Its key property is **totality**: every well-formed Embeem program is mathematically guaranteed to terminate.

## Features

- **Guaranteed Termination**: No infinite loops or unbounded recursion possible
- **Bounded Loops**: All loops have statically-known iteration counts
- **Direct Hardware Access**: Built-in operations for GPIO, ADC, PWM, UART, I2C, SPI, and more
- **Predictable WCET**: Worst-case execution time is always computable
- **Simple Syntax**: Familiar C-like syntax optimized for embedded use cases

## Quick Example

```embeem
fn main() {
    GPIO_SET_MODE(13, OUTPUT);
    
    // Blink LED exactly 10 times
    repeat 10 {
        GPIO_WRITE(13, HIGH);
        DELAY_MS(500);
        GPIO_WRITE(13, LOW);
        DELAY_MS(500);
    }
}
```

## Documentation

- [Language Guide](doc/LANGUAGE_GUIDE.md) - Human-readable introduction
- [Specification](spec/SPECIFICATION.md) - Formal language specification
- [Totality Proof](doc/TOTALITY.md) - Proof that all programs terminate
- [Desugaring](doc/DESUGARING.md) - How Embeem compiles to Expr
- [Examples](examples/) - Sample programs

## Design Goals

1. **Safety**: No undefined behavior, no crashes
2. **Predictability**: Know exactly how long code takes to run
3. **Simplicity**: Easy to learn for embedded developers
4. **Interoperability**: Direct mapping to emback operations

## Why Totality?

Traditional languages allow `while (true) {}` which runs forever. In safety-critical embedded systems, this is dangerous. Embeem requires you to specify maximum loop iterations:

```embeem
// Wait for sensor, but give up after 1000 tries
while ADC_READ(0) < threshold max 1000 {
    DELAY_MS(10);
}
```

This ensures:
- Programs always finish
- WCET can be calculated
- No runaway code

## License

See [LICENSE](LICENSE)
