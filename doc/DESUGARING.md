# Desugaring Embeem to Expr

This document describes how Embeem language constructs are translated (desugared) into the `Expr<M>` type defined in `emback::ops`.

## 1. The Target: Expr Type

From `ops.rs`:

```rust
#[cfg(feature = "alloc")]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr<M> {
    Op(Box<Op<Expr<M>>>),    // An operation with nested expressions
    Const(u64),               // A constant value
    MetaVar(M),               // A meta-variable (for patterns/bindings)
}
```

The `Expr` type represents:
- **Operations**: Nested operation trees
- **Constants**: Literal values
- **Meta-variables**: Named bindings (variables)

## 2. Basic Desugaring Rules

### 2.1 Literals

Embeem literals desugar directly to `Expr::Const`:

```
⟦ 42 ⟧ = Expr::Const(42)
⟦ 0xFF ⟧ = Expr::Const(255)
⟦ true ⟧ = Expr::Const(1)
⟦ false ⟧ = Expr::Const(0)
```

### 2.2 Variables

Variable references become meta-variables:

```
⟦ x ⟧ = Expr::MetaVar("x")
```

### 2.3 Binary Operations

Binary operators desugar to `Op::Arithmetic`, `Op::Logical`, etc.:

```
⟦ a + b ⟧ = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Add {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))

⟦ a - b ⟧ = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Sub {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))

⟦ a * b ⟧ = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Mul {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))

⟦ a / b ⟧ = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Div {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))

⟦ a % b ⟧ = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Mod {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))
```

### 2.4 Comparison Operations

```
⟦ a == b ⟧ = Expr::Op(Box::new(Op::Comparison(ComparisonOp::Eq {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))

⟦ a < b ⟧ = Expr::Op(Box::new(Op::Comparison(ComparisonOp::Lt {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))

// Similarly for !=, <=, >, >=
```

### 2.5 Logical Operations

```
⟦ a & b ⟧ = Expr::Op(Box::new(Op::Logical(LogicalOp::And {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))

⟦ ~a ⟧ = Expr::Op(Box::new(Op::Logical(LogicalOp::Not {
    value: ⟦ a ⟧,
})))

⟦ a << n ⟧ = Expr::Op(Box::new(Op::Logical(LogicalOp::Shl {
    value: ⟦ a ⟧,
    shift: ⟦ n ⟧,
})))
```

### 2.6 Unary Operations

```
⟦ -a ⟧ = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Neg {
    value: ⟦ a ⟧,
})))
```

## 3. Conditional Desugaring

### 3.1 If Expression

The key insight is that `Expr` already provides an `if` method that uses arithmetic to simulate conditionals:

```rust
impl<M> Expr<M> {
    pub fn if(self, then: Expr<M>, otherwise: Expr<M>) -> Expr<M> {
        // Implements: cond * then + (1 - cond) * otherwise
        Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Add {
            a: Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Mul {
                a: self.clone(),
                b: then,
            }))),
            b: Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Mul {
                a: Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Sub {
                    a: Expr::Const(1),
                    b: self.clone(),
                }))),
                b: otherwise,
            }))),
        })))
    }
}
```

Therefore:

```
⟦ if cond { then_expr } else { else_expr } ⟧ 
    = ⟦ cond ⟧.if(⟦ then_expr ⟧, ⟦ else_expr ⟧)
```

Expanded:
```
⟦ if c { t } else { e } ⟧ = 
    ADD(
        MUL(⟦ c ⟧, ⟦ t ⟧),
        MUL(SUB(1, ⟦ c ⟧), ⟦ e ⟧)
    )
```

### 3.2 Condition Normalization

For the arithmetic encoding to work, conditions must be normalized to 0 or 1:

```
⟦ a and b ⟧ = MUL(⟦ a ⟧, ⟦ b ⟧)         // 1 iff both are 1
⟦ a or b ⟧ = SUB(1, MUL(SUB(1, ⟦ a ⟧), SUB(1, ⟦ b ⟧)))  // De Morgan
⟦ not a ⟧ = SUB(1, ⟦ a ⟧)
```

## 4. Loop Desugaring

### 4.1 Repeat Loop

A `repeat n { body }` unrolls into a sequence:

```
⟦ repeat 0 { body } ⟧ = Expr::Const(0)  // No-op, return 0

⟦ repeat n { body } ⟧ = 
    let _0 = ⟦ body ⟧ in
    let _1 = ⟦ body ⟧ in
    ...
    let _(n-1) = ⟦ body ⟧ in
    Expr::Const(0)
```

In practice, this is done through loop unrolling at compile time.

### 4.2 For Loop

```
⟦ for i in 0 to n { body } ⟧ =
    let i = 0 in ⟦ body ⟧;
    let i = 1 in ⟦ body ⟧;
    ...
    let i = n in ⟦ body ⟧;
```

Each iteration creates a new binding environment where `i` is bound to the current value.

### 4.3 Bounded While Loop

```
⟦ while cond max m { body } ⟧ =
```

Desugars to a nested conditional:

```
if cond_0 { 
    body_0;
    if cond_1 { 
        body_1;
        ...
        if cond_m { body_m } else { () }
    } else { () }
} else { () }
```

## 5. Statement Desugaring

### 5.1 Let Bindings

Let bindings introduce meta-variables:

```
⟦ let x = e1; e2 ⟧ = ⟦ e2 ⟧[x ↦ ⟦ e1 ⟧]
```

Where `[x ↦ e]` denotes substitution of all occurrences of `x` with `e`.

### 5.2 Mutable Variables

Mutable variables require tracking the "current" value. This is done through single-static assignment (SSA) transformation:

```embeem
let mut x = 0;
x = x + 1;
x = x + 2;
x
```

Becomes:

```
let x_0 = 0;
let x_1 = ADD(x_0, 1);
let x_2 = ADD(x_1, 2);
x_2
```

### 5.3 Blocks

Block expressions evaluate to their last expression:

```
⟦ { s1; s2; e } ⟧ = ⟦ s1 ⟧; ⟦ s2 ⟧; ⟦ e ⟧
```

## 6. Operation Calls

Operation calls map directly to the corresponding `Op` variant:

```
⟦ ADD(a, b) ⟧ = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Add {
    a: ⟦ a ⟧,
    b: ⟦ b ⟧,
})))

⟦ GPIO_READ(pin) ⟧ = Expr::Op(Box::new(Op::Gpio(GpioOp::Read {
    pin: ⟦ pin ⟧,
})))

⟦ ADC_READ(channel) ⟧ = Expr::Op(Box::new(Op::Analog(AnalogOp::Read {
    channel: ⟦ channel ⟧,
})))
```

## 7. Function Calls and Inlining

Since Embeem has no recursion, all function calls can be inlined:

```
fn add_one(x: u64) -> u64 { x + 1 }

fn main() {
    add_one(5)
}
```

Desugars to:

```
⟦ main ⟧ = 
    let x = 5 in
    ADD(MetaVar("x"), Const(1))
```

After substitution:
```
⟦ main ⟧ = ADD(Const(5), Const(1))
```

## 8. Complete Example

### Embeem Source

```embeem
fn abs_diff(a: u32, b: u32) -> u32 {
    if a > b {
        a - b
    } else {
        b - a
    }
}

fn main() {
    let x = 10;
    let y = 7;
    abs_diff(x, y)
}
```

### Step 1: Inline Functions

```
main = 
    let x = 10;
    let y = 7;
    if x > y { x - y } else { y - x }
```

### Step 2: Substitute Bindings

```
main = if 10 > 7 { 10 - 7 } else { 7 - 10 }
```

### Step 3: Desugar Conditional

Using the `if` encoding:

```rust
let cond = Expr::Op(Box::new(Op::Comparison(ComparisonOp::Gt {
    a: Expr::Const(10),
    b: Expr::Const(7),
})));

let then_branch = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Sub {
    a: Expr::Const(10),
    b: Expr::Const(7),
})));

let else_branch = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Sub {
    a: Expr::Const(7),
    b: Expr::Const(10),
})));

let result = Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Add {
    a: Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Mul {
        a: cond.clone(),
        b: then_branch,
    }))),
    b: Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Mul {
        a: Expr::Op(Box::new(Op::Arithmetic(ArithmeticOp::Sub {
            a: Expr::Const(1),
            b: cond,
        }))),
        b: else_branch,
    }))),
})));
```

## 9. Optimization Opportunities

### 9.1 Constant Folding

During desugaring, constant expressions can be evaluated:

```
⟦ 3 + 4 ⟧ = Expr::Const(7)  // Not ADD(Const(3), Const(4))
```

### 9.2 Dead Code Elimination

If a condition is statically known:

```
⟦ if true { a } else { b } ⟧ = ⟦ a ⟧
```

### 9.3 Common Subexpression Elimination

```
⟦ x + x ⟧ 
```

Can share the computation of `x` rather than duplicating it.

## 10. Type Mapping

| Embeem Type | Expr Representation |
|-------------|---------------------|
| `u8`, `u16`, `u32`, `u64` | `Expr::Const` (lower bits) |
| `i8`, `i16`, `i32`, `i64` | `Expr::Const` (two's complement) |
| `f32`, `f64` | `Expr::Const` (IEEE 754 bits) |
| `bool` | `Expr::Const(0)` or `Expr::Const(1)` |
| `[T; N]` | N separate `Expr` values |

## 11. Implementation Notes

### 11.1 Meta-Variable Type

The type parameter `M` in `Expr<M>` can be:
- `String` for named variables during parsing
- `usize` for de Bruijn indices after scope resolution
- `()` for closed expressions (no free variables)

### 11.2 Traversal

The `map` method on `Expr` enables transformations:

```rust
// Replace all variables with their values
fn substitute<M: Eq>(expr: Expr<M>, var: M, value: Expr<M>) -> Expr<M> {
    expr.map(|m| {
        if m == var { Ok(value.clone()) } 
        else { Ok(Expr::MetaVar(m)) }
    }).unwrap()
}
```

### 11.3 From Expr Back to Embeem

The desugaring is not always reversible, but pattern matching on `Expr` can recognize common patterns and pretty-print them:

```rust
fn to_embeem<M: Display>(expr: &Expr<M>) -> String {
    match expr {
        Expr::Const(n) => n.to_string(),
        Expr::MetaVar(m) => m.to_string(),
        Expr::Op(op) => match op.as_ref() {
            Op::Arithmetic(ArithmeticOp::Add { a, b }) => 
                format!("({} + {})", to_embeem(a), to_embeem(b)),
            // ... other cases
        }
    }
}
```
