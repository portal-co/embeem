# Totality Proof for Embeem

**Theorem**: Every well-typed Embeem program terminates.

## 1. Introduction

This document provides a formal justification that Embeem is a **total** programming language—that is, every well-formed Embeem program is guaranteed to terminate in a finite number of steps.

### 1.1 Definitions

**Definition 1.1 (Termination)**: A program *terminates* if its execution reaches a final state (returns a value) after a finite number of reduction steps.

**Definition 1.2 (Total Language)**: A programming language is *total* if every well-typed program in that language terminates.

**Definition 1.3 (Partial Language)**: A programming language is *partial* if it admits programs that may not terminate (e.g., infinite loops).

## 2. Language Restrictions for Totality

Embeem achieves totality through careful restrictions on three language features that traditionally enable non-termination:

### 2.1 No Unbounded Loops

**Standard Languages**: Languages like C, Rust, and Python allow loops with arbitrary conditions:

```c
while (1) { }              // Infinite loop
while (condition) { }      // May never terminate
```

**Embeem Restriction**: All loops in Embeem have statically-bounded iteration counts:

| Construct | Bound | Termination Guarantee |
|-----------|-------|----------------------|
| `for i in a to b` | `\|b - a + 1\|` | Exactly `\|b - a + 1\|` iterations |
| `for i in a downto b` | `\|a - b + 1\|` | Exactly `\|a - b + 1\|` iterations |
| `repeat n` | `n` | Exactly `n` iterations |
| `while cond max m` | `m` | At most `m` iterations |

**Note**: The bounds `n` and `m` may be compile-time constants or immutable variables. When an immutable variable is used, its value is captured at loop entry and cannot change during iteration, preserving the termination guarantee.

### 2.2 No General Recursion

**Standard Languages**: Allow functions to call themselves:

```c
void f() { f(); }  // Infinite recursion
```

**Embeem Restriction**: 
- Function calls form a directed acyclic graph (DAG)
- A function `f` can only call functions defined before `f`
- No function can call itself, directly or transitively

### 2.3 No Divergent Primitives

**Standard Languages**: May provide operations that don't return:

```c
while(1);  // halt and catch fire
```

**Embeem Restriction**: All primitive operations are total:
- All arithmetic operations return values (division by zero → 0)
- All IO operations return within bounded time (timeout semantics)
- No `HALT` operation is exposed (only internal use for ending `main`)

## 3. Formal Proof

### 3.1 Program Structure

An Embeem program consists of:
- A set of function definitions $F = \{f_1, f_2, \ldots, f_n\}$
- An entry point `main`

### 3.2 Call Graph is Acyclic

**Lemma 3.1**: The call graph of any Embeem program is a DAG.

*Proof*: Functions in Embeem are defined sequentially. A function $f_i$ can only call functions $f_j$ where $j < i$ (functions defined earlier). Since $j < i$ implies there's no path from $f_j$ back to $f_i$, cycles are impossible. ∎

### 3.3 Immutable Variable Invariant

**Definition 3.2 (Immutable Variable)**: A variable declared without `mut` is *immutable*. Its value, once assigned, cannot be modified.

**Lemma 3.3 (Bound Stability)**: If a loop bound expression $b$ consists solely of literals and immutable variables, then $b$ evaluates to a fixed finite value that does not change during loop execution.

*Proof*: 
1. Literals are constant by definition
2. Immutable variables cannot appear on the left-hand side of an assignment
3. The loop body cannot contain assignments to immutable variables (type error)
4. Therefore, evaluating $b$ at loop entry yields the same result as at any point during execution
5. Since all integer types in Embeem are finite (u8 to u64, i8 to i64), $b$ is bounded by $2^{64}$

∎

### 3.4 Loop Termination

**Lemma 3.4**: Every loop construct in Embeem terminates.

*Proof by cases*:

**Case 1: `for i in a to b { body }`**
- The loop variable `i` is initialized to `a`
- Each iteration increments `i` by 1
- The loop exits when `i > b`
- Since `a, b ∈ ℤ` (bounded integers), the number of iterations is exactly $\max(0, b - a + 1)$
- This is a finite natural number. ∎

**Case 2: `for i in a downto b { body }`**
- The loop variable `i` is initialized to `a`
- Each iteration decrements `i` by 1
- The loop exits when `i < b`
- The number of iterations is exactly $\max(0, a - b + 1)$
- This is a finite natural number. ∎

**Case 3: `repeat n { body }`**
- The bound `n` is either a compile-time constant or an immutable variable
- If `n` is an immutable variable, its value is determined before the loop starts
- The body is executed exactly `n` times, where `n` is the value at loop entry
- Since immutable variables cannot be modified, `n` remains fixed throughout
- This is a finite natural number (bounded by the integer type). ∎

**Case 4: `while cond max m { body }`**
- Let $c_i$ be the condition value at iteration $i$
- The loop exits if $c_i = \text{false}$ OR $i \geq m$
- The bound `m` is either a compile-time constant or an immutable variable
- If `m` is an immutable variable, its value $m_0$ is captured at loop entry
- Since immutable variables cannot be modified, $m_0$ remains fixed
- The second condition is guaranteed to be true after $m_0$ iterations
- Therefore, the loop executes at most $m_0$ times
- $m_0$ is a finite natural number (bounded by the integer type). ∎

### 3.5 Expression Termination

**Lemma 3.5**: Every expression in Embeem terminates.

*Proof*: Expressions in Embeem consist of:

1. **Literals**: Immediate values, terminate trivially
2. **Variables**: Single memory lookup, terminates
3. **Operations**: Each operation in the emback op set is a primitive that computes a result from its inputs in bounded time
4. **Conditionals**: `if c { e1 } else { e2 }` - evaluating condition and one branch, both terminate by induction
5. **Function calls**: By Lemma 3.1, calls don't cycle; each call is to a "smaller" function

Since all expression forms terminate, the lemma holds. ∎

### 3.6 Statement Termination

**Lemma 3.6**: Every statement in Embeem terminates.

*Proof*: Statements in Embeem are:

1. **Let binding**: `let x = e;` - expression `e` terminates (Lemma 3.5)
2. **Assignment**: `x = e;` - expression `e` terminates (Lemma 3.5)
3. **Expression statement**: `e;` - terminates (Lemma 3.5)
4. **If statement**: Condition and branches terminate (Lemma 3.5)
5. **Loop statements**: Terminate (Lemma 3.4)
6. **Block**: Sequence of statements; each terminates; block terminates

∎

### 3.7 Main Theorem

**Theorem 3.7 (Totality without External Functions)**: Every well-typed Embeem program *without external functions* terminates.

*Proof*: 
1. A program's execution starts at `main`
2. `main` is a function with a body (sequence of statements)
3. Each statement in `main` terminates (Lemma 3.6)
4. Any function called from `main` is earlier in the call DAG
5. By strong induction on the call DAG depth, all called functions terminate
6. Therefore, `main` terminates
7. Therefore, the program terminates

∎

### 3.8 External Functions

External functions are provided by the environment and declared without a body:

```embeem
extern fn get_sensor_value(channel: u8) -> i32;
extern fn set_led(pin: u8, value: bool);
```

**Definition 3.8 (Total External Function)**: An external function $e$ is *total* if every call to $e$ with valid arguments returns in finite time.

**Theorem 3.9 (Conditional Totality with External Functions)**: An Embeem program $P$ with external functions $E = \{e_1, e_2, \ldots, e_k\}$ terminates *if and only if* all external functions in $E$ are total.

*Proof (⟹)*: 
If all external functions terminate, then:
1. Lemma 3.5 extends: calls to external functions terminate by assumption
2. All other lemmas hold unchanged
3. By Theorem 3.7's proof structure, the program terminates

*Proof (⟸)*:
If some external function $e_i$ does not terminate:
1. Construct a program that calls $e_i$
2. The call to $e_i$ does not return
3. The program does not terminate

∎

**Corollary 3.10 (Bounds with External Functions)**: If external function $e_i$ has worst-case execution time $T_i$, then the program's WCET can be computed by substituting $T_i$ for each call to $e_i$.

### 3.9 Pure Embeem Programs

**Definition 3.11 (Pure Embeem Program)**: A program is *pure* if it contains no external function declarations.

**Theorem 3.12 (Unconditional Totality)**: Every pure Embeem program terminates unconditionally.

*Proof*: Direct consequence of Theorem 3.7. ∎

This distinction is important for embedded systems:
- **Pure programs**: Guaranteed to terminate, suitable for safety-critical applications
- **Programs with external functions**: Termination depends on the environment's guarantees

## 4. Complexity Bounds

Not only does every Embeem program terminate, but we can compute an upper bound on the number of steps.

### 4.1 Step Function

Define $\text{steps}(P)$ as the maximum number of primitive operations executed by program $P$:

$$\text{steps}(P) = \sum_{f \in F} \text{calls}(f) \cdot \text{body-steps}(f)$$

where:
- $\text{calls}(f)$ = maximum times function $f$ is called
- $\text{body-steps}(f)$ = steps to execute $f$'s body once

### 4.2 Loop Contribution

For a loop `for i in a to b { body }`:
$$\text{steps} = (b - a + 1) \cdot \text{steps}(\text{body})$$

For nested loops:
$$\text{steps}(\texttt{for } i \texttt{ in } 0 \texttt{ to } n \{ \texttt{for } j \texttt{ in } 0 \texttt{ to } m \{ \ldots \}\}) = O(n \cdot m)$$

### 4.3 WCET Analysis

The Worst-Case Execution Time (WCET) of an Embeem program is always computable:

$$\text{WCET}(P) = \text{steps}(P) \cdot t_{\text{step}}$$

where $t_{\text{step}}$ is the time per primitive operation on the target architecture.

## 5. Comparison with Other Total Languages

| Language | Totality Mechanism |
|----------|-------------------|
| Agda | Structural recursion + termination checker |
| Idris | Size types + termination checker |
| Coq | Guardedness + structural recursion |
| F* | Refinement types + fuel |
| **Embeem** | Bounded loops + no recursion |

Embeem's approach is syntactically simpler than dependent type systems, making it suitable for embedded programmers without theorem prover experience.

## 6. What Embeem Cannot Express

Totality comes with trade-offs. Embeem cannot express:

### 6.1 Unbounded Iteration

```c
// C: Read until EOF
while ((c = getchar()) != EOF) { process(c); }
```

**Embeem alternative**: Use bounded maximum with early exit:
```embeem
while UART_AVAILABLE(0) > 0 max MAX_INPUT_SIZE {
    process(UART_READ_BYTE(0));
}
```

### 6.2 General Recursion

```python
# Python: Fibonacci (recursive)
def fib(n):
    if n <= 1: return n
    return fib(n-1) + fib(n-2)
```

**Embeem alternative**: Iterative with bounded loop:
```embeem
fn fib(n: u64) -> u64 {
    let mut a: u64 = 0;
    let mut b: u64 = 1;
    
    for i in 0 to 63 {  // Max iterations for u64
        if i < n {
            let temp = a + b;
            a = b;
            b = temp;
        }
    }
    a
}
```

### 6.3 Turing-Complete Computation

Embeem is intentionally **not** Turing-complete. It has the computational power of primitive recursive functions, which is sufficient for:
- All practical embedded systems tasks
- Signal processing
- Control loops
- State machines
- Communication protocols

## 7. Conclusion

Embeem's totality is guaranteed by three syntactic restrictions:

1. **Bounded loops**: All iteration counts are known at compile time
2. **Acyclic calls**: Functions cannot recurse
3. **Total primitives**: All operations return values

These restrictions are sufficient to ensure that every well-typed Embeem program terminates, while preserving enough expressiveness for practical embedded systems programming.

---

## Appendix A: Formal Semantics (Operational)

### Small-Step Semantics

$$\frac{}{v \rightarrow v} \text{(Value)}$$

$$\frac{e_1 \rightarrow e_1'}{e_1 \oplus e_2 \rightarrow e_1' \oplus e_2} \text{(BinOpL)}$$

$$\frac{e_2 \rightarrow e_2'}{v_1 \oplus e_2 \rightarrow v_1 \oplus e_2'} \text{(BinOpR)}$$

$$\frac{}{v_1 \oplus v_2 \rightarrow [[v_1 \oplus v_2]]} \text{(BinOpEval)}$$

$$\frac{c \rightarrow \text{true}}{\texttt{if } c \texttt{ \{ } e_1 \texttt{ \} else \{ } e_2 \texttt{ \}} \rightarrow e_1} \text{(IfTrue)}$$

$$\frac{c \rightarrow \text{false}}{\texttt{if } c \texttt{ \{ } e_1 \texttt{ \} else \{ } e_2 \texttt{ \}} \rightarrow e_2} \text{(IfFalse)}$$

### Loop Unrolling

$$\texttt{repeat } 0 \texttt{ \{ } s \texttt{ \}} \rightarrow \texttt{skip}$$

$$\texttt{repeat } (n+1) \texttt{ \{ } s \texttt{ \}} \rightarrow s; \texttt{repeat } n \texttt{ \{ } s \texttt{ \}}$$

### Progress and Preservation

**Theorem (Progress)**: If $\Gamma \vdash e : \tau$, then either $e$ is a value or $\exists e'. e \rightarrow e'$.

**Theorem (Preservation)**: If $\Gamma \vdash e : \tau$ and $e \rightarrow e'$, then $\Gamma \vdash e' : \tau$.

Together with the step bound from Section 4, these theorems imply termination.
