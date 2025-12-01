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
| `for i in a to b` | `|b - a + 1|` | Exactly `|b - a + 1|` iterations |
| `for i in a downto b` | `|a - b + 1|` | Exactly `|a - b + 1|` iterations |
| `repeat n` | `n` | Exactly `n` iterations |
| `while cond max m` | `m` | At most `m` iterations |

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

### 3.3 Loop Termination

**Lemma 3.2**: Every loop construct in Embeem terminates.

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
- The body is executed exactly `n` times
- `n` is a compile-time constant natural number
- Termination after `n` iterations is guaranteed. ∎

**Case 4: `while cond max m { body }`**
- Let $c_i$ be the condition value at iteration $i$
- The loop exits if $c_i = \text{false}$ OR $i \geq m$
- Since the second condition is guaranteed to be true after $m$ iterations, the loop executes at most $m$ times
- $m$ is a compile-time constant natural number. ∎

### 3.4 Expression Termination

**Lemma 3.3**: Every expression in Embeem terminates.

*Proof*: Expressions in Embeem consist of:

1. **Literals**: Immediate values, terminate trivially
2. **Variables**: Single memory lookup, terminates
3. **Operations**: Each operation in the emback op set is a primitive that computes a result from its inputs in bounded time
4. **Conditionals**: `if c { e1 } else { e2 }` - evaluating condition and one branch, both terminate by induction
5. **Function calls**: By Lemma 3.1, calls don't cycle; each call is to a "smaller" function

Since all expression forms terminate, the lemma holds. ∎

### 3.5 Statement Termination

**Lemma 3.4**: Every statement in Embeem terminates.

*Proof*: Statements in Embeem are:

1. **Let binding**: `let x = e;` - expression `e` terminates (Lemma 3.3)
2. **Assignment**: `x = e;` - expression `e` terminates (Lemma 3.3)
3. **Expression statement**: `e;` - terminates (Lemma 3.3)
4. **If statement**: Condition and branches terminate (Lemma 3.3)
5. **Loop statements**: Terminate (Lemma 3.2)
6. **Block**: Sequence of statements; each terminates; block terminates

∎

### 3.6 Main Theorem

**Theorem 3.5**: Every well-typed Embeem program terminates.

*Proof*: 
1. A program's execution starts at `main`
2. `main` is a function with a body (sequence of statements)
3. Each statement in `main` terminates (Lemma 3.4)
4. Any function called from `main` is earlier in the call DAG
5. By strong induction on the call DAG depth, all called functions terminate
6. Therefore, `main` terminates
7. Therefore, the program terminates

∎

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
