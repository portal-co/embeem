//! The core Expr type representing desugared expressions.

use alloc::boxed::Box;

use crate::Op;

/// A desugared expression in the Embeem language.
///
/// This is the target representation after parsing and desugaring the
/// surface syntax. It can represent:
/// - Operations: Nested operation trees
/// - Constants: Literal values
/// - Meta-variables: Named bindings (variables)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr<M> {
    /// An operation with nested expressions
    Op(Box<Op<Expr<M>>>),
    /// A constant value
    Const(u64),
    /// A meta-variable (for patterns/bindings)
    MetaVar(M),
}

impl<M> Expr<M> {
    /// Create a new constant expression.
    pub fn constant(value: u64) -> Self {
        Expr::Const(value)
    }

    /// Create a new meta-variable expression.
    pub fn meta_var(var: M) -> Self {
        Expr::MetaVar(var)
    }

    /// Create a new operation expression.
    pub fn op(op: Op<Expr<M>>) -> Self {
        Expr::Op(Box::new(op))
    }

    /// Map over meta-variables in this expression.
    pub fn map<N, E, F>(self, mut f: F) -> Result<Expr<N>, E>
    where
        F: FnMut(M) -> Result<Expr<N>, E>,
    {
        match self {
            Expr::Const(n) => Ok(Expr::Const(n)),
            Expr::MetaVar(m) => f(m),
            Expr::Op(op) => Ok(Expr::Op(Box::new(op.map(|e| e.map(&mut f))?))),
        }
    }

    /// Implements conditional: cond * then + (1 - cond) * otherwise
    ///
    /// This encodes a conditional as arithmetic, assuming `self` evaluates to 0 or 1.
    pub fn if_then_else(self, then_branch: Expr<M>, else_branch: Expr<M>) -> Expr<M>
    where
        M: Clone,
    {
        // cond * then + (1 - cond) * otherwise
        Expr::Op(Box::new(Op::Add(
            Expr::Op(Box::new(Op::Mul(self.clone(), then_branch))),
            Expr::Op(Box::new(Op::Mul(
                Expr::Op(Box::new(Op::Sub(Expr::Const(1), self))),
                else_branch,
            ))),
        )))
    }
}
