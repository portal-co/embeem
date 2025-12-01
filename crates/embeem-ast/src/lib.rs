//! AST types for the Embeem total programming language.
//!
//! This crate provides the abstract syntax tree representation for Embeem programs,
//! including the core `Expr` type that is used for desugared representations.

#![no_std]
#![forbid(unsafe_code)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
mod ast;
#[cfg(feature = "alloc")]
mod expr;
#[cfg(feature = "alloc")]
mod ops;

#[cfg(feature = "alloc")]
pub use ast::*;
#[cfg(feature = "alloc")]
pub use expr::Expr;
#[cfg(feature = "alloc")]
pub use ops::*;
