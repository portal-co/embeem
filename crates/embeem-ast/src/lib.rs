//! AST types for the Embeem total programming language.
//!
//! This crate provides the abstract syntax tree representation for Embeem programs,
//! including the core `Expr` type that is used for desugared representations.
//!
//! # Identifier Naming Conventions
//!
//! UPPER_SNAKE_CASE identifiers are reserved exclusively for operations. User-defined
//! identifiers (variables, functions, constants) must not use this pattern.
//!
//! See [`is_upper_snake_case`] and [`is_valid_user_identifier`] for validation functions.

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

#[cfg(all(feature = "alloc", feature = "inference"))]
mod infer;

#[cfg(feature = "alloc")]
pub use ast::*;
#[cfg(feature = "alloc")]
pub use expr::Expr;
#[cfg(feature = "alloc")]
pub use ops::{is_upper_snake_case, is_valid_user_identifier, *};

#[cfg(all(feature = "alloc", feature = "inference"))]
pub use infer::*;
