//! AST types for the Embeem total programming language.
//!
//! This crate provides the abstract syntax tree representation for Embeem programs.
//! Operations are represented using a path-based system where nested operations like
//! `WRITE(GPIO(pin), value)` are stored as `path: ["WRITE", "GPIO"], args: [pin, value]`.
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
mod ops;
#[cfg(feature = "alloc")]
mod pretty;

#[cfg(all(feature = "alloc", feature = "inference"))]
mod infer;

#[cfg(feature = "alloc")]
pub use ast::*;
#[cfg(feature = "alloc")]
pub use ops::{is_upper_snake_case, is_valid_user_identifier, op_kind_from_str, OpKind};
#[cfg(feature = "alloc")]
pub use pretty::{pretty_print_program, PrettyPrint, PrettyPrintContext};

#[cfg(all(feature = "alloc", feature = "inference"))]
pub use infer::*;
