//! C code generator for the Embeem total programming language.
//!
//! This crate provides functionality to compile Embeem programs to C code.

#![no_std]
#![forbid(unsafe_code)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
mod codegen;

#[cfg(feature = "alloc")]
pub use codegen::*;
