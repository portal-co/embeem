//! Parser for the Embeem total programming language.
//!
//! This crate provides a parser for Embeem source code using the `nom` parsing library.

#![no_std]
#![forbid(unsafe_code)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
mod parser;

#[cfg(feature = "alloc")]
pub use parser::*;
