//! Code generators for the Embeem total programming language.
//!
//! This crate provides functionality to compile Embeem programs to various
//! target languages, including C and TypeScript/JavaScript.
//!
//! ## Modules
//!
//! - [`c`] - C code generator for embedded systems
//! - [`typescript`] - TypeScript/JavaScript code generator
//!
//! ## Mangling
//!
//! Both backends use the shared mangling scheme from [`embeem_ast::mangle`].
//! See that module for details on how function and operation names are encoded.

#![no_std]
#![forbid(unsafe_code)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
pub mod c;

#[cfg(feature = "alloc")]
pub mod typescript;

// Re-export commonly used items for convenience
#[cfg(feature = "alloc")]
pub use c::{compile_to_c, compile_to_c_with_options, compile_to_c_gcc, CCodegen, CodegenError, CCodegenOptions};

#[cfg(feature = "alloc")]
pub use typescript::{compile_to_js, compile_to_ts, compile_to_ts_with_options, TsCodegen, TsCodegenOptions};
