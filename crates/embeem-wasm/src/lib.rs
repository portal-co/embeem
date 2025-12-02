//! WebAssembly code generator for the Embeem total programming language.
//!
//! This crate provides functionality to compile Embeem functions to WebAssembly
//! by generating an iterator of `wasmparser::Operator` instructions.
//!
//! ## Overview
//!
//! The WASM backend translates Embeem AST nodes into WebAssembly operators.
//! It uses a "pull" model where consumers iterate over generated operators.
//!
//! ## Usage
//!
//! ```ignore
//! use embeem_wasm::{WasmCodegen, FunctionResolver};
//! use embeem_ast::Function;
//!
//! struct MyResolver;
//! impl FunctionResolver for MyResolver {
//!     fn resolve_function(&self, name: &str) -> Option<u32> {
//!         // Return the WASM function index for the given name
//!         None
//!     }
//!     fn resolve_extern(&self, name: &str) -> Option<u32> {
//!         // Return the WASM function index for external functions
//!         None
//!     }
//! }
//!
//! let resolver = MyResolver;
//! let codegen = WasmCodegen::new(&resolver);
//! // Generate operators for a function...
//! ```
//!
//! ## Local Variable Management
//!
//! The code generator tracks local variables and maps Embeem variable names to
//! WASM local indices. Parameters are assigned indices 0..n-1, followed by
//! locally declared variables.
//!
//! ## Type Mapping
//!
//! Embeem types map to WASM types as follows:
//! - `i8`, `i16`, `i32` → `i32`
//! - `i64` → `i64`
//! - `u8`, `u16`, `u32` → `i32`
//! - `u64` → `i64`
//! - `f32` → `f32`
//! - `f64` → `f64`
//! - `bool` → `i32`
//! - Arrays are "splatted" into multiple locals (one per element)
//! - Tuples are "splatted" into multiple locals (one per field)

#![cfg_attr(not(feature = "std"), no_std)]
#![forbid(unsafe_code)]

#[cfg(any(feature = "alloc", feature = "std"))]
extern crate alloc;

#[cfg(any(feature = "alloc", feature = "std"))]
mod codegen;

#[cfg(any(feature = "alloc", feature = "std"))]
pub use codegen::*;

