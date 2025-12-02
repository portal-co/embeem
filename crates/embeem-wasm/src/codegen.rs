//! WebAssembly operator generation for Embeem functions.
//!
//! This module provides the core code generation logic that transforms Embeem
//! AST into WebAssembly operators.

use alloc::string::String;
use alloc::vec::Vec;
use alloc::collections::BTreeMap;

use embeem_ast::{
    AssignTarget, BinaryOp, Block, Expression, Function, Literal, Param, PrimitiveType,
    RangeDirection, Statement, Type, UnaryOp, ElseBlock,
};
use wasmparser::{BlockType, Ieee64, Operator, ValType};

/// Trait for resolving function names to WebAssembly function indices.
///
/// Implementors must provide resolution for both user-defined Embeem functions
/// and external functions declared with `extern fn`.
pub trait FunctionResolver {
    /// Resolve an Embeem function name to its WASM function index.
    ///
    /// Returns `Some(index)` if the function exists, `None` otherwise.
    fn resolve_function(&self, name: &str) -> Option<u32>;

    /// Resolve an external function name to its WASM function index.
    ///
    /// External functions are typically imported from the host environment.
    /// Returns `Some(index)` if the extern exists, `None` otherwise.
    fn resolve_extern(&self, name: &str) -> Option<u32>;
}

/// Error type for WASM code generation.
#[derive(Clone, Debug, PartialEq)]
pub struct WasmCodegenError {
    pub message: String,
}

impl WasmCodegenError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

/// Maps Embeem primitive types to WebAssembly value types.
pub fn primitive_to_wasm_type(ty: PrimitiveType) -> ValType {
    match ty {
        PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 => ValType::I32,
        PrimitiveType::I64 => ValType::I64,
        PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 => ValType::I32,
        PrimitiveType::U64 => ValType::I64,
        PrimitiveType::F32 => ValType::F32,
        PrimitiveType::F64 => ValType::F64,
        PrimitiveType::Bool => ValType::I32,
    }
}

/// Maps Embeem types to WebAssembly value types.
///
/// For compound types (arrays/tuples), returns the element type.
/// Use `type_local_count` to determine how many locals are needed.
pub fn type_to_wasm(ty: &Type) -> ValType {
    match ty {
        Type::Primitive(p) => primitive_to_wasm_type(*p),
        Type::Array(elem_ty, _) => type_to_wasm(elem_ty),
        Type::Tuple(elems) => {
            // For tuples, return the first element type (or i32 if empty)
            elems.first().map(type_to_wasm).unwrap_or(ValType::I32)
        }
    }
}

/// Returns the number of WASM locals needed to represent an Embeem type.
///
/// Arrays and tuples are "splatted" into multiple locals.
pub fn type_local_count(ty: &Type) -> usize {
    match ty {
        Type::Primitive(_) => 1,
        Type::Array(elem_ty, len) => type_local_count(elem_ty) * (*len as usize),
        Type::Tuple(elems) => elems.iter().map(type_local_count).sum(),
    }
}

/// Returns the WASM types for all locals needed to represent an Embeem type.
///
/// For compound types, this returns multiple types (one per splatted local).
pub fn type_to_wasm_locals(ty: &Type) -> Vec<ValType> {
    match ty {
        Type::Primitive(p) => vec![primitive_to_wasm_type(*p)],
        Type::Array(elem_ty, _len) => {
            let elem_types = type_to_wasm_locals(elem_ty);
            elem_types.into_iter().cycle().take(type_local_count(ty)).collect()
        }
        Type::Tuple(elems) => {
            elems.iter().flat_map(type_to_wasm_locals).collect()
        }
    }
}

/// Information about a declared variable.
#[derive(Clone, Debug)]
pub struct VarInfo {
    /// The starting local index for this variable.
    pub start_index: u32,
    /// The number of locals this variable occupies.
    pub local_count: usize,
    /// The Embeem type of the variable.
    pub ty: Type,
}

/// Context for tracking local variables during code generation.
#[derive(Clone, Debug, Default)]
pub struct LocalContext {
    /// Maps variable names to their info (including start index and span).
    vars: BTreeMap<String, VarInfo>,
    /// Next available local index.
    next_local: u32,
    /// Types of all locals (for validation).
    local_types: Vec<ValType>,
}

impl LocalContext {
    /// Create a new local context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a local context initialized with function parameters.
    pub fn from_params(params: &[Param]) -> Self {
        let mut ctx = Self::new();
        for param in params {
            let wasm_types = type_to_wasm_locals(&param.ty);
            let local_count = wasm_types.len();
            ctx.vars.insert(param.name.clone(), VarInfo {
                start_index: ctx.next_local,
                local_count,
                ty: param.ty.clone(),
            });
            for wasm_ty in wasm_types {
                ctx.local_types.push(wasm_ty);
                ctx.next_local += 1;
            }
        }
        ctx
    }

    /// Declare a new local variable and return its starting index.
    pub fn declare_local(&mut self, name: &str, ty: &Type) -> u32 {
        let idx = self.next_local;
        let wasm_types = type_to_wasm_locals(ty);
        let local_count = wasm_types.len();
        self.vars.insert(name.to_string(), VarInfo {
            start_index: idx,
            local_count,
            ty: ty.clone(),
        });
        for wasm_ty in wasm_types {
            self.local_types.push(wasm_ty);
            self.next_local += 1;
        }
        idx
    }

    /// Allocate an anonymous local (for temporaries).
    pub fn alloc_temp(&mut self, ty: ValType) -> u32 {
        let idx = self.next_local;
        self.local_types.push(ty);
        self.next_local += 1;
        idx
    }

    /// Look up a local variable by name, returning its starting index.
    pub fn get_local(&self, name: &str) -> Option<u32> {
        self.vars.get(name).map(|info| info.start_index)
    }

    /// Look up full variable info by name.
    pub fn get_var_info(&self, name: &str) -> Option<&VarInfo> {
        self.vars.get(name)
    }

    /// Get all local types (excluding parameters, which are separate in WASM).
    pub fn get_additional_locals(&self, param_count: usize) -> &[ValType] {
        // Count how many actual WASM locals the params occupy
        let param_local_count: usize = self.local_types.len().min(
            self.vars.values()
                .filter(|v| v.start_index < param_count as u32)
                .map(|v| v.local_count)
                .sum()
        );
        &self.local_types[param_local_count..]
    }

    /// Get the total number of WASM locals used by parameters.
    pub fn param_local_count(&self, param_count: usize) -> usize {
        // Sum the local counts for the first `param_count` declared variables
        self.vars.values()
            .take(param_count)
            .map(|v| v.local_count)
            .sum()
    }
}

/// WebAssembly code generator for Embeem functions.
///
/// This struct holds the state needed to generate WASM operators from an
/// Embeem function. It uses a resolver to look up function indices for calls.
pub struct WasmCodegen<'a, R: FunctionResolver> {
    resolver: &'a R,
    locals: LocalContext,
    /// Stack of label depths for break/continue (not currently used in Embeem).
    label_depth: u32,
}

impl<'a, R: FunctionResolver> WasmCodegen<'a, R> {
    /// Create a new code generator with the given function resolver.
    pub fn new(resolver: &'a R) -> Self {
        Self {
            resolver,
            locals: LocalContext::new(),
            label_depth: 0,
        }
    }

    /// Generate WASM operators for a function body.
    ///
    /// Returns a vector of operators and the additional local declarations needed.
    pub fn generate_function(
        &mut self,
        func: &Function,
    ) -> Result<(Vec<Operator<'static>>, Vec<ValType>), WasmCodegenError> {
        // Initialize locals from parameters
        self.locals = LocalContext::from_params(&func.params);
        self.label_depth = 0;

        let mut ops = Vec::new();

        // Generate body
        self.generate_block(&func.body, &mut ops)?;

        // Add implicit return if no result
        ops.push(Operator::End);

        let additional_locals = self.locals.get_additional_locals(func.params.len()).to_vec();
        Ok((ops, additional_locals))
    }

    /// Generate operators for a block.
    fn generate_block(
        &mut self,
        block: &Block,
        ops: &mut Vec<Operator<'static>>,
    ) -> Result<(), WasmCodegenError> {
        // Generate all statements
        for stmt in &block.statements {
            self.generate_statement(stmt, ops)?;
        }

        // Generate result expression if present
        if let Some(result) = &block.result {
            self.generate_expression(result, ops)?;
        }

        Ok(())
    }

    /// Generate operators for a statement.
    fn generate_statement(
        &mut self,
        stmt: &Statement,
        ops: &mut Vec<Operator<'static>>,
    ) -> Result<(), WasmCodegenError> {
        match stmt {
            Statement::Let { name, ty, value, .. } => {
                let actual_ty = ty.as_ref().unwrap_or(&Type::Primitive(PrimitiveType::I32));
                let local_count = type_local_count(actual_ty);
                
                // Declare the local(s)
                let local_idx = self.locals.declare_local(name, actual_ty);
                
                // Generate value expression (may push multiple values for arrays/tuples)
                self.generate_expression(value, ops)?;
                
                // Store to local(s) - in reverse order since stack is LIFO
                for i in (0..local_count).rev() {
                    ops.push(Operator::LocalSet { local_index: local_idx + i as u32 });
                }
            }

            Statement::Assign { target, value } => {
                match target {
                    AssignTarget::Identifier(name) => {
                        let var_info = self.locals.get_var_info(name).ok_or_else(|| {
                            WasmCodegenError::new(format!("undefined variable: {}", name))
                        })?.clone();
                        
                        self.generate_expression(value, ops)?;
                        
                        // Store to local(s) - in reverse order since stack is LIFO
                        for i in (0..var_info.local_count).rev() {
                            ops.push(Operator::LocalSet { local_index: var_info.start_index + i as u32 });
                        }
                    }
                    AssignTarget::Index { array, index } => {
                        // For array index assignment with splatted locals: arr[idx] = value
                        // We need to compute which local to write to based on the index.
                        // Since WASM doesn't have computed local access, we generate a switch.
                        
                        let var_info = self.locals.get_var_info(array).ok_or_else(|| {
                            WasmCodegenError::new(format!("undefined array: {}", array))
                        })?.clone();
                        
                        // Get element count from the type
                        let elem_count = match &var_info.ty {
                            Type::Array(elem_ty, len) => {
                                let elem_locals = type_local_count(elem_ty);
                                if elem_locals != 1 {
                                    return Err(WasmCodegenError::new(
                                        "nested compound types in arrays not yet supported"
                                    ));
                                }
                                *len as usize
                            }
                            _ => return Err(WasmCodegenError::new(
                                format!("cannot index non-array type: {}", array)
                            )),
                        };
                        
                        // Generate index expression and store in temp
                        let idx_temp = self.locals.alloc_temp(ValType::I32);
                        self.generate_expression(index, ops)?;
                        ops.push(Operator::LocalSet { local_index: idx_temp });
                        
                        // Generate value expression and store in temp
                        let val_temp = self.locals.alloc_temp(ValType::I32);
                        self.generate_expression(value, ops)?;
                        ops.push(Operator::LocalSet { local_index: val_temp });
                        
                        // Generate switch: if idx == 0, set local[0]; else if idx == 1, set local[1]; etc.
                        for i in 0..elem_count {
                            ops.push(Operator::LocalGet { local_index: idx_temp });
                            ops.push(Operator::I32Const { value: i as i32 });
                            ops.push(Operator::I32Eq);
                            ops.push(Operator::If { blockty: BlockType::Empty });
                            ops.push(Operator::LocalGet { local_index: val_temp });
                            ops.push(Operator::LocalSet { local_index: var_info.start_index + i as u32 });
                            ops.push(Operator::End);
                        }
                    }
                }
            }

            Statement::Expr(expr) => {
                self.generate_expression(expr, ops)?;
                // Drop the result if it's not used
                ops.push(Operator::Drop);
            }

            Statement::If { condition, then_block, else_block } => {
                self.generate_expression(condition, ops)?;
                
                ops.push(Operator::If {
                    blockty: BlockType::Empty,
                });
                self.label_depth += 1;
                
                self.generate_block(then_block, ops)?;
                
                if let Some(else_blk) = else_block {
                    ops.push(Operator::Else);
                    match else_blk {
                        ElseBlock::Block(block) => {
                            self.generate_block(block, ops)?;
                        }
                        ElseBlock::ElseIf(if_stmt) => {
                            self.generate_statement(if_stmt, ops)?;
                        }
                    }
                }
                
                ops.push(Operator::End);
                self.label_depth -= 1;
            }

            Statement::For { variable, start, end, direction, body } => {
                // For loop: for i in start to end { body }
                // 
                // Compiled as:
                // local.set $i, start
                // block $exit
                //   loop $continue
                //     br_if $exit, (i >= end) or (i <= end) depending on direction
                //     body
                //     i = i +/- 1
                //     br $continue
                //   end
                // end
                
                // Declare loop variable
                let loop_var = self.locals.declare_local(variable, &Type::Primitive(PrimitiveType::I32));
                
                // Initialize loop variable
                self.generate_expression(start, ops)?;
                ops.push(Operator::LocalSet { local_index: loop_var });
                
                // Outer block for exit
                ops.push(Operator::Block {
                    blockty: BlockType::Empty,
                });
                self.label_depth += 1;
                
                // Inner loop
                ops.push(Operator::Loop {
                    blockty: BlockType::Empty,
                });
                self.label_depth += 1;
                
                // Check condition (exit if past end)
                ops.push(Operator::LocalGet { local_index: loop_var });
                self.generate_expression(end, ops)?;
                
                match direction {
                    RangeDirection::To => {
                        // Exit if i > end
                        ops.push(Operator::I32GtS);
                    }
                    RangeDirection::DownTo => {
                        // Exit if i < end  
                        ops.push(Operator::I32LtS);
                    }
                }
                ops.push(Operator::BrIf { relative_depth: 1 }); // Break to outer block
                
                // Generate body
                self.generate_block(body, ops)?;
                
                // Increment/decrement
                ops.push(Operator::LocalGet { local_index: loop_var });
                ops.push(Operator::I32Const { value: 1 });
                match direction {
                    RangeDirection::To => ops.push(Operator::I32Add),
                    RangeDirection::DownTo => ops.push(Operator::I32Sub),
                }
                ops.push(Operator::LocalSet { local_index: loop_var });
                
                // Continue loop
                ops.push(Operator::Br { relative_depth: 0 });
                
                // End loop
                ops.push(Operator::End);
                self.label_depth -= 1;
                
                // End block
                ops.push(Operator::End);
                self.label_depth -= 1;
            }

            Statement::Repeat { count, body } => {
                // Repeat loop: repeat count { body }
                //
                // Compiled as:
                // local.set $counter, count
                // block $exit
                //   loop $continue
                //     br_if $exit, ($counter <= 0)
                //     body
                //     $counter = $counter - 1
                //     br $continue
                //   end
                // end
                
                let counter = self.locals.alloc_temp(ValType::I32);
                
                // Initialize counter
                self.generate_expression(count, ops)?;
                ops.push(Operator::LocalSet { local_index: counter });
                
                // Outer block
                ops.push(Operator::Block {
                    blockty: BlockType::Empty,
                });
                self.label_depth += 1;
                
                // Loop
                ops.push(Operator::Loop {
                    blockty: BlockType::Empty,
                });
                self.label_depth += 1;
                
                // Check if counter <= 0
                ops.push(Operator::LocalGet { local_index: counter });
                ops.push(Operator::I32Const { value: 0 });
                ops.push(Operator::I32LeS);
                ops.push(Operator::BrIf { relative_depth: 1 });
                
                // Body
                self.generate_block(body, ops)?;
                
                // Decrement counter
                ops.push(Operator::LocalGet { local_index: counter });
                ops.push(Operator::I32Const { value: 1 });
                ops.push(Operator::I32Sub);
                ops.push(Operator::LocalSet { local_index: counter });
                
                // Continue
                ops.push(Operator::Br { relative_depth: 0 });
                
                // End loop
                ops.push(Operator::End);
                self.label_depth -= 1;
                
                // End block
                ops.push(Operator::End);
                self.label_depth -= 1;
            }

            Statement::While { condition, max_iterations, body } => {
                // Bounded while loop: while condition max max_iterations { body }
                //
                // Compiled as:
                // local.set $counter, max_iterations
                // block $exit
                //   loop $continue
                //     br_if $exit, ($counter <= 0)
                //     br_if $exit, (!condition)
                //     body
                //     $counter = $counter - 1
                //     br $continue
                //   end
                // end
                
                let counter = self.locals.alloc_temp(ValType::I32);
                
                // Initialize counter
                self.generate_expression(max_iterations, ops)?;
                ops.push(Operator::LocalSet { local_index: counter });
                
                // Outer block
                ops.push(Operator::Block {
                    blockty: BlockType::Empty,
                });
                self.label_depth += 1;
                
                // Loop
                ops.push(Operator::Loop {
                    blockty: BlockType::Empty,
                });
                self.label_depth += 1;
                
                // Check counter
                ops.push(Operator::LocalGet { local_index: counter });
                ops.push(Operator::I32Const { value: 0 });
                ops.push(Operator::I32LeS);
                ops.push(Operator::BrIf { relative_depth: 1 });
                
                // Check condition (exit if false)
                self.generate_expression(condition, ops)?;
                ops.push(Operator::I32Eqz);
                ops.push(Operator::BrIf { relative_depth: 1 });
                
                // Body
                self.generate_block(body, ops)?;
                
                // Decrement counter
                ops.push(Operator::LocalGet { local_index: counter });
                ops.push(Operator::I32Const { value: 1 });
                ops.push(Operator::I32Sub);
                ops.push(Operator::LocalSet { local_index: counter });
                
                // Continue
                ops.push(Operator::Br { relative_depth: 0 });
                
                // End loop
                ops.push(Operator::End);
                self.label_depth -= 1;
                
                // End block
                ops.push(Operator::End);
                self.label_depth -= 1;
            }
        }

        Ok(())
    }

    /// Generate operators for an expression.
    fn generate_expression(
        &mut self,
        expr: &Expression,
        ops: &mut Vec<Operator<'static>>,
    ) -> Result<(), WasmCodegenError> {
        match expr {
            Expression::Literal(lit) => {
                match lit {
                    Literal::Integer(n) => {
                        if *n <= i32::MAX as u64 {
                            ops.push(Operator::I32Const { value: *n as i32 });
                        } else {
                            ops.push(Operator::I64Const { value: *n as i64 });
                        }
                    }
                    Literal::Float(f) => {
                        // Use f64 by default
                        ops.push(Operator::F64Const { value: Ieee64::from(*f) });
                    }
                    Literal::Bool(b) => {
                        ops.push(Operator::I32Const { value: if *b { 1 } else { 0 } });
                    }
                }
            }

            Expression::Identifier(name) => {
                let var_info = self.locals.get_var_info(name).ok_or_else(|| {
                    WasmCodegenError::new(format!("undefined variable: {}", name))
                })?.clone();
                // Push all locals for this variable (handles compound types)
                for i in 0..var_info.local_count {
                    ops.push(Operator::LocalGet { local_index: var_info.start_index + i as u32 });
                }
            }

            Expression::QualifiedIdentifier { namespace, name } => {
                // TODO: Handle qualified identifiers properly
                Err(WasmCodegenError::new(format!(
                    "qualified identifiers not yet supported: {}::{}",
                    namespace, name
                )))?;
            }

            Expression::Binary { op, left, right } => {
                self.generate_expression(left, ops)?;
                self.generate_expression(right, ops)?;
                
                // TODO: Proper type inference to choose i32/i64/f32/f64 variants
                // For now, assume i32 for integers and f64 for floats
                match op {
                    BinaryOp::Add => ops.push(Operator::I32Add),
                    BinaryOp::Sub => ops.push(Operator::I32Sub),
                    BinaryOp::Mul => ops.push(Operator::I32Mul),
                    BinaryOp::Div => ops.push(Operator::I32DivS),
                    BinaryOp::Mod => ops.push(Operator::I32RemS),
                    BinaryOp::BitAnd => ops.push(Operator::I32And),
                    BinaryOp::BitOr => ops.push(Operator::I32Or),
                    BinaryOp::BitXor => ops.push(Operator::I32Xor),
                    BinaryOp::Shl => ops.push(Operator::I32Shl),
                    BinaryOp::Shr => ops.push(Operator::I32ShrS),
                    BinaryOp::LogicalShr => ops.push(Operator::I32ShrU),
                    BinaryOp::Eq => ops.push(Operator::I32Eq),
                    BinaryOp::Ne => ops.push(Operator::I32Ne),
                    BinaryOp::Lt => ops.push(Operator::I32LtS),
                    BinaryOp::Le => ops.push(Operator::I32LeS),
                    BinaryOp::Gt => ops.push(Operator::I32GtS),
                    BinaryOp::Ge => ops.push(Operator::I32GeS),
                    BinaryOp::And => {
                        // Logical AND: both must be non-zero
                        // a && b => (a != 0) & (b != 0)
                        // But we've already pushed both, so we need:
                        // We want: if a then b else 0
                        // Simpler: multiply them (if either is 0, result is 0)
                        // But that's not quite right for bools...
                        // For now, use i32.and
                        ops.push(Operator::I32And);
                    }
                    BinaryOp::Or => {
                        // Logical OR: either must be non-zero
                        ops.push(Operator::I32Or);
                    }
                }
            }

            Expression::Unary { op, operand } => {
                self.generate_expression(operand, ops)?;
                
                match op {
                    UnaryOp::Neg => {
                        // -x = 0 - x
                        ops.push(Operator::I32Const { value: 0 });
                        // Need to swap: value is on stack, then 0
                        // Actually we pushed operand first, so stack is [operand]
                        // We need [0, operand] for sub
                        // Let's re-do: push 0, swap would be needed
                        // Simpler: use a temp
                        // Actually the correct way:
                        // 0 - x means we need 0 on stack first
                        // So: i32.const 0, then the value, then sub
                        // But we already pushed the value. Let's fix:
                        ops.pop(); // Remove the operand push
                        ops.push(Operator::I32Const { value: 0 });
                        self.generate_expression(operand, ops)?;
                        ops.push(Operator::I32Sub);
                    }
                    UnaryOp::BitNot => {
                        // ~x = x ^ -1
                        ops.push(Operator::I32Const { value: -1 });
                        ops.push(Operator::I32Xor);
                    }
                    UnaryOp::Not => {
                        // !x = x == 0
                        ops.push(Operator::I32Eqz);
                    }
                }
            }

            Expression::If { condition, then_branch, else_branch } => {
                self.generate_expression(condition, ops)?;
                
                // Determine result type - assume i32 for now
                ops.push(Operator::If {
                    blockty: BlockType::Type(ValType::I32),
                });
                self.label_depth += 1;
                
                self.generate_block(then_branch, ops)?;
                
                ops.push(Operator::Else);
                self.generate_block(else_branch, ops)?;
                
                ops.push(Operator::End);
                self.label_depth -= 1;
            }

            Expression::Operation { path, extern_fn, args } => {
                // Operations are external calls that must be resolved
                // The path represents the operation path (e.g., ["WRITE", "GPIO"])
                
                // For now, treat operations as imported functions
                // The operation name is constructed from the path
                let op_name = path.join("_");
                
                // Generate arguments first
                for arg in args {
                    self.generate_expression(arg, ops)?;
                }
                
                // If there's an extern_fn, call it with the result
                if let Some(ext_fn) = extern_fn {
                    let fn_idx = self.resolver.resolve_extern(ext_fn).ok_or_else(|| {
                        WasmCodegenError::new(format!("unresolved extern function: {}", ext_fn))
                    })?;
                    ops.push(Operator::Call { function_index: fn_idx });
                }
                
                // Then call the operation function
                let fn_idx = self.resolver.resolve_extern(&op_name).ok_or_else(|| {
                    WasmCodegenError::new(format!("unresolved operation: {}", op_name))
                })?;
                ops.push(Operator::Call { function_index: fn_idx });
            }

            Expression::Call { function, args } => {
                // Generate arguments
                for arg in args {
                    self.generate_expression(arg, ops)?;
                }
                
                // Resolve and call function
                let fn_idx = self.resolver.resolve_function(function).ok_or_else(|| {
                    WasmCodegenError::new(format!("unresolved function: {}", function))
                })?;
                ops.push(Operator::Call { function_index: fn_idx });
            }

            Expression::QualifiedCall { namespace, function, args: _ } => {
                // TODO: Handle qualified calls properly
                Err(WasmCodegenError::new(format!(
                    "qualified calls not yet supported: {}::{}",
                    namespace, function
                )))?;
            }

            Expression::Block(block) => {
                // Block expressions produce a value
                ops.push(Operator::Block {
                    blockty: BlockType::Type(ValType::I32), // Assume i32 for now
                });
                self.label_depth += 1;
                
                self.generate_block(block, ops)?;
                
                ops.push(Operator::End);
                self.label_depth -= 1;
            }

            Expression::Index { array, index } => {
                // Array indexing with splatted locals: arr[idx]
                // Need to generate a switch since WASM doesn't have computed local access.
                
                // First, check if 'array' is an identifier we can look up
                if let Expression::Identifier(name) = array.as_ref() {
                    let var_info = self.locals.get_var_info(name).ok_or_else(|| {
                        WasmCodegenError::new(format!("undefined array: {}", name))
                    })?.clone();
                    
                    // Get element count from the type
                    let elem_count = match &var_info.ty {
                        Type::Array(elem_ty, len) => {
                            let elem_locals = type_local_count(elem_ty);
                            if elem_locals != 1 {
                                return Err(WasmCodegenError::new(
                                    "nested compound types in arrays not yet supported"
                                ));
                            }
                            *len as usize
                        }
                        _ => return Err(WasmCodegenError::new(
                            format!("cannot index non-array type: {}", name)
                        )),
                    };
                    
                    // Generate index expression and store in temp
                    let idx_temp = self.locals.alloc_temp(ValType::I32);
                    self.generate_expression(index, ops)?;
                    ops.push(Operator::LocalSet { local_index: idx_temp });
                    
                    // Generate switch using br_table for efficiency
                    // Result will be stored in a temp local
                    let result_temp = self.locals.alloc_temp(ValType::I32);
                    
                    // Initialize result to 0 (default)
                    ops.push(Operator::I32Const { value: 0 });
                    ops.push(Operator::LocalSet { local_index: result_temp });
                    
                    // Generate switch: if idx == 0, get local[0]; else if idx == 1, get local[1]; etc.
                    for i in 0..elem_count {
                        ops.push(Operator::LocalGet { local_index: idx_temp });
                        ops.push(Operator::I32Const { value: i as i32 });
                        ops.push(Operator::I32Eq);
                        ops.push(Operator::If { blockty: BlockType::Empty });
                        ops.push(Operator::LocalGet { local_index: var_info.start_index + i as u32 });
                        ops.push(Operator::LocalSet { local_index: result_temp });
                        ops.push(Operator::End);
                    }
                    
                    // Push the result onto the stack
                    ops.push(Operator::LocalGet { local_index: result_temp });
                } else {
                    return Err(WasmCodegenError::new(
                        "array indexing only supported on identifiers, not expressions"
                    ));
                }
            }

            Expression::Array(elements) => {
                // Array literals: generate each element expression
                // The values are left on the stack in order for the caller to store
                // This works because arrays are splatted into multiple locals
                for elem in elements {
                    self.generate_expression(elem, ops)?;
                }
            }

            Expression::Cast { value, ty } => {
                self.generate_expression(value, ops)?;
                
                // Generate appropriate conversion
                let target_ty = type_to_wasm(ty);
                // TODO: Proper type tracking to know source type
                // For now, assume source is i32
                match target_ty {
                    ValType::I32 => {} // No-op if already i32
                    ValType::I64 => ops.push(Operator::I64ExtendI32S),
                    ValType::F32 => ops.push(Operator::F32ConvertI32S),
                    ValType::F64 => ops.push(Operator::F64ConvertI32S),
                    _ => {}
                }
            }
        }

        Ok(())
    }
}

/// Iterator adapter that yields WASM operators for a function.
///
/// This provides a convenient way to consume generated operators lazily,
/// though the current implementation generates all operators upfront.
#[derive(Debug)]
pub struct WasmOperatorIter {
    operators: Vec<Operator<'static>>,
    index: usize,
}

impl WasmOperatorIter {
    /// Create a new iterator from a vector of operators.
    pub fn new(operators: Vec<Operator<'static>>) -> Self {
        Self { operators, index: 0 }
    }
}

impl Iterator for WasmOperatorIter {
    type Item = Operator<'static>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.operators.len() {
            let op = self.operators[self.index].clone();
            self.index += 1;
            Some(op)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.operators.len() - self.index;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for WasmOperatorIter {}

/// Generate WASM operators for a function.
///
/// This is a convenience function that creates a codegen instance and
/// generates operators for the given function.
pub fn generate_function_operators<R: FunctionResolver>(
    func: &Function,
    resolver: &R,
) -> Result<(WasmOperatorIter, Vec<ValType>), WasmCodegenError> {
    let mut codegen = WasmCodegen::new(resolver);
    let (ops, locals) = codegen.generate_function(func)?;
    Ok((WasmOperatorIter::new(ops), locals))
}

#[cfg(test)]
mod tests {
    use super::*;
    use embeem_ast::{Block, Function, Literal, Param, PrimitiveType, Type};

    struct TestResolver;
    
    impl FunctionResolver for TestResolver {
        fn resolve_function(&self, name: &str) -> Option<u32> {
            match name {
                "test_fn" => Some(0),
                _ => None,
            }
        }
        
        fn resolve_extern(&self, name: &str) -> Option<u32> {
            match name {
                "GPIO_READ" => Some(100),
                "GPIO_WRITE" => Some(101),
                _ => None,
            }
        }
    }

    #[test]
    fn test_literal_integer() {
        let func = Function {
            name: "test".into(),
            params: vec![],
            return_type: Some(Type::Primitive(PrimitiveType::I32)),
            body: Block {
                statements: vec![],
                result: Some(Box::new(Expression::Literal(Literal::Integer(42)))),
            },
        };

        let resolver = TestResolver;
        let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
        let ops: Vec<_> = iter.collect();

        assert!(matches!(ops[0], Operator::I32Const { value: 42 }));
        assert!(matches!(ops[1], Operator::End));
    }

    #[test]
    fn test_binary_add() {
        let func = Function {
            name: "add".into(),
            params: vec![
                Param { name: "a".into(), ty: Type::Primitive(PrimitiveType::I32) },
                Param { name: "b".into(), ty: Type::Primitive(PrimitiveType::I32) },
            ],
            return_type: Some(Type::Primitive(PrimitiveType::I32)),
            body: Block {
                statements: vec![],
                result: Some(Box::new(Expression::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Expression::Identifier("a".into())),
                    right: Box::new(Expression::Identifier("b".into())),
                })),
            },
        };

        let resolver = TestResolver;
        let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
        let ops: Vec<_> = iter.collect();

        assert!(matches!(ops[0], Operator::LocalGet { local_index: 0 }));
        assert!(matches!(ops[1], Operator::LocalGet { local_index: 1 }));
        assert!(matches!(ops[2], Operator::I32Add));
        assert!(matches!(ops[3], Operator::End));
    }

    #[test]
    fn test_local_variable() {
        let func = Function {
            name: "test".into(),
            params: vec![],
            return_type: Some(Type::Primitive(PrimitiveType::I32)),
            body: Block {
                statements: vec![
                    Statement::Let {
                        name: "x".into(),
                        mutable: false,
                        ty: Some(Type::Primitive(PrimitiveType::I32)),
                        value: Expression::Literal(Literal::Integer(10)),
                    },
                ],
                result: Some(Box::new(Expression::Identifier("x".into()))),
            },
        };

        let resolver = TestResolver;
        let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
        let ops: Vec<_> = iter.collect();

        // Should have one additional local
        assert_eq!(locals.len(), 1);
        assert_eq!(locals[0], ValType::I32);

        // i32.const 10, local.set 0, local.get 0, end
        assert!(matches!(ops[0], Operator::I32Const { value: 10 }));
        assert!(matches!(ops[1], Operator::LocalSet { local_index: 0 }));
        assert!(matches!(ops[2], Operator::LocalGet { local_index: 0 }));
        assert!(matches!(ops[3], Operator::End));
    }
}
