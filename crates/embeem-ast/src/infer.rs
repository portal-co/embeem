//! Type inference for Embeem expressions.
//!
//! This module provides type inference functionality to determine the type of
//! expressions based on their structure and context.

use alloc::collections::BTreeMap;
use alloc::string::String;

use crate::{
    BinaryOp, Block, Expression, Function, Literal, PrimitiveType, Program, Statement,
    Type, UnaryOp,
};

/// Type inference context containing variable bindings.
#[derive(Clone, Debug, Default)]
pub struct TypeContext {
    /// Variable name to type mapping.
    variables: BTreeMap<String, Type>,
    /// Function name to return type mapping.
    functions: BTreeMap<String, Option<Type>>,
    /// Constant name to type mapping.
    constants: BTreeMap<String, Type>,
}

impl TypeContext {
    /// Create a new empty type context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a type context from a program, collecting function and constant types.
    pub fn from_program(program: &Program) -> Self {
        let mut ctx = Self::new();

        // Add constants
        for constant in &program.constants {
            ctx.constants
                .insert(constant.name.clone(), constant.ty.clone());
        }

        // Add functions
        for function in &program.functions {
            ctx.functions
                .insert(function.name.clone(), function.return_type.clone());
        }

        ctx
    }

    /// Add a variable binding to the context.
    pub fn add_variable(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    /// Get the type of a variable.
    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    /// Get the type of a constant.
    pub fn get_constant(&self, name: &str) -> Option<&Type> {
        self.constants.get(name)
    }

    /// Get the return type of a function.
    pub fn get_function_return_type(&self, name: &str) -> Option<Option<&Type>> {
        self.functions.get(name).map(|t| t.as_ref())
    }

    /// Clone the context for a new scope.
    pub fn clone_for_scope(&self) -> Self {
        self.clone()
    }
}

/// Infer the type of an expression given a type context.
///
/// Returns `None` if the type cannot be inferred.
pub fn infer_expression_type(expr: &Expression, ctx: &TypeContext) -> Option<Type> {
    match expr {
        Expression::Literal(lit) => Some(infer_literal_type(lit)),

        Expression::Identifier(name) => ctx
            .get_variable(name)
            .or_else(|| ctx.get_constant(name))
            .cloned(),

        Expression::Binary { op, left, right } => {
            let left_ty = infer_expression_type(left, ctx)?;
            let right_ty = infer_expression_type(right, ctx)?;
            infer_binary_op_type(*op, &left_ty, &right_ty)
        }

        Expression::Unary { op, operand } => {
            let operand_ty = infer_expression_type(operand, ctx)?;
            infer_unary_op_type(*op, &operand_ty)
        }

        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            // The type of an if expression is the type of the branches
            // Both branches must have the same type
            let then_ty = infer_block_type(then_branch, ctx);
            let else_ty = infer_block_type(else_branch, ctx);
            
            match (then_ty, else_ty) {
                (Some(t1), Some(t2)) if t1 == t2 => Some(t1),
                (Some(t), None) | (None, Some(t)) => Some(t),
                (None, None) => None,
                // Types differ - cannot infer
                _ => None,
            }
        }

        Expression::Operation { path, args } => infer_operation_type(path, args, ctx),

        Expression::Call { function, .. } => ctx
            .get_function_return_type(function)
            .and_then(|opt_ty| opt_ty.cloned()),

        Expression::Block(block) => infer_block_type(block, ctx),

        Expression::Index { array, .. } => {
            // The type of an array index is the element type
            let array_ty = infer_expression_type(array, ctx)?;
            match array_ty {
                Type::Array(elem_ty, _) => Some(*elem_ty),
                _ => None,
            }
        }

        Expression::Array(elements) => {
            if elements.is_empty() {
                None
            } else {
                let elem_ty = infer_expression_type(&elements[0], ctx)?;
                Some(Type::Array(
                    alloc::boxed::Box::new(elem_ty),
                    elements.len() as u64,
                ))
            }
        }

        Expression::Cast { ty, .. } => Some(ty.clone()),
    }
}

/// Infer the type of a literal.
pub fn infer_literal_type(lit: &Literal) -> Type {
    match lit {
        Literal::Integer(_) => Type::Primitive(PrimitiveType::U64),
        Literal::Float(_) => Type::Primitive(PrimitiveType::F64),
        Literal::Bool(_) => Type::Primitive(PrimitiveType::Bool),
    }
}

/// Infer the type of a block.
pub fn infer_block_type(block: &Block, ctx: &TypeContext) -> Option<Type> {
    // Create a new context for the block scope
    let mut block_ctx = ctx.clone_for_scope();

    // Process statements to update the context with let bindings
    for stmt in &block.statements {
        if let Statement::Let { name, ty, value, .. } = stmt {
            let var_ty = if let Some(explicit_ty) = ty {
                explicit_ty.clone()
            } else {
                infer_expression_type(value, &block_ctx)?
            };
            block_ctx.add_variable(name.clone(), var_ty);
        }
    }

    // The type of a block is the type of its result expression
    block
        .result
        .as_ref()
        .and_then(|expr| infer_expression_type(expr, &block_ctx))
}

/// Infer the type of a binary operation.
fn infer_binary_op_type(op: BinaryOp, left: &Type, right: &Type) -> Option<Type> {
    match op {
        // Comparison operations always return bool
        BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            Some(Type::Primitive(PrimitiveType::Bool))
        }

        // Logical operations return bool
        BinaryOp::And | BinaryOp::Or => Some(Type::Primitive(PrimitiveType::Bool)),

        // Arithmetic and bitwise operations preserve the type
        BinaryOp::Add
        | BinaryOp::Sub
        | BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Mod
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor => {
            // Use the wider type or find a common compatible type
            unify_types(left, right)
        }

        // Shift operations preserve the left operand type
        BinaryOp::Shl | BinaryOp::Shr | BinaryOp::LogicalShr => Some(left.clone()),
    }
}

/// Infer the type of a unary operation.
fn infer_unary_op_type(op: UnaryOp, operand: &Type) -> Option<Type> {
    match op {
        UnaryOp::Neg => Some(operand.clone()),
        UnaryOp::BitNot => Some(operand.clone()),
        UnaryOp::Not => Some(Type::Primitive(PrimitiveType::Bool)),
    }
}

/// Attempt to unify two types, returning a common compatible type.
/// Returns the wider type for numeric types, or the same type if they are equal.
fn unify_types(left: &Type, right: &Type) -> Option<Type> {
    if left == right {
        return Some(left.clone());
    }
    widen_numeric_types(left, right)
}

/// Widen numeric types to find a common type.
fn widen_numeric_types(left: &Type, right: &Type) -> Option<Type> {
    match (left, right) {
        (Type::Primitive(l), Type::Primitive(r)) => {
            let wider = widen_primitives(*l, *r)?;
            Some(Type::Primitive(wider))
        }
        _ => None,
    }
}

/// Widen two primitive types to find the wider one.
fn widen_primitives(left: PrimitiveType, right: PrimitiveType) -> Option<PrimitiveType> {
    use PrimitiveType::*;

    // Define the widening order for numeric types
    let rank = |p: PrimitiveType| -> Option<u8> {
        match p {
            Bool => Some(0),
            U8 => Some(1),
            I8 => Some(2),
            U16 => Some(3),
            I16 => Some(4),
            U32 => Some(5),
            I32 => Some(6),
            U64 => Some(7),
            I64 => Some(8),
            F32 => Some(9),
            F64 => Some(10),
        }
    };

    let l_rank = rank(left)?;
    let r_rank = rank(right)?;

    if l_rank >= r_rank {
        Some(left)
    } else {
        Some(right)
    }
}

/// Infer the type of an operation.
///
/// For operations that mirror binary/unary operations (ADD, SUB, etc.), we use
/// argument-based type inference to determine the result type. Other operations
/// have fixed return types based on their semantics.
///
/// The path is a list of UPPER_SNAKE_CASE strings representing the operation.
/// For type inference, we join the path with underscores to match against known
/// operation patterns.
fn infer_operation_type(path: &[String], args: &[Expression], ctx: &TypeContext) -> Option<Type> {
    // Join the path to create an operation name
    let op_name = path.join("_");
    let op = op_name.as_str();
    
    match op {
        // Binary arithmetic operations - infer from arguments
        "ADD" | "SUB" | "MUL" | "DIV" | "MOD" => {
            if args.len() == 2 {
                let left_ty = infer_expression_type(&args[0], ctx)?;
                let right_ty = infer_expression_type(&args[1], ctx)?;
                return unify_types(&left_ty, &right_ty);
            }
            Some(Type::Primitive(PrimitiveType::U64))
        }

        // Unary arithmetic operations - infer from argument
        "INC" | "DEC" | "NEG" | "ABS" => {
            if args.len() == 1 {
                return infer_expression_type(&args[0], ctx);
            }
            Some(Type::Primitive(PrimitiveType::U64))
        }

        // Binary bitwise operations - infer from arguments (unify both types)
        "AND" | "OR" | "XOR" => {
            if args.len() == 2 {
                let left_ty = infer_expression_type(&args[0], ctx)?;
                let right_ty = infer_expression_type(&args[1], ctx)?;
                return unify_types(&left_ty, &right_ty);
            }
            Some(Type::Primitive(PrimitiveType::U64))
        }

        // Shift and rotate operations - result type matches left operand
        "SHL" | "SHR" | "SAR" | "ROL" | "ROR" => {
            if args.len() == 2 {
                return infer_expression_type(&args[0], ctx);
            }
            Some(Type::Primitive(PrimitiveType::U64))
        }

        // Unary bitwise operation - infer from argument
        "NOT" => {
            if args.len() == 1 {
                return infer_expression_type(&args[0], ctx);
            }
            Some(Type::Primitive(PrimitiveType::U64))
        }

        // Comparison operations always return bool
        "EQ" | "NE" | "LT" | "LE" | "GT" | "GE" | "CMP" => {
            Some(Type::Primitive(PrimitiveType::Bool))
        }

        // Floating point binary operations - infer from arguments
        "FADD" | "FSUB" | "FMUL" | "FDIV" => {
            if args.len() == 2 {
                let left_ty = infer_expression_type(&args[0], ctx)?;
                let right_ty = infer_expression_type(&args[1], ctx)?;
                return unify_types(&left_ty, &right_ty);
            }
            Some(Type::Primitive(PrimitiveType::F64))
        }

        // Floating point unary operations - infer from argument
        "FSQRT" | "FABS" => {
            if args.len() == 1 {
                return infer_expression_type(&args[0], ctx);
            }
            Some(Type::Primitive(PrimitiveType::F64))
        }

        // Floating point comparison
        "FCMP" => Some(Type::Primitive(PrimitiveType::I32)),

        // Bit manipulation - preserve input type for set/clear/toggle
        "SET_BIT" | "CLEAR_BIT" | "TOGGLE_BIT" => {
            if !args.is_empty() {
                return infer_expression_type(&args[0], ctx);
            }
            Some(Type::Primitive(PrimitiveType::U64))
        }

        // Test operations return bool
        "TEST" | "TEST_BIT" => Some(Type::Primitive(PrimitiveType::Bool)),

        // Bit counting operations return u8
        "COUNT_ONES" | "COUNT_ZEROS" | "FIND_FIRST_SET" | "FIND_FIRST_ZERO" => {
            Some(Type::Primitive(PrimitiveType::U8))
        }

        // GPIO read returns u8 (0 or 1) - handles both GPIO_READ and READ_GPIO
        "GPIO_READ" | "READ_GPIO" => Some(Type::Primitive(PrimitiveType::U8)),

        // ADC read typically returns a value based on resolution (default to u16)
        "ADC_READ" | "ADC_READ_MULTI" | "READ_ADC" => Some(Type::Primitive(PrimitiveType::U16)),

        // Timer read operations
        "TIMER_READ" | "READ_TIMER" | "GET_MILLIS" | "GET_MICROS" => {
            Some(Type::Primitive(PrimitiveType::U64))
        }

        // UART operations - handles both traditional and path-based
        "UART_READ_BYTE" | "READ_UART" => Some(Type::Primitive(PrimitiveType::U8)),
        "UART_AVAILABLE" | "AVAILABLE_UART" => Some(Type::Primitive(PrimitiveType::Bool)),

        // I2C read - handles both traditional and path-based
        "I2C_READ" | "I2C_READ_FROM" | "READ_I2C" => Some(Type::Primitive(PrimitiveType::U8)),

        // RTC get time
        "RTC_GET_TIME" | "READ_RTC" => Some(Type::Primitive(PrimitiveType::U64)),

        // Default to u64 for most other operations
        _ => Some(Type::Primitive(PrimitiveType::U64)),
    }
}

/// Infer types for all let statements in a function that don't have explicit types.
pub fn infer_function_types(function: &Function, program_ctx: &TypeContext) -> TypeContext {
    let mut ctx = program_ctx.clone_for_scope();

    // Add function parameters to context
    for param in &function.params {
        ctx.add_variable(param.name.clone(), param.ty.clone());
    }

    // Process the function body
    infer_block_types(&function.body, &mut ctx);

    ctx
}

/// Recursively process a block to infer types.
fn infer_block_types(block: &Block, ctx: &mut TypeContext) {
    for stmt in &block.statements {
        match stmt {
            Statement::Let { name, ty, value, .. } => {
                let var_ty = if let Some(explicit_ty) = ty {
                    explicit_ty.clone()
                } else {
                    infer_expression_type(value, ctx)
                        .unwrap_or(Type::Primitive(PrimitiveType::U64))
                };
                ctx.add_variable(name.clone(), var_ty);
            }
            Statement::If {
                then_block,
                else_block,
                ..
            } => {
                // Process nested blocks with a cloned context
                let mut then_ctx = ctx.clone_for_scope();
                infer_block_types(then_block, &mut then_ctx);

                if let Some(else_blk) = else_block {
                    match else_blk {
                        crate::ElseBlock::Block(blk) => {
                            let mut else_ctx = ctx.clone_for_scope();
                            infer_block_types(blk, &mut else_ctx);
                        }
                        crate::ElseBlock::ElseIf(stmt) => {
                            if let Statement::If {
                                then_block,
                                else_block,
                                ..
                            } = stmt.as_ref()
                            {
                                let mut nested_ctx = ctx.clone_for_scope();
                                infer_block_types(then_block, &mut nested_ctx);
                                if let Some(crate::ElseBlock::Block(blk)) = else_block {
                                    infer_block_types(blk, &mut nested_ctx);
                                }
                            }
                        }
                    }
                }
            }
            Statement::For { body, .. } => {
                let mut body_ctx = ctx.clone_for_scope();
                infer_block_types(body, &mut body_ctx);
            }
            Statement::Repeat { body, .. } => {
                let mut body_ctx = ctx.clone_for_scope();
                infer_block_types(body, &mut body_ctx);
            }
            Statement::While { body, .. } => {
                let mut body_ctx = ctx.clone_for_scope();
                infer_block_types(body, &mut body_ctx);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_infer_literal_types() {
        assert_eq!(
            infer_literal_type(&Literal::Integer(42)),
            Type::Primitive(PrimitiveType::U64)
        );
        assert_eq!(
            infer_literal_type(&Literal::Float(3.14)),
            Type::Primitive(PrimitiveType::F64)
        );
        assert_eq!(
            infer_literal_type(&Literal::Bool(true)),
            Type::Primitive(PrimitiveType::Bool)
        );
    }

    #[test]
    fn test_infer_identifier() {
        let mut ctx = TypeContext::new();
        ctx.add_variable("x".into(), Type::Primitive(PrimitiveType::U32));

        let expr = Expression::Identifier("x".into());
        assert_eq!(
            infer_expression_type(&expr, &ctx),
            Some(Type::Primitive(PrimitiveType::U32))
        );
    }

    #[test]
    fn test_infer_comparison() {
        let ctx = TypeContext::new();
        let expr = Expression::Binary {
            op: BinaryOp::Gt,
            left: alloc::boxed::Box::new(Expression::Literal(Literal::Integer(10))),
            right: alloc::boxed::Box::new(Expression::Literal(Literal::Integer(5))),
        };
        assert_eq!(
            infer_expression_type(&expr, &ctx),
            Some(Type::Primitive(PrimitiveType::Bool))
        );
    }

    #[test]
    fn test_infer_cast() {
        let ctx = TypeContext::new();
        let expr = Expression::Cast {
            value: alloc::boxed::Box::new(Expression::Literal(Literal::Integer(42))),
            ty: Type::Primitive(PrimitiveType::U8),
        };
        assert_eq!(
            infer_expression_type(&expr, &ctx),
            Some(Type::Primitive(PrimitiveType::U8))
        );
    }
}
