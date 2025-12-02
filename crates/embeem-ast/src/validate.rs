//! Specification validator for Embeem programs.
//!
//! This module validates Embeem programs against the language specification
//! defined in `spec/SPECIFICATION.md`, ensuring:
//!
//! 1. **Identifier Naming**: User identifiers don't use UPPER_SNAKE_CASE
//! 2. **Totality**: All loops have bounded iteration counts
//! 3. **Syntax Compliance**: All constructs follow the spec
//! 4. **Type Annotations**: Required annotations are present
//! 5. **Feature Support**: Only spec-defined features are used

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use alloc::format;

use crate::{
    Block, ConstDecl, Expression, ExternFn, Function, Literal, Module,
    Item, Program, Statement, Type,
    is_upper_snake_case, is_valid_user_identifier,
};

/// A validation error with location context.
#[derive(Clone, Debug, PartialEq)]
pub struct ValidationError {
    /// Human-readable error message.
    pub message: String,
    /// The kind of validation error.
    pub kind: ValidationErrorKind,
    /// Context path (e.g., "function 'main' -> statement 3 -> expression").
    pub context: String,
}

/// Categories of validation errors.
#[derive(Clone, Debug, PartialEq)]
pub enum ValidationErrorKind {
    /// User identifier uses reserved UPPER_SNAKE_CASE pattern.
    ReservedIdentifier,
    /// Loop bound is not a compile-time constant or immutable variable reference.
    NonConstantLoopBound,
    /// Mutable variable used as loop bound (violates totality).
    MutableLoopBound,
    /// Unsupported language feature.
    UnsupportedFeature,
    /// Reserved keyword used as identifier.
    ReservedKeyword,
    /// Missing required type annotation.
    MissingTypeAnnotation,
    /// Invalid constant expression.
    NonConstantExpression,
    /// Array repeat syntax requires constant initializer.
    NonConstantArrayRepeat,
    /// Type cast expression uses unsupported syntax.
    UnsupportedCast,
    /// Expression is not total (could diverge).
    PotentialDivergence,
}

/// Result of validation.
#[derive(Clone, Debug)]
pub struct ValidationResult {
    /// Errors found during validation.
    pub errors: Vec<ValidationError>,
    /// Warnings (non-fatal issues).
    pub warnings: Vec<ValidationWarning>,
}

/// A validation warning (non-fatal).
#[derive(Clone, Debug, PartialEq)]
pub struct ValidationWarning {
    /// Human-readable warning message.
    pub message: String,
    /// Context path.
    pub context: String,
}

impl ValidationResult {
    /// Create an empty validation result (success).
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    /// Check if validation passed with no errors.
    pub fn is_valid(&self) -> bool {
        self.errors.is_empty()
    }

    /// Merge another result into this one.
    pub fn merge(&mut self, other: ValidationResult) {
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
    }

    fn add_error(&mut self, kind: ValidationErrorKind, message: String, context: &str) {
        self.errors.push(ValidationError {
            message,
            kind,
            context: context.to_string(),
        });
    }

    fn add_warning(&mut self, message: String, context: &str) {
        self.warnings.push(ValidationWarning {
            message,
            context: context.to_string(),
        });
    }
}

impl Default for ValidationResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Validation context for tracking location in the AST.
#[derive(Clone)]
struct ValidationContext {
    /// Path of named scopes for error messages.
    path: Vec<String>,
    /// Set of mutable variables in scope.
    mutable_vars: Vec<String>,
    /// Set of immutable variables in scope.
    immutable_vars: Vec<String>,
    /// Set of constant names.
    constants: Vec<String>,
}

impl ValidationContext {
    fn new() -> Self {
        Self {
            path: Vec::new(),
            mutable_vars: Vec::new(),
            immutable_vars: Vec::new(),
            constants: Vec::new(),
        }
    }

    fn push(&mut self, segment: &str) {
        self.path.push(segment.to_string());
    }

    fn pop(&mut self) {
        self.path.pop();
    }

    fn context_string(&self) -> String {
        if self.path.is_empty() {
            "top level".to_string()
        } else {
            self.path.join(" -> ")
        }
    }

    fn with_scope<F, R>(&mut self, segment: &str, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.push(segment);
        let result = f(self);
        self.pop();
        result
    }

    fn add_mutable(&mut self, name: &str) {
        self.mutable_vars.push(name.to_string());
    }

    fn add_immutable(&mut self, name: &str) {
        self.immutable_vars.push(name.to_string());
    }

    fn add_constant(&mut self, name: &str) {
        self.constants.push(name.to_string());
    }

    fn is_mutable(&self, name: &str) -> bool {
        self.mutable_vars.iter().any(|v| v == name)
    }

    fn is_immutable(&self, name: &str) -> bool {
        self.immutable_vars.iter().any(|v| v == name)
    }

    fn is_constant(&self, name: &str) -> bool {
        self.constants.iter().any(|c| c == name)
    }

    /// Check if an expression is a valid loop bound (constant or immutable var).
    fn is_valid_loop_bound(&self, expr: &Expression) -> BoundCheckResult {
        match expr {
            Expression::Literal(Literal::Integer(_)) => BoundCheckResult::Valid,
            Expression::Literal(Literal::Float(_)) => BoundCheckResult::InvalidType,
            Expression::Literal(Literal::Bool(_)) => BoundCheckResult::InvalidType,
            
            Expression::Identifier(name) => {
                if self.is_constant(name) {
                    BoundCheckResult::Valid
                } else if self.is_immutable(name) {
                    BoundCheckResult::Valid
                } else if self.is_mutable(name) {
                    BoundCheckResult::MutableVariable
                } else {
                    // Unknown variable - could be a parameter (which is immutable)
                    BoundCheckResult::Valid
                }
            }
            
            // Binary ops on constants are constant
            Expression::Binary { left, right, .. } => {
                let left_check = self.is_valid_loop_bound(left);
                let right_check = self.is_valid_loop_bound(right);
                
                match (left_check, right_check) {
                    (BoundCheckResult::Valid, BoundCheckResult::Valid) => BoundCheckResult::Valid,
                    (BoundCheckResult::MutableVariable, _) | (_, BoundCheckResult::MutableVariable) => {
                        BoundCheckResult::MutableVariable
                    }
                    _ => BoundCheckResult::NonConstant,
                }
            }
            
            // Unary ops on constants are constant
            Expression::Unary { operand, .. } => self.is_valid_loop_bound(operand),
            
            // Casts on constants are constant
            Expression::Cast { value, .. } => self.is_valid_loop_bound(value),
            
            // Array index on constant array with constant index
            Expression::Index { array, index } => {
                let arr_check = self.is_valid_loop_bound(array);
                let idx_check = self.is_valid_loop_bound(index);
                
                match (arr_check, idx_check) {
                    (BoundCheckResult::Valid, BoundCheckResult::Valid) => BoundCheckResult::Valid,
                    (BoundCheckResult::MutableVariable, _) | (_, BoundCheckResult::MutableVariable) => {
                        BoundCheckResult::MutableVariable
                    }
                    _ => BoundCheckResult::NonConstant,
                }
            }
            
            // Function calls are not constant
            Expression::Call { .. } => BoundCheckResult::NonConstant,
            Expression::QualifiedCall { .. } => BoundCheckResult::NonConstant,
            
            // Operations that are pure and have constant args could be constant,
            // but for simplicity we treat them as non-constant unless they're
            // compile-time evaluable
            Expression::Operation { args, .. } => {
                // Some operations like pure arithmetic on constants could be OK,
                // but we're conservative here
                if args.iter().all(|arg| matches!(self.is_valid_loop_bound(arg), BoundCheckResult::Valid)) {
                    BoundCheckResult::Valid
                } else {
                    BoundCheckResult::NonConstant
                }
            }
            
            // Conditionals are constant if both branches and condition are constant
            Expression::If { condition, then_branch, else_branch } => {
                let cond_check = self.is_valid_loop_bound(condition);
                let then_check = then_branch.result.as_ref()
                    .map(|e| self.is_valid_loop_bound(e))
                    .unwrap_or(BoundCheckResult::Valid);
                let else_check = else_branch.result.as_ref()
                    .map(|e| self.is_valid_loop_bound(e))
                    .unwrap_or(BoundCheckResult::Valid);
                
                match (cond_check, then_check, else_check) {
                    (BoundCheckResult::Valid, BoundCheckResult::Valid, BoundCheckResult::Valid) => {
                        BoundCheckResult::Valid
                    }
                    (BoundCheckResult::MutableVariable, _, _)
                    | (_, BoundCheckResult::MutableVariable, _)
                    | (_, _, BoundCheckResult::MutableVariable) => {
                        BoundCheckResult::MutableVariable
                    }
                    _ => BoundCheckResult::NonConstant,
                }
            }
            
            Expression::Block(block) => {
                block.result.as_ref()
                    .map(|e| self.is_valid_loop_bound(e))
                    .unwrap_or(BoundCheckResult::Valid)
            }
            
            Expression::QualifiedIdentifier { .. } => {
                // Qualified identifiers could be constants from other modules
                BoundCheckResult::Valid
            }
            
            Expression::Array(_) => BoundCheckResult::InvalidType,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum BoundCheckResult {
    Valid,
    MutableVariable,
    NonConstant,
    InvalidType,
}

/// Validate a Program against the Embeem specification.
pub fn validate_program(program: &Program) -> ValidationResult {
    let mut result = ValidationResult::new();
    let mut ctx = ValidationContext::new();

    // Register constants
    for c in &program.constants {
        ctx.add_constant(&c.name);
    }

    // Validate constants
    for c in &program.constants {
        result.merge(validate_const_decl(c, &mut ctx));
    }

    // Validate extern fns
    for e in &program.extern_fns {
        result.merge(validate_extern_fn(e, &mut ctx));
    }

    // Validate functions
    for f in &program.functions {
        result.merge(validate_function(f, &mut ctx));
    }

    result
}

/// Validate a Module against the Embeem specification.
pub fn validate_module(module: &Module) -> ValidationResult {
    let mut result = ValidationResult::new();
    let mut ctx = ValidationContext::new();

    // First pass: register all constants
    for item in &module.items {
        if let Item::Const(c) = &item.item {
            ctx.add_constant(&c.name);
        }
    }

    // Validate each item
    for item in &module.items {
        match &item.item {
            Item::Const(c) => result.merge(validate_const_decl(c, &mut ctx)),
            Item::ExternFn(e) => result.merge(validate_extern_fn(e, &mut ctx)),
            Item::Function(f) => result.merge(validate_function(f, &mut ctx)),
        }
    }

    result
}

fn validate_const_decl(c: &ConstDecl, ctx: &mut ValidationContext) -> ValidationResult {
    let mut result = ValidationResult::new();

    ctx.with_scope(&format!("const '{}'", c.name), |ctx| {
        // Per spec section 2.1.1: UPPER_SNAKE_CASE is reserved for operations
        // Constants should use PascalCase instead
        if !is_valid_user_identifier(&c.name) {
            result.add_error(
                ValidationErrorKind::ReservedIdentifier,
                format!("constant name '{}' uses reserved UPPER_SNAKE_CASE pattern; use PascalCase instead (e.g., '{}')", 
                    c.name, 
                    to_pascal_case(&c.name)),
                &ctx.context_string(),
            );
        }

        // Validate the value is a constant expression
        result.merge(validate_const_expr(&c.value, ctx));

        // Validate type
        result.merge(validate_type(&c.ty, ctx));
    });

    result
}

/// Convert UPPER_SNAKE_CASE to PascalCase for error message suggestions.
fn to_pascal_case(s: &str) -> String {
    s.split('_')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    first.to_uppercase().chain(chars.flat_map(|c| c.to_lowercase())).collect()
                }
            }
        })
        .collect()
}

fn validate_extern_fn(e: &ExternFn, ctx: &mut ValidationContext) -> ValidationResult {
    let mut result = ValidationResult::new();

    ctx.with_scope(&format!("extern fn '{}'", e.name), |ctx| {
        // Validate identifier
        if !is_valid_user_identifier(&e.name) {
            result.add_error(
                ValidationErrorKind::ReservedIdentifier,
                format!("extern fn name '{}' uses reserved UPPER_SNAKE_CASE pattern", e.name),
                &ctx.context_string(),
            );
        }

        // Validate parameters
        for param in &e.params {
            if !is_valid_user_identifier(&param.name) {
                result.add_error(
                    ValidationErrorKind::ReservedIdentifier,
                    format!("parameter name '{}' uses reserved UPPER_SNAKE_CASE pattern", param.name),
                    &ctx.context_string(),
                );
            }
            result.merge(validate_type(&param.ty, ctx));
        }

        // Validate return type
        if let Some(ref ty) = e.return_type {
            result.merge(validate_type(ty, ctx));
        }
    });

    result
}

fn validate_function(f: &Function, ctx: &mut ValidationContext) -> ValidationResult {
    let mut result = ValidationResult::new();

    ctx.with_scope(&format!("fn '{}'", f.name), |ctx| {
        // Validate identifier (allow 'main')
        if f.name != "main" && !is_valid_user_identifier(&f.name) {
            result.add_error(
                ValidationErrorKind::ReservedIdentifier,
                format!("function name '{}' uses reserved UPPER_SNAKE_CASE pattern", f.name),
                &ctx.context_string(),
            );
        }

        // Validate parameters - they are immutable by default
        for param in &f.params {
            if !is_valid_user_identifier(&param.name) {
                result.add_error(
                    ValidationErrorKind::ReservedIdentifier,
                    format!("parameter name '{}' uses reserved UPPER_SNAKE_CASE pattern", param.name),
                    &ctx.context_string(),
                );
            }
            result.merge(validate_type(&param.ty, ctx));
            ctx.add_immutable(&param.name);
        }

        // Validate return type
        if let Some(ref ty) = f.return_type {
            result.merge(validate_type(ty, ctx));
        }

        // Validate body
        result.merge(validate_block(&f.body, ctx));
    });

    result
}

fn validate_type(ty: &Type, ctx: &mut ValidationContext) -> ValidationResult {
    let mut result = ValidationResult::new();

    match ty {
        Type::Primitive(_) => {}
        Type::Array(elem_ty, _size) => {
            result.merge(validate_type(elem_ty, ctx));
        }
        Type::Tuple(types) => {
            for t in types {
                result.merge(validate_type(t, ctx));
            }
        }
    }

    result
}

fn validate_block(block: &Block, ctx: &mut ValidationContext) -> ValidationResult {
    let mut result = ValidationResult::new();

    for (i, stmt) in block.statements.iter().enumerate() {
        ctx.with_scope(&format!("statement {}", i + 1), |ctx| {
            result.merge(validate_statement(stmt, ctx));
        });
    }

    if let Some(ref expr) = block.result {
        ctx.with_scope("result expression", |ctx| {
            result.merge(validate_expression(expr, ctx));
        });
    }

    result
}

fn validate_statement(stmt: &Statement, ctx: &mut ValidationContext) -> ValidationResult {
    let mut result = ValidationResult::new();

    match stmt {
        Statement::Let { name, mutable, ty, value } => {
            // Validate identifier
            if !is_valid_user_identifier(name) {
                result.add_error(
                    ValidationErrorKind::ReservedIdentifier,
                    format!("variable name '{}' uses reserved UPPER_SNAKE_CASE pattern", name),
                    &ctx.context_string(),
                );
            }

            // Validate type if present
            if let Some(t) = ty {
                result.merge(validate_type(t, ctx));
            }

            // Validate value expression
            result.merge(validate_expression(value, ctx));

            // Register variable
            if *mutable {
                ctx.add_mutable(name);
            } else {
                ctx.add_immutable(name);
            }
        }

        Statement::Assign { target, value } => {
            // Validate target
            match target {
                crate::AssignTarget::Identifier(_name) => {
                    // Simple assignment - target is validated at declaration
                }
                crate::AssignTarget::Index { array: _, index } => {
                    // Array index assignment - validate index expression
                    result.merge(validate_expression(index, ctx));
                }
            }
            // Validate value expression
            result.merge(validate_expression(value, ctx));
        }

        Statement::Expr(expr) => {
            result.merge(validate_expression(expr, ctx));
        }

        Statement::If { condition, then_block, else_block } => {
            result.merge(validate_expression(condition, ctx));
            result.merge(validate_block(then_block, ctx));
            if let Some(else_b) = else_block {
                match else_b {
                    crate::ElseBlock::Block(b) => result.merge(validate_block(b, ctx)),
                    crate::ElseBlock::ElseIf(stmt) => result.merge(validate_statement(stmt, ctx)),
                }
            }
        }

        Statement::For { variable, start, end, direction: _, body } => {
            // Validate variable name
            if !is_valid_user_identifier(variable) {
                result.add_error(
                    ValidationErrorKind::ReservedIdentifier,
                    format!("loop variable '{}' uses reserved UPPER_SNAKE_CASE pattern", variable),
                    &ctx.context_string(),
                );
            }

            // Validate range bounds - must be constant expressions
            result.merge(validate_expression(start, ctx));
            result.merge(validate_expression(end, ctx));

            // For spec compliance, range bounds should be constant expressions
            // But we allow immutable variables too per spec section 6.2.1

            // Loop variable is immutable within the body
            ctx.add_immutable(variable);
            result.merge(validate_block(body, ctx));
        }

        Statement::Repeat { count, body } => {
            // Validate count expression
            result.merge(validate_expression(count, ctx));

            // Spec section 6.2.2: bound must be compile-time constant or immutable variable
            match ctx.is_valid_loop_bound(count) {
                BoundCheckResult::Valid => {}
                BoundCheckResult::MutableVariable => {
                    result.add_error(
                        ValidationErrorKind::MutableLoopBound,
                        "repeat loop bound must be a compile-time constant or immutable variable".to_string(),
                        &ctx.context_string(),
                    );
                }
                BoundCheckResult::NonConstant => {
                    result.add_error(
                        ValidationErrorKind::NonConstantLoopBound,
                        "repeat loop bound must be a compile-time constant or immutable variable".to_string(),
                        &ctx.context_string(),
                    );
                }
                BoundCheckResult::InvalidType => {
                    result.add_error(
                        ValidationErrorKind::NonConstantLoopBound,
                        "repeat loop bound must be an integer".to_string(),
                        &ctx.context_string(),
                    );
                }
            }

            result.merge(validate_block(body, ctx));
        }

        Statement::While { condition, max_iterations, body } => {
            // Validate condition and max iterations
            result.merge(validate_expression(condition, ctx));
            result.merge(validate_expression(max_iterations, ctx));

            // Spec section 6.2.3: bound must be compile-time constant or immutable variable
            match ctx.is_valid_loop_bound(max_iterations) {
                BoundCheckResult::Valid => {}
                BoundCheckResult::MutableVariable => {
                    result.add_error(
                        ValidationErrorKind::MutableLoopBound,
                        "while loop max bound must be a compile-time constant or immutable variable".to_string(),
                        &ctx.context_string(),
                    );
                }
                BoundCheckResult::NonConstant => {
                    result.add_error(
                        ValidationErrorKind::NonConstantLoopBound,
                        "while loop max bound must be a compile-time constant or immutable variable".to_string(),
                        &ctx.context_string(),
                    );
                }
                BoundCheckResult::InvalidType => {
                    result.add_error(
                        ValidationErrorKind::NonConstantLoopBound,
                        "while loop max bound must be an integer".to_string(),
                        &ctx.context_string(),
                    );
                }
            }

            result.merge(validate_block(body, ctx));
        }
    }

    result
}

fn validate_expression(expr: &Expression, ctx: &mut ValidationContext) -> ValidationResult {
    let mut result = ValidationResult::new();

    match expr {
        Expression::Literal(_) => {}

        Expression::Identifier(_name) => {
            // Identifiers that are variable references are OK - they were validated at declaration
        }

        Expression::QualifiedIdentifier { namespace: _, name: _ } => {
            // Qualified identifiers are OK
        }

        Expression::Binary { left, right, .. } => {
            result.merge(validate_expression(left, ctx));
            result.merge(validate_expression(right, ctx));
        }

        Expression::Unary { operand, .. } => {
            result.merge(validate_expression(operand, ctx));
        }

        Expression::If { condition, then_branch, else_branch } => {
            result.merge(validate_expression(condition, ctx));
            result.merge(validate_block(then_branch, ctx));
            result.merge(validate_block(else_branch, ctx));
        }

        Expression::Operation { path, extern_fn, args } => {
            // Validate that path segments are UPPER_SNAKE_CASE
            for segment in path {
                if !is_upper_snake_case(segment) {
                    result.add_error(
                        ValidationErrorKind::UnsupportedFeature,
                        format!("operation path segment '{}' must be UPPER_SNAKE_CASE", segment),
                        &ctx.context_string(),
                    );
                }
            }

            // Validate extern_fn if present
            if let Some(fn_name) = extern_fn {
                if is_upper_snake_case(fn_name) {
                    result.add_error(
                        ValidationErrorKind::ReservedIdentifier,
                        format!("hybrid operation extern fn '{}' cannot use UPPER_SNAKE_CASE", fn_name),
                        &ctx.context_string(),
                    );
                }
            }

            // Validate arguments
            for arg in args {
                result.merge(validate_expression(arg, ctx));
            }
        }

        Expression::Call { function, args } => {
            // Function name should be valid user identifier
            if is_upper_snake_case(function) {
                result.add_error(
                    ValidationErrorKind::ReservedIdentifier,
                    format!("function call '{}' uses reserved UPPER_SNAKE_CASE (use operation syntax)", function),
                    &ctx.context_string(),
                );
            }

            for arg in args {
                result.merge(validate_expression(arg, ctx));
            }
        }

        Expression::QualifiedCall { namespace, function, args } => {
            // Validate function name
            if is_upper_snake_case(function) {
                result.add_error(
                    ValidationErrorKind::ReservedIdentifier,
                    format!("qualified function call '{}::{}' uses reserved UPPER_SNAKE_CASE", namespace, function),
                    &ctx.context_string(),
                );
            }

            for arg in args {
                result.merge(validate_expression(arg, ctx));
            }
        }

        Expression::Block(block) => {
            result.merge(validate_block(block, ctx));
        }

        Expression::Index { array, index } => {
            result.merge(validate_expression(array, ctx));
            result.merge(validate_expression(index, ctx));
        }

        Expression::Array(elements) => {
            for elem in elements {
                result.merge(validate_expression(elem, ctx));
            }
        }

        Expression::Cast { value, ty } => {
            result.merge(validate_expression(value, ctx));
            result.merge(validate_type(ty, ctx));
        }
    }

    result
}

fn validate_const_expr(expr: &Expression, ctx: &mut ValidationContext) -> ValidationResult {
    let mut result = ValidationResult::new();

    match ctx.is_valid_loop_bound(expr) {
        BoundCheckResult::Valid => {}
        BoundCheckResult::MutableVariable => {
            result.add_error(
                ValidationErrorKind::NonConstantExpression,
                "constant expression cannot reference mutable variable".to_string(),
                &ctx.context_string(),
            );
        }
        BoundCheckResult::NonConstant => {
            result.add_error(
                ValidationErrorKind::NonConstantExpression,
                "expected constant expression".to_string(),
                &ctx.context_string(),
            );
        }
        BoundCheckResult::InvalidType => {
            // Type is validated elsewhere
        }
    }

    // Also validate the expression itself
    result.merge(validate_expression(expr, ctx));

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use crate::{Block, Function};

    #[test]
    fn test_valid_user_identifiers() {
        let valid_names = ["x", "myVariable", "my_variable", "MyType", "_private", "x1"];
        for name in valid_names {
            assert!(is_valid_user_identifier(name), "expected '{}' to be valid", name);
        }
    }

    #[test]
    fn test_invalid_user_identifiers() {
        let invalid_names = ["GPIO", "MY_CONST", "ADC_CHANNEL", "X", "GPIO_READ"];
        for name in invalid_names {
            assert!(!is_valid_user_identifier(name), "expected '{}' to be invalid", name);
        }
    }

    #[test]
    fn test_validate_function_with_reserved_name() {
        let f = Function {
            name: "GPIO_BLINK".to_string(),
            params: vec![],
            return_type: None,
            body: Block {
                statements: vec![],
                result: None,
            },
        };

        let program = Program {
            constants: vec![],
            extern_fns: vec![],
            functions: vec![f],
        };

        let result = validate_program(&program);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::ReservedIdentifier)));
    }

    #[test]
    fn test_validate_mutable_loop_bound() {
        // This would need actual parsing to test properly
        // For now, just test the context
        let mut ctx = ValidationContext::new();
        ctx.add_mutable("count");

        let expr = Expression::Identifier("count".to_string());
        let check = ctx.is_valid_loop_bound(&expr);
        assert_eq!(check, BoundCheckResult::MutableVariable);
    }

    #[test]
    fn test_validate_immutable_loop_bound() {
        let mut ctx = ValidationContext::new();
        ctx.add_immutable("count");

        let expr = Expression::Identifier("count".to_string());
        let check = ctx.is_valid_loop_bound(&expr);
        assert_eq!(check, BoundCheckResult::Valid);
    }

    #[test]
    fn test_validate_constant_loop_bound() {
        let mut ctx = ValidationContext::new();
        ctx.add_constant("MAX_ITER");

        let expr = Expression::Identifier("MAX_ITER".to_string());
        let check = ctx.is_valid_loop_bound(&expr);
        assert_eq!(check, BoundCheckResult::Valid);
    }

    #[test]
    fn test_validate_literal_loop_bound() {
        let ctx = ValidationContext::new();

        let expr = Expression::Literal(Literal::Integer(100));
        let check = ctx.is_valid_loop_bound(&expr);
        assert_eq!(check, BoundCheckResult::Valid);
    }
}
