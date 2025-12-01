//! Pretty-printer for Embeem AST.
//!
//! This module provides functionality to convert AST nodes back to readable
//! Embeem source code. It handles operation paths with proper nesting based
//! on operation arities.

use alloc::collections::BTreeMap;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use alloc::format;

use crate::ast::*;
use crate::ops::op_kind_from_str;

/// Context for pretty-printing, holding information about external function arities.
#[derive(Clone, Debug, Default)]
pub struct PrettyPrintContext {
    /// Map from external function name to its arity.
    extern_fn_arities: BTreeMap<String, usize>,
    /// Current indentation level.
    indent: usize,
}

impl PrettyPrintContext {
    /// Create a new pretty-print context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a context from a program, extracting external function arities.
    pub fn from_program(program: &Program) -> Self {
        let mut ctx = Self::new();
        for ext_fn in &program.extern_fns {
            ctx.extern_fn_arities.insert(ext_fn.name.clone(), ext_fn.params.len());
        }
        ctx
    }

    /// Register an external function with its arity.
    pub fn register_extern_fn(&mut self, name: &str, arity: usize) {
        self.extern_fn_arities.insert(name.to_string(), arity);
    }

    /// Get the arity of an external function.
    pub fn extern_fn_arity(&self, name: &str) -> Option<usize> {
        self.extern_fn_arities.get(name).copied()
    }

    /// Get the arity of an operation segment by name.
    ///
    /// Returns the arity from `OpKind::arity()` if the segment is known,
    /// or `None` for unknown segments.
    pub fn segment_arity(name: &str) -> Option<usize> {
        op_kind_from_str(name).map(|op| op.arity())
    }

    fn indent_str(&self) -> String {
        "    ".repeat(self.indent)
    }

    fn with_indent<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.indent += 1;
        let result = f(self);
        self.indent -= 1;
        result
    }
}

/// Trait for pretty-printing AST nodes.
pub trait PrettyPrint {
    /// Convert this AST node to a pretty-printed string.
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String;
}

/// Convenience function to pretty-print a program.
///
/// This creates a context from the program's external functions and
/// pretty-prints the entire program.
pub fn pretty_print_program(program: &Program) -> String {
    let mut ctx = PrettyPrintContext::from_program(program);
    program.pretty_print(&mut ctx)
}

impl PrettyPrint for Program {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        let mut parts = Vec::new();

        // Constants
        for constant in &self.constants {
            parts.push(constant.pretty_print(ctx));
        }

        // External functions
        for ext_fn in &self.extern_fns {
            parts.push(ext_fn.pretty_print(ctx));
        }

        // Functions
        for function in &self.functions {
            parts.push(function.pretty_print(ctx));
        }

        parts.join("\n\n")
    }
}

impl PrettyPrint for ConstDecl {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        let indent = ctx.indent_str();
        format!(
            "{}const {}: {} = {};",
            indent,
            self.name,
            self.ty.pretty_print(ctx),
            self.value.pretty_print(ctx)
        )
    }
}

impl PrettyPrint for ExternFn {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        let indent = ctx.indent_str();
        let params = self.params
            .iter()
            .map(|p| p.pretty_print(ctx))
            .collect::<Vec<_>>()
            .join(", ");
        
        let ret = match &self.return_type {
            Some(ty) => format!(" -> {}", ty.pretty_print(ctx)),
            None => String::new(),
        };

        format!("{}extern fn {}({}){};", indent, self.name, params, ret)
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        let indent = ctx.indent_str();
        let params = self.params
            .iter()
            .map(|p| p.pretty_print(ctx))
            .collect::<Vec<_>>()
            .join(", ");
        
        let ret = match &self.return_type {
            Some(ty) => format!(" -> {}", ty.pretty_print(ctx)),
            None => String::new(),
        };

        let body = self.body.pretty_print(ctx);
        format!("{}fn {}({}){} {}", indent, self.name, params, ret, body)
    }
}

impl PrettyPrint for Param {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        format!("{}: {}", self.name, self.ty.pretty_print(ctx))
    }
}

impl PrettyPrint for Type {
    fn pretty_print(&self, _ctx: &mut PrettyPrintContext) -> String {
        match self {
            Type::Primitive(p) => p.pretty_print(_ctx),
            Type::Array(elem, size) => format!("[{}; {}]", elem.pretty_print(_ctx), size),
            Type::Tuple(elems) => {
                let parts: Vec<_> = elems.iter().map(|t| t.pretty_print(_ctx)).collect();
                format!("({})", parts.join(", "))
            }
        }
    }
}

impl PrettyPrint for PrimitiveType {
    fn pretty_print(&self, _ctx: &mut PrettyPrintContext) -> String {
        match self {
            PrimitiveType::U8 => "u8".to_string(),
            PrimitiveType::U16 => "u16".to_string(),
            PrimitiveType::U32 => "u32".to_string(),
            PrimitiveType::U64 => "u64".to_string(),
            PrimitiveType::I8 => "i8".to_string(),
            PrimitiveType::I16 => "i16".to_string(),
            PrimitiveType::I32 => "i32".to_string(),
            PrimitiveType::I64 => "i64".to_string(),
            PrimitiveType::F32 => "f32".to_string(),
            PrimitiveType::F64 => "f64".to_string(),
            PrimitiveType::Bool => "bool".to_string(),
        }
    }
}

impl PrettyPrint for Block {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        if self.statements.is_empty() && self.result.is_none() {
            return "{}".to_string();
        }

        let mut parts = Vec::new();
        parts.push("{".to_string());

        ctx.with_indent(|ctx| {
            for stmt in &self.statements {
                parts.push(stmt.pretty_print(ctx));
            }

            if let Some(ref result) = self.result {
                let indent = ctx.indent_str();
                parts.push(format!("{}{}", indent, result.pretty_print(ctx)));
            }
        });

        parts.push(format!("{}}}", ctx.indent_str()));
        parts.join("\n")
    }
}

impl PrettyPrint for Statement {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        let indent = ctx.indent_str();
        match self {
            Statement::Let { name, mutable, ty, value } => {
                let mut_kw = if *mutable { "mut " } else { "" };
                let ty_ann = match ty {
                    Some(t) => format!(": {}", t.pretty_print(ctx)),
                    None => String::new(),
                };
                format!("{}let {}{}{} = {};", indent, mut_kw, name, ty_ann, value.pretty_print(ctx))
            }
            Statement::Assign { target, value } => {
                let target_str = match target {
                    crate::AssignTarget::Identifier(name) => name.clone(),
                    crate::AssignTarget::Index { array, index } => {
                        format!("{}[{}]", array, index.pretty_print(ctx))
                    }
                };
                format!("{}{} = {};", indent, target_str, value.pretty_print(ctx))
            }
            Statement::Expr(expr) => {
                format!("{}{};", indent, expr.pretty_print(ctx))
            }
            Statement::If { condition, then_block, else_block } => {
                let mut result = format!(
                    "{}if {} {}",
                    indent,
                    condition.pretty_print(ctx),
                    then_block.pretty_print(ctx)
                );
                if let Some(else_blk) = else_block {
                    result.push_str(" else ");
                    result.push_str(&else_blk.pretty_print(ctx));
                }
                result
            }
            Statement::For { variable, start, end, direction, body } => {
                let dir = match direction {
                    RangeDirection::To => "to",
                    RangeDirection::DownTo => "downto",
                };
                format!(
                    "{}for {} in {} {} {} {}",
                    indent,
                    variable,
                    start.pretty_print(ctx),
                    dir,
                    end.pretty_print(ctx),
                    body.pretty_print(ctx)
                )
            }
            Statement::Repeat { count, body } => {
                format!(
                    "{}repeat {} {}",
                    indent,
                    count.pretty_print(ctx),
                    body.pretty_print(ctx)
                )
            }
            Statement::While { condition, max_iterations, body } => {
                format!(
                    "{}while {} max {} {}",
                    indent,
                    condition.pretty_print(ctx),
                    max_iterations.pretty_print(ctx),
                    body.pretty_print(ctx)
                )
            }
        }
    }
}

impl PrettyPrint for ElseBlock {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        match self {
            ElseBlock::Block(block) => block.pretty_print(ctx),
            ElseBlock::ElseIf(stmt) => {
                // Remove the leading indent since it's part of "else if"
                let s = stmt.pretty_print(ctx);
                s.trim_start().to_string()
            }
        }
    }
}

impl PrettyPrint for Expression {
    fn pretty_print(&self, ctx: &mut PrettyPrintContext) -> String {
        match self {
            Expression::Literal(lit) => lit.pretty_print(ctx),
            Expression::Identifier(name) => name.clone(),
            Expression::QualifiedIdentifier { namespace, name } => {
                format!("{}::{}", namespace, name)
            }
            Expression::Binary { op, left, right } => {
                format!(
                    "({} {} {})",
                    left.pretty_print(ctx),
                    op.pretty_print(ctx),
                    right.pretty_print(ctx)
                )
            }
            Expression::Unary { op, operand } => {
                format!("{}{}", op.pretty_print(ctx), operand.pretty_print(ctx))
            }
            Expression::If { condition, then_branch, else_branch } => {
                format!(
                    "if {} {} else {}",
                    condition.pretty_print(ctx),
                    then_branch.pretty_print(ctx),
                    else_branch.pretty_print(ctx)
                )
            }
            Expression::Operation { path, extern_fn, args } => {
                pretty_print_operation(path, extern_fn.as_deref(), args, ctx)
            }
            Expression::Call { function, args } => {
                let arg_strs: Vec<_> = args.iter().map(|a| a.pretty_print(ctx)).collect();
                format!("{}({})", function, arg_strs.join(", "))
            }
            Expression::QualifiedCall { namespace, function, args } => {
                let arg_strs: Vec<_> = args.iter().map(|a| a.pretty_print(ctx)).collect();
                format!("{}::{}({})", namespace, function, arg_strs.join(", "))
            }
            Expression::Block(block) => block.pretty_print(ctx),
            Expression::Index { array, index } => {
                format!("{}[{}]", array.pretty_print(ctx), index.pretty_print(ctx))
            }
            Expression::Array(elements) => {
                let elem_strs: Vec<_> = elements.iter().map(|e| e.pretty_print(ctx)).collect();
                format!("[{}]", elem_strs.join(", "))
            }
            Expression::Cast { value, ty } => {
                format!("{} as {}", value.pretty_print(ctx), ty.pretty_print(ctx))
            }
        }
    }
}

/// Pretty-print an operation with proper nesting.
///
/// Operations are stored as flat paths like `["WRITE", "GPIO"]` with args `[pin, value]`.
/// This function reconstructs the nested syntax: `WRITE(GPIO(pin), value)`.
///
/// The algorithm works from the innermost segment outward:
/// - Each segment consumes its arity worth of arguments
/// - The innermost segment (or extern fn) consumes from the front of args
/// - Each outer segment wraps the inner result and consumes additional args
fn pretty_print_operation(
    path: &[String],
    extern_fn: Option<&str>,
    args: &[Expression],
    ctx: &mut PrettyPrintContext,
) -> String {
    if path.is_empty() && extern_fn.is_none() {
        // Edge case: no path, just return args as a tuple
        let arg_strs: Vec<_> = args.iter().map(|a| a.pretty_print(ctx)).collect();
        return format!("({})", arg_strs.join(", "));
    }

    let mut args_iter = args.iter();

    // Handle single-segment case (primitive operations)
    if path.len() == 1 && extern_fn.is_none() {
        let seg = &path[0];
        let arg_strs: Vec<_> = args.iter().map(|a| a.pretty_print(ctx)).collect();
        return format!("{}({})", seg, arg_strs.join(", "));
    }

    // Build from innermost to outermost
    // The innermost is either the extern_fn or the last path segment
    
    let innermost_str = if let Some(ext_fn) = extern_fn {
        // Extern function is the innermost
        let ext_arity = ctx.extern_fn_arity(ext_fn).unwrap_or(0);
        let ext_args: Vec<_> = (0..ext_arity)
            .filter_map(|_| args_iter.next())
            .map(|a| a.pretty_print(ctx))
            .collect();
        format!("{}({})", ext_fn, ext_args.join(", "))
    } else {
        // Last path segment is the innermost
        let innermost = &path[path.len() - 1];
        let arity = PrettyPrintContext::segment_arity(innermost).unwrap_or(1);
        let inner_args: Vec<_> = (0..arity)
            .filter_map(|_| args_iter.next())
            .map(|a| a.pretty_print(ctx))
            .collect();
        format!("{}({})", innermost, inner_args.join(", "))
    };

    // Determine which segments to wrap around the innermost
    let outer_segments = if extern_fn.is_some() {
        &path[..]
    } else {
        &path[..path.len() - 1]
    };

    // Build outward through remaining segments (from inside to outside)
    let mut current = innermost_str;
    
    for seg in outer_segments.iter().rev() {
        let arity = PrettyPrintContext::segment_arity(seg).unwrap_or(0);
        // This segment wraps the current result and takes `arity` additional args
        let extra_args: Vec<_> = (0..arity)
            .filter_map(|_| args_iter.next())
            .map(|a| a.pretty_print(ctx))
            .collect();
        
        if extra_args.is_empty() {
            current = format!("{}({})", seg, current);
        } else {
            current = format!("{}({}, {})", seg, current, extra_args.join(", "));
        }
    }

    current
}

impl PrettyPrint for Literal {
    fn pretty_print(&self, _ctx: &mut PrettyPrintContext) -> String {
        match self {
            Literal::Integer(n) => n.to_string(),
            Literal::Float(f) => {
                let s = f.to_string();
                // Ensure floats always have a decimal point
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    s
                } else {
                    format!("{}.0", s)
                }
            }
            Literal::Bool(b) => b.to_string(),
        }
    }
}

impl PrettyPrint for BinaryOp {
    fn pretty_print(&self, _ctx: &mut PrettyPrintContext) -> String {
        match self {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Sub => "-".to_string(),
            BinaryOp::Mul => "*".to_string(),
            BinaryOp::Div => "/".to_string(),
            BinaryOp::Mod => "%".to_string(),
            BinaryOp::BitAnd => "&".to_string(),
            BinaryOp::BitOr => "|".to_string(),
            BinaryOp::BitXor => "^".to_string(),
            BinaryOp::Shl => "<<".to_string(),
            BinaryOp::Shr => ">>".to_string(),
            BinaryOp::LogicalShr => ">>>".to_string(),
            BinaryOp::Eq => "==".to_string(),
            BinaryOp::Ne => "!=".to_string(),
            BinaryOp::Lt => "<".to_string(),
            BinaryOp::Le => "<=".to_string(),
            BinaryOp::Gt => ">".to_string(),
            BinaryOp::Ge => ">=".to_string(),
            BinaryOp::And => "&&".to_string(),
            BinaryOp::Or => "||".to_string(),
        }
    }
}

impl PrettyPrint for UnaryOp {
    fn pretty_print(&self, _ctx: &mut PrettyPrintContext) -> String {
        match self {
            UnaryOp::Neg => "-".to_string(),
            UnaryOp::BitNot => "~".to_string(),
            UnaryOp::Not => "!".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::boxed::Box;
    use alloc::vec;

    #[test]
    fn test_pretty_print_simple_operation() {
        let mut ctx = PrettyPrintContext::new();
        
        // NOP() - nullary operation
        let expr = Expression::Operation {
            path: vec!["NOP".to_string()],
            extern_fn: None,
            args: vec![],
        };
        assert_eq!(expr.pretty_print(&mut ctx), "NOP()");
    }

    #[test]
    fn test_pretty_print_unary_operation() {
        let mut ctx = PrettyPrintContext::new();
        
        // GPIO_READ(13) - unary operation
        let expr = Expression::Operation {
            path: vec!["GPIO_READ".to_string()],
            extern_fn: None,
            args: vec![Expression::Literal(Literal::Integer(13))],
        };
        assert_eq!(expr.pretty_print(&mut ctx), "GPIO_READ(13)");
    }

    #[test]
    fn test_pretty_print_binary_operation() {
        let mut ctx = PrettyPrintContext::new();
        
        // GPIO_WRITE(13, 1) - binary operation
        let expr = Expression::Operation {
            path: vec!["GPIO_WRITE".to_string()],
            extern_fn: None,
            args: vec![
                Expression::Literal(Literal::Integer(13)),
                Expression::Literal(Literal::Integer(1)),
            ],
        };
        assert_eq!(expr.pretty_print(&mut ctx), "GPIO_WRITE(13, 1)");
    }

    #[test]
    fn test_pretty_print_nested_operation() {
        let mut ctx = PrettyPrintContext::new();
        
        // For a nested operation like WRITE(GPIO(pin), value):
        // - Stored as path: ["WRITE", "GPIO"], args: [pin, value]
        // - But we don't have WRITE in OpKind, so let's use a known one
        
        // Actually, let's test with unknown operations (they default to arity 1)
        // TEST(INNER(x)) where both are unknown -> defaults
        let expr = Expression::Operation {
            path: vec!["OUTER".to_string(), "INNER".to_string()],
            extern_fn: None,
            args: vec![Expression::Literal(Literal::Integer(42))],
        };
        // INNER gets arity 1 (default), takes 1 arg -> INNER(42)
        // OUTER gets arity 1 (default), its arg is INNER(42) -> OUTER(INNER(42))
        assert_eq!(expr.pretty_print(&mut ctx), "OUTER(INNER(42))");
    }

    #[test]
    fn test_pretty_print_hybrid_operation() {
        let mut ctx = PrettyPrintContext::new();
        ctx.register_extern_fn("sensor_read", 1);
        
        // WRITE(GPIO(sensor_read(0))) - hybrid operation
        // path: ["WRITE", "GPIO"], extern_fn: "sensor_read", args: [0]
        let expr = Expression::Operation {
            path: vec!["OUTER".to_string(), "INNER".to_string()],
            extern_fn: Some("sensor_read".to_string()),
            args: vec![Expression::Literal(Literal::Integer(0))],
        };
        assert_eq!(expr.pretty_print(&mut ctx), "OUTER(INNER(sensor_read(0)))");
    }

    #[test]
    fn test_pretty_print_type() {
        let mut ctx = PrettyPrintContext::new();
        
        assert_eq!(Type::Primitive(PrimitiveType::U32).pretty_print(&mut ctx), "u32");
        assert_eq!(Type::Primitive(PrimitiveType::Bool).pretty_print(&mut ctx), "bool");
        
        let arr_ty = Type::Array(Box::new(Type::Primitive(PrimitiveType::U8)), 10);
        assert_eq!(arr_ty.pretty_print(&mut ctx), "[u8; 10]");
    }

    #[test]
    fn test_pretty_print_extern_fn() {
        let mut ctx = PrettyPrintContext::new();
        
        let ext = ExternFn {
            name: "read_sensor".to_string(),
            params: vec![
                Param { name: "channel".to_string(), ty: Type::Primitive(PrimitiveType::U8) },
            ],
            return_type: Some(Type::Primitive(PrimitiveType::I32)),
        };
        assert_eq!(ext.pretty_print(&mut ctx), "extern fn read_sensor(channel: u8) -> i32;");
    }

    #[test]
    fn test_pretty_print_function() {
        let mut ctx = PrettyPrintContext::new();
        
        let func = Function {
            name: "blink".to_string(),
            params: vec![
                Param { name: "pin".to_string(), ty: Type::Primitive(PrimitiveType::U8) },
            ],
            return_type: None,
            body: Block {
                statements: vec![
                    Statement::Expr(Expression::Operation {
                        path: vec!["GPIO_TOGGLE".to_string()],
                        extern_fn: None,
                        args: vec![Expression::Identifier("pin".to_string())],
                    }),
                ],
                result: None,
            },
        };
        
        let output = func.pretty_print(&mut ctx);
        assert!(output.contains("fn blink(pin: u8)"));
        assert!(output.contains("GPIO_TOGGLE(pin)"));
    }

    #[test]
    fn test_pretty_print_let_statement() {
        let mut ctx = PrettyPrintContext::new();
        
        let stmt = Statement::Let {
            name: "x".to_string(),
            mutable: false,
            ty: Some(Type::Primitive(PrimitiveType::I32)),
            value: Expression::Literal(Literal::Integer(42)),
        };
        assert_eq!(stmt.pretty_print(&mut ctx), "let x: i32 = 42;");
        
        let stmt_mut = Statement::Let {
            name: "y".to_string(),
            mutable: true,
            ty: None,
            value: Expression::Literal(Literal::Bool(true)),
        };
        assert_eq!(stmt_mut.pretty_print(&mut ctx), "let mut y = true;");
    }

    #[test]
    fn test_pretty_print_for_loop() {
        let mut ctx = PrettyPrintContext::new();
        
        let stmt = Statement::For {
            variable: "i".to_string(),
            start: Expression::Literal(Literal::Integer(0)),
            end: Expression::Literal(Literal::Integer(10)),
            direction: RangeDirection::To,
            body: Block { statements: vec![], result: None },
        };
        assert_eq!(stmt.pretty_print(&mut ctx), "for i in 0 to 10 {}");
        
        let stmt_down = Statement::For {
            variable: "j".to_string(),
            start: Expression::Literal(Literal::Integer(10)),
            end: Expression::Literal(Literal::Integer(0)),
            direction: RangeDirection::DownTo,
            body: Block { statements: vec![], result: None },
        };
        assert_eq!(stmt_down.pretty_print(&mut ctx), "for j in 10 downto 0 {}");
    }

    #[test]
    fn test_pretty_print_binary_expr() {
        let mut ctx = PrettyPrintContext::new();
        
        let expr = Expression::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expression::Literal(Literal::Integer(1))),
            right: Box::new(Expression::Literal(Literal::Integer(2))),
        };
        assert_eq!(expr.pretty_print(&mut ctx), "(1 + 2)");
    }

    #[test]
    fn test_pretty_print_cast() {
        let mut ctx = PrettyPrintContext::new();
        
        let expr = Expression::Cast {
            value: Box::new(Expression::Literal(Literal::Integer(42))),
            ty: Type::Primitive(PrimitiveType::U8),
        };
        assert_eq!(expr.pretty_print(&mut ctx), "42 as u8");
    }
}
