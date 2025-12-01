//! Parser implementation for Embeem.
//!
//! This module implements a parser for the Embeem language following the
//! grammar specification in `spec/SPECIFICATION.md`.

use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use embeem_ast::{
    BinaryOp, Block, ConstDecl, ElseBlock, Expression, Function, Literal, Param,
    PrimitiveType, Program, RangeDirection, Statement, Type, UnaryOp,
    Target, VirtualOp,
};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::char,
    combinator::{opt, value},
};

/// Parse error type.
#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(msg: &str) -> Self {
        Self {
            message: msg.to_string(),
        }
    }
}

/// Parse an Embeem program from source code.
pub fn parse_program(input: &str) -> Result<Program, ParseError> {
    let input = skip_ws(input);
    match program(input) {
        Ok((remaining, prog)) => {
            let remaining = skip_ws(remaining);
            if remaining.is_empty() {
                Ok(prog)
            } else {
                Err(ParseError::new("unexpected input after program"))
            }
        }
        Err(_) => Err(ParseError::new("failed to parse program")),
    }
}

/// Skip whitespace and comments.
///
/// From spec Section 2.6:
/// ```text
/// LINE_COMMENT   ::= '//' [^\n]*
/// BLOCK_COMMENT  ::= '/*' .* '*/'
/// ```
fn skip_ws(input: &str) -> &str {
    let mut rest = input;
    loop {
        // Skip whitespace
        let trimmed = rest.trim_start();
        if trimmed.len() == rest.len() && !rest.starts_with("//") && !rest.starts_with("/*") {
            break;
        }
        rest = trimmed;
        
        // Skip line comments
        if rest.starts_with("//") {
            if let Some(pos) = rest.find('\n') {
                rest = &rest[pos + 1..];
            } else {
                rest = "";
            }
            continue;
        }
        
        // Skip block comments
        if rest.starts_with("/*") {
            if let Some(pos) = rest.find("*/") {
                rest = &rest[pos + 2..];
            } else {
                rest = "";
            }
            continue;
        }
    }
    rest
}

/// Parse a program.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// program     = { item } ;
/// item        = function | const_decl ;
/// ```
fn program(input: &str) -> IResult<&str, Program> {
    let mut constants = Vec::new();
    let mut functions = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        if trimmed.is_empty() {
            break;
        }
        
        if let Ok((rest, c)) = const_decl(trimmed) {
            constants.push(c);
            current = rest;
            continue;
        }
        
        if let Ok((rest, f)) = function(trimmed) {
            functions.push(f);
            current = rest;
            continue;
        }
        
        break;
    }
    
    Ok((current, Program { constants, functions }))
}

/// Parse a constant declaration.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// const_decl  = "const" IDENT ":" type "=" const_expr ";" ;
/// ```
fn const_decl(input: &str) -> IResult<&str, ConstDecl> {
    let (input, _) = tag("const").parse(input)?;
    let input = skip_ws(input);
    let (input, name) = identifier(input)?;
    let input = skip_ws(input);
    let (input, _) = char(':').parse(input)?;
    let input = skip_ws(input);
    let (input, ty) = type_annotation(input)?;
    let input = skip_ws(input);
    let (input, _) = char('=').parse(input)?;
    let input = skip_ws(input);
    let (input, value) = expression(input)?;
    let input = skip_ws(input);
    let (input, _) = char(';').parse(input)?;
    
    Ok((input, ConstDecl { name, ty, value }))
}

/// Parse a function definition.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// function    = "fn" IDENT "(" [ param_list ] ")" [ "->" type ] block ;
/// ```
fn function(input: &str) -> IResult<&str, Function> {
    let (input, _) = tag("fn").parse(input)?;
    let input = skip_ws(input);
    let (input, name) = identifier(input)?;
    let input = skip_ws(input);
    let (input, _) = char('(').parse(input)?;
    let input = skip_ws(input);
    let (input, params) = param_list(input)?;
    let input = skip_ws(input);
    let (input, _) = char(')').parse(input)?;
    let input = skip_ws(input);
    let (input, return_type) = opt(|i| {
        let (i, _) = tag("->").parse(i)?;
        let i = skip_ws(i);
        type_annotation(i)
    }).parse(input)?;
    let input = skip_ws(input);
    let (input, body) = block(input)?;
    
    Ok((input, Function { name, params, return_type, body }))
}

/// Parse a parameter list.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// param_list  = param { "," param } ;
/// ```
fn param_list(input: &str) -> IResult<&str, Vec<Param>> {
    let mut params = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        
        if let Ok((rest, p)) = param(trimmed) {
            params.push(p);
            let rest = skip_ws(rest);
            if rest.starts_with(',') {
                current = skip_ws(&rest[1..]);
                continue;
            }
            current = rest;
            break;
        }
        
        break;
    }
    
    Ok((current, params))
}

/// Parse a function parameter.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// param       = IDENT ":" type ;
/// ```
fn param(input: &str) -> IResult<&str, Param> {
    let (input, name) = identifier(input)?;
    let input = skip_ws(input);
    let (input, _) = char(':').parse(input)?;
    let input = skip_ws(input);
    let (input, ty) = type_annotation(input)?;
    
    Ok((input, Param { name, ty }))
}

/// Parse a type annotation.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// type        = prim_type | array_type | tuple_type ;
/// ```
fn type_annotation(input: &str) -> IResult<&str, Type> {
    // Try array type first
    if input.starts_with('[') {
        return array_type(input);
    }
    
    // Try tuple type
    if input.starts_with('(') {
        return tuple_type(input);
    }
    
    primitive_type(input)
}

/// Parse a primitive type.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// prim_type   = "u8" | "u16" | "u32" | "u64"
///             | "i8" | "i16" | "i32" | "i64"
///             | "f32" | "f64" | "bool" ;
/// ```
fn primitive_type(input: &str) -> IResult<&str, Type> {
    alt((
        value(Type::Primitive(PrimitiveType::U64), tag("u64")),
        value(Type::Primitive(PrimitiveType::U32), tag("u32")),
        value(Type::Primitive(PrimitiveType::U16), tag("u16")),
        value(Type::Primitive(PrimitiveType::U8), tag("u8")),
        value(Type::Primitive(PrimitiveType::I64), tag("i64")),
        value(Type::Primitive(PrimitiveType::I32), tag("i32")),
        value(Type::Primitive(PrimitiveType::I16), tag("i16")),
        value(Type::Primitive(PrimitiveType::I8), tag("i8")),
        value(Type::Primitive(PrimitiveType::F64), tag("f64")),
        value(Type::Primitive(PrimitiveType::F32), tag("f32")),
        value(Type::Primitive(PrimitiveType::Bool), tag("bool")),
    )).parse(input)
}

/// Parse an array type.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// array_type  = "[" type ";" INTEGER "]" ;
/// ```
fn array_type(input: &str) -> IResult<&str, Type> {
    let (input, _) = char('[').parse(input)?;
    let input = skip_ws(input);
    let (input, elem_type) = type_annotation(input)?;
    let input = skip_ws(input);
    let (input, _) = char(';').parse(input)?;
    let input = skip_ws(input);
    let (input, size) = integer_literal(input)?;
    let input = skip_ws(input);
    let (input, _) = char(']').parse(input)?;
    
    Ok((input, Type::Array(Box::new(elem_type), size)))
}

/// Parse a tuple type.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// tuple_type  = "(" type { "," type } ")" ;
/// ```
fn tuple_type(input: &str) -> IResult<&str, Type> {
    let (input, _) = char('(').parse(input)?;
    let input = skip_ws(input);
    
    let mut types = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        if trimmed.starts_with(')') {
            current = &trimmed[1..];
            break;
        }
        
        let (rest, ty) = type_annotation(trimmed)?;
        types.push(ty);
        let rest = skip_ws(rest);
        
        if rest.starts_with(',') {
            current = skip_ws(&rest[1..]);
            continue;
        }
        
        if rest.starts_with(')') {
            current = &rest[1..];
            break;
        }
        
        return Err(nom::Err::Error(nom::error::Error::new(rest, nom::error::ErrorKind::Char)));
    }
    
    Ok((current, Type::Tuple(types)))
}

fn block(input: &str) -> IResult<&str, Block> {
    let (input, _) = char('{').parse(input)?;
    let input = skip_ws(input);
    let (input, (statements, result)) = block_contents(input)?;
    let input = skip_ws(input);
    let (input, _) = char('}').parse(input)?;
    
    Ok((input, Block { statements, result }))
}

/// Parse block contents (statements and optional result expression).
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// block       = "{" { statement } [ expr ] "}" ;
/// ```
fn block_contents(input: &str) -> IResult<&str, (Vec<Statement>, Option<Box<Expression>>)> {
    let mut statements = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        
        // Check if we've reached the end of the block
        if trimmed.starts_with('}') {
            return Ok((trimmed, (statements, None)));
        }
        
        // Check if this looks like a potential result expression by seeing if
        // we can parse an expression that ends with '}' without a semicolon
        // We need to be careful: if the expression is an if statement (with side effects
        // in its branches), we should parse it as a statement instead.
        let is_potential_result = {
            // Try parsing an expression
            if let Ok((rest, expr)) = expression(trimmed) {
                let rest_trimmed = skip_ws(rest);
                // It's a result if it's followed by '}' and appears to be a value expression
                rest_trimmed.starts_with('}') && is_value_expression(&expr)
            } else {
                false
            }
        };
        
        if is_potential_result {
            let (rest, expr) = expression(trimmed)?;
            let rest_trimmed = skip_ws(rest);
            return Ok((rest_trimmed, (statements, Some(Box::new(expr)))));
        }
        
        // Try to parse a statement
        if let Ok((rest, stmt)) = statement(trimmed) {
            statements.push(stmt);
            current = rest;
            continue;
        }
        
        // Try to parse an expression followed by ';'
        if let Ok((rest, expr)) = expression(trimmed) {
            let rest_trimmed = skip_ws(rest);
            if rest_trimmed.starts_with(';') {
                statements.push(Statement::Expr(expr));
                current = &rest_trimmed[1..];
                continue;
            }
        }
        
        // Can't parse anything more
        break;
    }
    
    Ok((current, (statements, None)))
}

/// Check if an expression is a "value expression" (produces a value rather than being void).
/// For our purposes, an if expression is a value expression only if both branches have
/// result expressions (not just statements).
fn is_value_expression(expr: &Expression) -> bool {
    match expr {
        Expression::If { then_branch, else_branch, .. } => {
            // Both branches must have result expressions
            then_branch.result.is_some() && else_branch.result.is_some()
        }
        Expression::Block(block) => {
            // A block is a value expression if it has a result
            block.result.is_some()
        }
        // All other expressions are value expressions
        _ => true,
    }
}

/// Parse a statement.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// statement   = let_stmt | assign_stmt | expr_stmt | if_stmt | for_stmt | repeat_stmt | while_stmt ;
/// ```
fn statement(input: &str) -> IResult<&str, Statement> {
    // Try different statement types
    if input.starts_with("let") && !is_ident_char(input.as_bytes().get(3).copied().unwrap_or(0)) {
        return let_statement(input);
    }
    
    if input.starts_with("if") && !is_ident_char(input.as_bytes().get(2).copied().unwrap_or(0)) {
        return if_statement(input);
    }
    
    if input.starts_with("for") && !is_ident_char(input.as_bytes().get(3).copied().unwrap_or(0)) {
        return for_statement(input);
    }
    
    if input.starts_with("repeat") && !is_ident_char(input.as_bytes().get(6).copied().unwrap_or(0)) {
        return repeat_statement(input);
    }
    
    if input.starts_with("while") && !is_ident_char(input.as_bytes().get(5).copied().unwrap_or(0)) {
        return while_statement(input);
    }
    
    assign_or_expr_statement(input)
}

/// Parse a let statement.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// let_stmt    = "let" [ "mut" ] IDENT [ ":" type ] "=" expr ";" ;
/// ```
fn let_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("let").parse(input)?;
    let input = skip_ws(input);
    let (input, mutable) = if input.starts_with("mut") && !is_ident_char(input.as_bytes().get(3).copied().unwrap_or(0)) {
        (skip_ws(&input[3..]), true)
    } else {
        (input, false)
    };
    let (input, name) = identifier(input)?;
    let input = skip_ws(input);
    let (input, ty) = if input.starts_with(':') {
        let input = skip_ws(&input[1..]);
        let (input, ty) = type_annotation(input)?;
        (input, Some(ty))
    } else {
        (input, None)
    };
    let input = skip_ws(input);
    let (input, _) = char('=').parse(input)?;
    let input = skip_ws(input);
    let (input, value) = expression(input)?;
    let input = skip_ws(input);
    let (input, _) = char(';').parse(input)?;
    
    Ok((input, Statement::Let { name, mutable, ty, value }))
}

/// Parse an if statement.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// if_stmt     = "if" expr block [ "else" ( block | if_stmt ) ] ;
/// ```
fn if_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("if").parse(input)?;
    let input = skip_ws(input);
    let (input, condition) = expression(input)?;
    let input = skip_ws(input);
    let (input, then_block) = block(input)?;
    let input = skip_ws(input);
    let (input, else_block) = if input.starts_with("else") && !is_ident_char(input.as_bytes().get(4).copied().unwrap_or(0)) {
        let input = skip_ws(&input[4..]);
        if input.starts_with("if") && !is_ident_char(input.as_bytes().get(2).copied().unwrap_or(0)) {
            let (input, stmt) = if_statement(input)?;
            (input, Some(ElseBlock::ElseIf(Box::new(stmt))))
        } else {
            let (input, blk) = block(input)?;
            (input, Some(ElseBlock::Block(blk)))
        }
    } else {
        (input, None)
    };
    
    Ok((input, Statement::If { condition, then_block, else_block }))
}

/// Parse a for loop statement.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// for_stmt    = "for" IDENT "in" range block ;
/// range       = const_expr ( "to" | "downto" ) const_expr ;
/// ```
fn for_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("for").parse(input)?;
    let input = skip_ws(input);
    let (input, variable) = identifier(input)?;
    let input = skip_ws(input);
    let (input, _) = tag("in").parse(input)?;
    let input = skip_ws(input);
    let (input, start) = expression(input)?;
    let input = skip_ws(input);
    let (input, direction) = if input.starts_with("downto") && !is_ident_char(input.as_bytes().get(6).copied().unwrap_or(0)) {
        (&input[6..], RangeDirection::DownTo)
    } else if input.starts_with("to") && !is_ident_char(input.as_bytes().get(2).copied().unwrap_or(0)) {
        (&input[2..], RangeDirection::To)
    } else {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
    };
    let input = skip_ws(input);
    let (input, end) = expression(input)?;
    let input = skip_ws(input);
    let (input, body) = block(input)?;
    
    Ok((input, Statement::For { variable, start, end, direction, body }))
}

/// Parse a repeat statement.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// repeat_stmt = "repeat" const_expr block ;
/// ```
fn repeat_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("repeat").parse(input)?;
    let input = skip_ws(input);
    let (input, count) = expression(input)?;
    let input = skip_ws(input);
    let (input, body) = block(input)?;
    
    Ok((input, Statement::Repeat { count, body }))
}

/// Parse a bounded while statement.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// while_stmt  = "while" expr "max" const_expr block ;
/// ```
fn while_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = tag("while").parse(input)?;
    let input = skip_ws(input);
    let (input, condition) = expression(input)?;
    let input = skip_ws(input);
    let (input, _) = tag("max").parse(input)?;
    let input = skip_ws(input);
    let (input, max_iterations) = expression(input)?;
    let input = skip_ws(input);
    let (input, body) = block(input)?;
    
    Ok((input, Statement::While { condition, max_iterations, body }))
}

/// Parse an assignment or expression statement.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// assign_stmt = IDENT "=" expr ";" ;
/// expr_stmt   = expr ";" ;
/// ```
fn assign_or_expr_statement(input: &str) -> IResult<&str, Statement> {
    // Try to parse an identifier followed by '='
    if let Ok((rest, name)) = identifier(input) {
        let rest = skip_ws(rest);
        if rest.starts_with('=') && !rest.starts_with("==") {
            let rest = skip_ws(&rest[1..]);
            if let Ok((rest, value)) = expression(rest) {
                let rest = skip_ws(rest);
                if rest.starts_with(';') {
                    return Ok((&rest[1..], Statement::Assign { target: name, value }));
                }
            }
        }
    }
    
    // Fall back to expression statement
    let (input, expr) = expression(input)?;
    let input = skip_ws(input);
    let (input, _) = char(';').parse(input)?;
    
    Ok((input, Statement::Expr(expr)))
}

/// Parse an expression.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// expr        = or_expr ;
/// ```
fn expression(input: &str) -> IResult<&str, Expression> {
    or_expr(input)
}

/// Parse a logical or expression.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// or_expr     = and_expr { "or" and_expr } ;
/// ```
fn or_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut left) = and_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        if trimmed.starts_with("or") && !is_ident_char(trimmed.as_bytes().get(2).copied().unwrap_or(0)) {
            let rest = skip_ws(&trimmed[2..]);
            if let Ok((rest, right)) = and_expr(rest) {
                left = Expression::Binary {
                    op: BinaryOp::Or,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                input = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((input, left))
}

/// Parse a logical and expression.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// and_expr    = comp_expr { "and" comp_expr } ;
/// ```
fn and_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut left) = comp_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        if trimmed.starts_with("and") && !is_ident_char(trimmed.as_bytes().get(3).copied().unwrap_or(0)) {
            let rest = skip_ws(&trimmed[3..]);
            if let Ok((rest, right)) = comp_expr(rest) {
                left = Expression::Binary {
                    op: BinaryOp::And,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                input = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((input, left))
}

/// Parse a comparison expression.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// comp_expr   = bitor_expr { comp_op bitor_expr } ;
/// comp_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" ;
/// ```
fn comp_expr(input: &str) -> IResult<&str, Expression> {
    let (input, left) = bitor_expr(input)?;
    let trimmed = skip_ws(input);
    
    let (op, op_len) = if trimmed.starts_with("==") {
        (Some(BinaryOp::Eq), 2)
    } else if trimmed.starts_with("!=") {
        (Some(BinaryOp::Ne), 2)
    } else if trimmed.starts_with("<=") {
        (Some(BinaryOp::Le), 2)
    } else if trimmed.starts_with(">=") {
        (Some(BinaryOp::Ge), 2)
    } else if trimmed.starts_with('<') {
        (Some(BinaryOp::Lt), 1)
    } else if trimmed.starts_with('>') {
        (Some(BinaryOp::Gt), 1)
    } else {
        (None, 0)
    };
    
    if let Some(op) = op {
        let rest = skip_ws(&trimmed[op_len..]);
        let (rest, right) = bitor_expr(rest)?;
        Ok((rest, Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }))
    } else {
        Ok((input, left))
    }
}

fn bitor_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut left) = xor_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        if trimmed.starts_with('|') && !trimmed.starts_with("||") {
            let rest = skip_ws(&trimmed[1..]);
            if let Ok((rest, right)) = xor_expr(rest) {
                left = Expression::Binary {
                    op: BinaryOp::BitOr,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                input = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((input, left))
}

fn xor_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut left) = bitand_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        if trimmed.starts_with('^') {
            let rest = skip_ws(&trimmed[1..]);
            if let Ok((rest, right)) = bitand_expr(rest) {
                left = Expression::Binary {
                    op: BinaryOp::BitXor,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                input = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((input, left))
}

fn bitand_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut left) = shift_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        if trimmed.starts_with('&') && !trimmed.starts_with("&&") {
            let rest = skip_ws(&trimmed[1..]);
            if let Ok((rest, right)) = shift_expr(rest) {
                left = Expression::Binary {
                    op: BinaryOp::BitAnd,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                input = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((input, left))
}

fn shift_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut left) = add_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        let (op, op_len) = if trimmed.starts_with(">>>") {
            (Some(BinaryOp::LogicalShr), 3)
        } else if trimmed.starts_with("<<") {
            (Some(BinaryOp::Shl), 2)
        } else if trimmed.starts_with(">>") {
            (Some(BinaryOp::Shr), 2)
        } else {
            (None, 0)
        };
        
        if let Some(op) = op {
            let rest = skip_ws(&trimmed[op_len..]);
            if let Ok((rest, right)) = add_expr(rest) {
                left = Expression::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                input = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((input, left))
}

fn add_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut left) = mul_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        let op = if trimmed.starts_with('+') {
            Some(BinaryOp::Add)
        } else if trimmed.starts_with('-') && !trimmed.starts_with("->") {
            Some(BinaryOp::Sub)
        } else {
            None
        };
        
        if let Some(op) = op {
            let rest = skip_ws(&trimmed[1..]);
            if let Ok((rest, right)) = mul_expr(rest) {
                left = Expression::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                input = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((input, left))
}

fn mul_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut left) = unary_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        let op = if trimmed.starts_with('*') {
            Some(BinaryOp::Mul)
        } else if trimmed.starts_with('/') && !trimmed.starts_with("//") {
            Some(BinaryOp::Div)
        } else if trimmed.starts_with('%') {
            Some(BinaryOp::Mod)
        } else {
            None
        };
        
        if let Some(op) = op {
            let rest = skip_ws(&trimmed[1..]);
            if let Ok((rest, right)) = unary_expr(rest) {
                left = Expression::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                input = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((input, left))
}

fn unary_expr(input: &str) -> IResult<&str, Expression> {
    if input.starts_with('-') {
        let rest = skip_ws(&input[1..]);
        let (rest, operand) = unary_expr(rest)?;
        return Ok((rest, Expression::Unary {
            op: UnaryOp::Neg,
            operand: Box::new(operand),
        }));
    }
    
    if input.starts_with('~') {
        let rest = skip_ws(&input[1..]);
        let (rest, operand) = unary_expr(rest)?;
        return Ok((rest, Expression::Unary {
            op: UnaryOp::BitNot,
            operand: Box::new(operand),
        }));
    }
    
    if input.starts_with("not") && !is_ident_char(input.as_bytes().get(3).copied().unwrap_or(0)) {
        let rest = skip_ws(&input[3..]);
        let (rest, operand) = unary_expr(rest)?;
        return Ok((rest, Expression::Unary {
            op: UnaryOp::Not,
            operand: Box::new(operand),
        }));
    }
    
    postfix_expr(input)
}

fn postfix_expr(input: &str) -> IResult<&str, Expression> {
    let (mut input, mut expr) = primary_expr(input)?;
    
    loop {
        let trimmed = skip_ws(input);
        
        // Array index
        if trimmed.starts_with('[') {
            let rest = skip_ws(&trimmed[1..]);
            if let Ok((rest, index)) = expression(rest) {
                let rest = skip_ws(rest);
                if rest.starts_with(']') {
                    expr = Expression::Index {
                        array: Box::new(expr),
                        index: Box::new(index),
                    };
                    input = &rest[1..];
                    continue;
                }
            }
        }
        
        // Type cast
        if trimmed.starts_with("as") && !is_ident_char(trimmed.as_bytes().get(2).copied().unwrap_or(0)) {
            let rest = skip_ws(&trimmed[2..]);
            if let Ok((rest, ty)) = type_annotation(rest) {
                expr = Expression::Cast {
                    value: Box::new(expr),
                    ty,
                };
                input = rest;
                continue;
            }
        }
        
        break;
    }
    
    Ok((input, expr))
}

fn primary_expr(input: &str) -> IResult<&str, Expression> {
    // Try if expression
    if input.starts_with("if") && !is_ident_char(input.as_bytes().get(2).copied().unwrap_or(0)) {
        return if_expr(input);
    }
    
    // Try block expression
    if input.starts_with('{') {
        return block_expr(input);
    }
    
    // Try parenthesized expression
    if input.starts_with('(') {
        return paren_expr(input);
    }
    
    // Try array literal
    if input.starts_with('[') {
        return array_literal(input);
    }
    
    // Try literal
    if let Ok((rest, expr)) = literal_expr(input) {
        return Ok((rest, expr));
    }
    
    // Try operation call or identifier
    operation_or_call_or_ident(input)
}

fn if_expr(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("if").parse(input)?;
    let input = skip_ws(input);
    let (input, condition) = expression(input)?;
    let input = skip_ws(input);
    let (input, then_branch) = block(input)?;
    let input = skip_ws(input);
    let (input, _) = tag("else").parse(input)?;
    let input = skip_ws(input);
    let (input, else_branch) = block(input)?;
    
    Ok((input, Expression::If {
        condition: Box::new(condition),
        then_branch,
        else_branch,
    }))
}

fn block_expr(input: &str) -> IResult<&str, Expression> {
    let (input, blk) = block(input)?;
    Ok((input, Expression::Block(blk)))
}

fn paren_expr(input: &str) -> IResult<&str, Expression> {
    let (input, _) = char('(').parse(input)?;
    let input = skip_ws(input);
    let (input, expr) = expression(input)?;
    let input = skip_ws(input);
    let (input, _) = char(')').parse(input)?;
    
    Ok((input, expr))
}

fn array_literal(input: &str) -> IResult<&str, Expression> {
    let (input, _) = char('[').parse(input)?;
    let input = skip_ws(input);
    
    let mut elements = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        
        if trimmed.starts_with(']') {
            current = &trimmed[1..];
            break;
        }
        
        let (rest, elem) = expression(trimmed)?;
        elements.push(elem);
        let rest = skip_ws(rest);
        
        if rest.starts_with(',') {
            current = skip_ws(&rest[1..]);
            continue;
        }
        
        if rest.starts_with(']') {
            current = &rest[1..];
            break;
        }
        
        return Err(nom::Err::Error(nom::error::Error::new(rest, nom::error::ErrorKind::Char)));
    }
    
    Ok((current, Expression::Array(elements)))
}

fn literal_expr(input: &str) -> IResult<&str, Expression> {
    // Try bool literal
    if input.starts_with("true") && !is_ident_char(input.as_bytes().get(4).copied().unwrap_or(0)) {
        return Ok((&input[4..], Expression::Literal(Literal::Bool(true))));
    }
    if input.starts_with("false") && !is_ident_char(input.as_bytes().get(5).copied().unwrap_or(0)) {
        return Ok((&input[5..], Expression::Literal(Literal::Bool(false))));
    }
    
    // Try hex literal
    if input.starts_with("0x") {
        let rest = &input[2..];
        let (rest, digits) = take_while1::<_, _, nom::error::Error<&str>>(|c: char| c.is_ascii_hexdigit()).parse(rest)?;
        let n = u64::from_str_radix(digits, 16).unwrap_or(0);
        return Ok((rest, Expression::Literal(Literal::Integer(n))));
    }
    
    // Try binary literal
    if input.starts_with("0b") {
        let rest = &input[2..];
        let (rest, digits) = take_while1::<_, _, nom::error::Error<&str>>(|c: char| c == '0' || c == '1').parse(rest)?;
        let n = u64::from_str_radix(digits, 2).unwrap_or(0);
        return Ok((rest, Expression::Literal(Literal::Integer(n))));
    }
    
    // Try float or integer
    if input.starts_with(|c: char| c.is_ascii_digit()) {
        let (rest, whole) = take_while1::<_, _, nom::error::Error<&str>>(|c: char| c.is_ascii_digit()).parse(input)?;
        
        // Check for float (has decimal point)
        if rest.starts_with('.') {
            let rest = &rest[1..];
            if rest.starts_with(|c: char| c.is_ascii_digit()) {
                let (rest, frac) = take_while1::<_, _, nom::error::Error<&str>>(|c: char| c.is_ascii_digit()).parse(rest)?;
                
                // Optional exponent
                let (rest, exp_part) = if rest.starts_with('e') || rest.starts_with('E') {
                    let rest = &rest[1..];
                    let (rest, sign) = if rest.starts_with('+') || rest.starts_with('-') {
                        (&rest[1..], &rest[..1])
                    } else {
                        (rest, "")
                    };
                    let (rest, exp_digits) = take_while1::<_, _, nom::error::Error<&str>>(|c: char| c.is_ascii_digit()).parse(rest)?;
                    (rest, Some((sign, exp_digits)))
                } else {
                    (rest, None)
                };
                
                let f = parse_float(whole, frac, exp_part);
                return Ok((rest, Expression::Literal(Literal::Float(f))));
            }
        }
        
        // It's an integer
        let mut n: u64 = 0;
        for c in whole.chars() {
            n = n.saturating_mul(10).saturating_add((c as u32 - '0' as u32) as u64);
        }
        return Ok((rest, Expression::Literal(Literal::Integer(n))));
    }
    
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Digit)))
}

fn parse_float(whole: &str, frac: &str, exp: Option<(&str, &str)>) -> f64 {
    let mut result = 0.0f64;
    let mut frac_divisor = 1.0f64;
    
    for c in whole.chars() {
        result = result * 10.0 + (c as u32 - '0' as u32) as f64;
    }
    
    for c in frac.chars() {
        frac_divisor *= 10.0;
        result += (c as u32 - '0' as u32) as f64 / frac_divisor;
    }
    
    if let Some((sign, exp_digits)) = exp {
        let mut exp_val = 0i32;
        for c in exp_digits.chars() {
            exp_val = exp_val * 10 + (c as u32 - '0' as u32) as i32;
        }
        if sign == "-" {
            exp_val = -exp_val;
        }
        
        if exp_val > 0 {
            for _ in 0..exp_val {
                result *= 10.0;
            }
        } else if exp_val < 0 {
            for _ in 0..(-exp_val) {
                result /= 10.0;
            }
        }
    }
    
    result
}

fn integer_literal(input: &str) -> IResult<&str, u64> {
    let (rest, digits) = take_while1::<_, _, nom::error::Error<&str>>(|c: char| c.is_ascii_digit()).parse(input)?;
    let mut n: u64 = 0;
    for c in digits.chars() {
        n = n.saturating_mul(10).saturating_add((c as u32 - '0' as u32) as u64);
    }
    Ok((rest, n))
}

fn operation_or_call_or_ident(input: &str) -> IResult<&str, Expression> {
    let (input, name) = identifier(input)?;
    let input_after_name = skip_ws(input);
    
    // Check if followed by '('
    if !input_after_name.starts_with('(') {
        return Ok((input, Expression::Identifier(name)));
    }
    
    // Check if this is a virtual operation (like READ, WRITE, etc.)
    if let Some(virtual_op) = VirtualOp::from_str(&name) {
        return parse_virtual_call(input_after_name, virtual_op);
    }
    
    // Parse arguments
    let (input, _) = char('(').parse(input_after_name)?;
    let input = skip_ws(input);
    
    let mut args = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        
        if trimmed.starts_with(')') {
            current = &trimmed[1..];
            break;
        }
        
        let (rest, arg) = expression(trimmed)?;
        args.push(arg);
        let rest = skip_ws(rest);
        
        if rest.starts_with(',') {
            current = skip_ws(&rest[1..]);
            continue;
        }
        
        if rest.starts_with(')') {
            current = &rest[1..];
            break;
        }
        
        return Err(nom::Err::Error(nom::error::Error::new(rest, nom::error::ErrorKind::Char)));
    }
    
    // Check if it's an operation
    if let Some(kind) = embeem_ast::op_name_from_str(&name) {
        Ok((current, Expression::Operation { kind, args }))
    } else {
        Ok((current, Expression::Call { function: name, args }))
    }
}

/// Parse a virtual function call like `WRITE(GPIO(pin), value)`.
///
/// The virtual operation (like WRITE, READ) has already been identified.
/// The input starts at the opening parenthesis.
///
/// Virtual calls have the form: `OP(TARGET(target_args...), additional_args...)`
fn parse_virtual_call(input: &str, virtual_op: VirtualOp) -> IResult<&str, Expression> {
    let (input, _) = char('(').parse(input)?;
    let input = skip_ws(input);
    
    // The first argument should be TARGET(args...)
    // Parse the target name
    let (input, target_name) = identifier(input)?;
    
    // Check if it's a valid target
    let target = match Target::from_str(&target_name) {
        Some(t) => t,
        None => {
            // Not a virtual call target, treat as regular expression
            // This shouldn't happen if VirtualOp::from_str was true, but just in case
            return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
        }
    };
    
    let input = skip_ws(input);
    
    // Parse target's arguments (e.g., the pin number in GPIO(13))
    let (input, _) = char('(').parse(input)?;
    let input = skip_ws(input);
    
    let mut target_args = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        
        if trimmed.starts_with(')') {
            current = &trimmed[1..];
            break;
        }
        
        let (rest, arg) = expression(trimmed)?;
        target_args.push(arg);
        let rest = skip_ws(rest);
        
        if rest.starts_with(',') {
            current = skip_ws(&rest[1..]);
            continue;
        }
        
        if rest.starts_with(')') {
            current = &rest[1..];
            break;
        }
        
        return Err(nom::Err::Error(nom::error::Error::new(rest, nom::error::ErrorKind::Char)));
    }
    
    let current = skip_ws(current);
    
    // Parse additional arguments (after the target)
    let mut additional_args = Vec::new();
    let mut current = current;
    
    while current.starts_with(',') {
        let rest = skip_ws(&current[1..]);
        let (rest, arg) = expression(rest)?;
        additional_args.push(arg);
        current = skip_ws(rest);
    }
    
    // Expect closing parenthesis
    if !current.starts_with(')') {
        return Err(nom::Err::Error(nom::error::Error::new(current, nom::error::ErrorKind::Char)));
    }
    let current = &current[1..];
    
    // Resolve the virtual operation to a concrete OpKind
    let op_kind = match virtual_op.resolve(target) {
        Some(k) => k,
        None => {
            // Invalid combination of virtual op and target
            return Err(nom::Err::Error(nom::error::Error::new(current, nom::error::ErrorKind::Tag)));
        }
    };
    
    // Combine target_args and additional_args
    let mut all_args = target_args;
    all_args.extend(additional_args);
    
    Ok((current, Expression::Operation { kind: op_kind, args: all_args }))
}

fn identifier(input: &str) -> IResult<&str, String> {
    // First character must be letter or underscore
    if !input.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alpha)));
    }
    
    let (rest, name) = take_while1::<_, _, nom::error::Error<&str>>(|c: char| c.is_ascii_alphanumeric() || c == '_').parse(input)?;
    
    // Check for keywords
    let keywords = [
        "let", "if", "else", "for", "in", "fn", "return",
        "true", "false", "to", "downto", "and", "or", "not",
        "while", "max", "repeat", "const", "mut", "as",
    ];
    
    if keywords.contains(&name) {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
    }
    
    Ok((rest, name.to_string()))
}

fn is_ident_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let src = r#"
            fn main() {
                GPIO_SET_MODE(13, 1);
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
        let prog = result.unwrap();
        assert_eq!(prog.functions.len(), 1);
        assert_eq!(prog.functions[0].name, "main");
    }

    #[test]
    fn test_parse_let_statement() {
        let src = r#"
            fn main() {
                let x = 42;
                let mut y: u32 = 0;
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_const() {
        let src = r#"
            const LED_PIN: u8 = 13;
            fn main() {}
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
        let prog = result.unwrap();
        assert_eq!(prog.constants.len(), 1);
        assert_eq!(prog.constants[0].name, "LED_PIN");
    }

    #[test]
    fn test_parse_if_else() {
        let src = r#"
            fn main() {
                if x > 0 {
                    GPIO_WRITE(13, 1);
                } else {
                    GPIO_WRITE(13, 0);
                }
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_for_loop() {
        let src = r#"
            fn main() {
                for i in 0 to 9 {
                    GPIO_TOGGLE(13);
                }
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_repeat_loop() {
        let src = r#"
            fn main() {
                repeat 10 {
                    DELAY_MS(100);
                }
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_bounded_while() {
        let src = r#"
            fn main() {
                while x < 100 max 1000 {
                    x = x + 1;
                }
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_binary_ops() {
        let src = r#"
            fn main() {
                let a = 1 + 2 * 3;
                let b = x and y or z;
                let c = a == b;
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_function_with_params() {
        let src = r#"
            fn add(a: u32, b: u32) -> u32 {
                a + b
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
        let prog = result.unwrap();
        assert_eq!(prog.functions[0].params.len(), 2);
        assert!(prog.functions[0].return_type.is_some());
    }

    #[test]
    fn test_parse_virtual_call_write_gpio() {
        let src = r#"
            fn main() {
                WRITE(GPIO(13), 1);
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
        let prog = result.unwrap();
        let main = &prog.functions[0];
        assert_eq!(main.body.statements.len(), 1);
        // The virtual call should be parsed as an Operation with GpioWrite
        if let Statement::Expr(Expression::Operation { kind, args }) = &main.body.statements[0] {
            assert_eq!(*kind, embeem_ast::OpKind::GpioWrite);
            assert_eq!(args.len(), 2);
        } else {
            panic!("Expected Operation expression");
        }
    }

    #[test]
    fn test_parse_virtual_call_read_gpio() {
        let src = r#"
            fn main() {
                let x = READ(GPIO(5));
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_virtual_call_pwm() {
        let src = r#"
            fn main() {
                START(PWM(0));
                SET_DUTY_CYCLE(PWM(0), 128);
                STOP(PWM(0));
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_virtual_call_with_variable_target() {
        let src = r#"
            fn main() {
                let pin = 13;
                WRITE(GPIO(pin), 1);
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_virtual_call_uart() {
        let src = r#"
            fn main() {
                INIT(UART(0));
                SET_BAUD_RATE(UART(0), 9600);
                WRITE(UART(0), 65);
                let byte = READ(UART(0));
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_virtual_call_watchdog() {
        let src = r#"
            fn main() {
                ENABLE(WDT());
                RESET(WDT());
                DISABLE(WDT());
            }
        "#;
        let result = parse_program(src);
        assert!(result.is_ok());
    }
}
