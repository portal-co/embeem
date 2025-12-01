//! Parser implementation for Embeem.
//!
//! This module implements a parser for the Embeem language following the
//! grammar specification in `spec/SPECIFICATION.md`.

use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use alloc::vec;

use embeem_ast::{
    BinaryOp, Block, ConstDecl, ElseBlock, Expression, ExternFn, Function, Literal, Param,
    PrimitiveType, Program, RangeDirection, Statement, Type, UnaryOp,
    is_upper_snake_case,
    // Module system types
    Module, ModuleItem, Item, Import, ImportSpec, ModulePath,
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

/// Parse an Embeem module from source code.
///
/// A module can contain imports, exports, and regular items (functions, constants, extern fns).
pub fn parse_module(input: &str) -> Result<Module, ParseError> {
    let input = skip_ws(input);
    match module(input) {
        Ok((remaining, m)) => {
            let remaining = skip_ws(remaining);
            if remaining.is_empty() {
                Ok(m)
            } else {
                Err(ParseError::new("unexpected input after module"))
            }
        }
        Err(_) => Err(ParseError::new("failed to parse module")),
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

/// Parse a module.
///
/// From spec Section 11.1 (EBNF Grammar):
/// ```text
/// module      = { module_item } ;
/// module_item = import_decl | export_decl | item ;
/// ```
fn module(input: &str) -> IResult<&str, Module> {
    let mut imports = Vec::new();
    let mut items = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        if trimmed.is_empty() {
            break;
        }
        
        // Try to parse import
        if let Ok((rest, imp)) = import_decl(trimmed) {
            imports.push(imp);
            current = rest;
            continue;
        }
        
        // Try to parse export
        if let Ok((rest, (exported, item))) = export_decl(trimmed) {
            items.push(ModuleItem { exported, item });
            current = rest;
            continue;
        }
        
        // Try to parse regular item (not exported)
        if let Ok((rest, c)) = const_decl(trimmed) {
            items.push(ModuleItem { exported: false, item: Item::Const(c) });
            current = rest;
            continue;
        }
        
        if let Ok((rest, e)) = extern_fn(trimmed) {
            items.push(ModuleItem { exported: false, item: Item::ExternFn(e) });
            current = rest;
            continue;
        }
        
        if let Ok((rest, f)) = function(trimmed) {
            items.push(ModuleItem { exported: false, item: Item::Function(f) });
            current = rest;
            continue;
        }
        
        break;
    }
    
    Ok((current, Module { imports, items }))
}

/// Parse an import declaration.
///
/// From spec Section 11.1 (EBNF Grammar):
/// ```text
/// import_decl    = named_import | namespace_import | side_effect_import ;
/// named_import   = "import" "{" import_list "}" "from" MODULE_PATH ";" ;
/// namespace_import = "import" "*" "as" IDENT "from" MODULE_PATH ";" ;
/// side_effect_import = "import" MODULE_PATH ";" ;
/// ```
fn import_decl(input: &str) -> IResult<&str, Import> {
    let (input, _) = tag("import").parse(input)?;
    let input = skip_ws(input);
    
    // Try namespace import: import * as name from "path";
    if input.starts_with('*') {
        let (input, _) = char('*').parse(input)?;
        let input = skip_ws(input);
        let (input, _) = tag("as").parse(input)?;
        let input = skip_ws(input);
        let (input, alias) = identifier(input)?;
        let input = skip_ws(input);
        let (input, _) = tag("from").parse(input)?;
        let input = skip_ws(input);
        let (input, path) = module_path(input)?;
        let input = skip_ws(input);
        let (input, _) = char(';').parse(input)?;
        
        return Ok((input, Import::Namespace { alias, path }));
    }
    
    // Try named import: import { a, b } from "path";
    if input.starts_with('{') {
        let (input, _) = char('{').parse(input)?;
        let input = skip_ws(input);
        let (input, items) = import_list(input)?;
        let input = skip_ws(input);
        let (input, _) = char('}').parse(input)?;
        let input = skip_ws(input);
        let (input, _) = tag("from").parse(input)?;
        let input = skip_ws(input);
        let (input, path) = module_path(input)?;
        let input = skip_ws(input);
        let (input, _) = char(';').parse(input)?;
        
        return Ok((input, Import::Named { items, path }));
    }
    
    // Side effect import: import "path";
    let (input, path) = module_path(input)?;
    let input = skip_ws(input);
    let (input, _) = char(';').parse(input)?;
    
    Ok((input, Import::SideEffect { path }))
}

/// Parse an import list.
///
/// From spec Section 11.1 (EBNF Grammar):
/// ```text
/// import_list    = import_spec { "," import_spec } ;
/// import_spec    = IDENT [ "as" IDENT ] ;
/// ```
fn import_list(input: &str) -> IResult<&str, Vec<ImportSpec>> {
    let mut items = Vec::new();
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        
        // Check for end of list
        if trimmed.starts_with('}') {
            return Ok((trimmed, items));
        }
        
        // Parse import spec
        if let Ok((rest, spec)) = import_spec(trimmed) {
            items.push(spec);
            let rest = skip_ws(rest);
            if rest.starts_with(',') {
                current = skip_ws(&rest[1..]);
                continue;
            }
            current = rest;
            continue;
        }
        
        break;
    }
    
    Ok((current, items))
}

/// Parse an import specifier.
fn import_spec(input: &str) -> IResult<&str, ImportSpec> {
    let (input, name) = identifier(input)?;
    let input = skip_ws(input);
    
    // Check for alias
    let (input, alias) = if input.starts_with("as") && !is_ident_continue(input.chars().nth(2)) {
        let (input, _) = tag("as").parse(input)?;
        let input = skip_ws(input);
        let (input, alias) = identifier(input)?;
        (input, Some(alias))
    } else {
        (input, None)
    };
    
    Ok((input, ImportSpec { name, alias }))
}

/// Parse a module path.
///
/// From spec Section 11.1 (EBNF Grammar):
/// ```text
/// MODULE_PATH = STRING | IDENT { "/" IDENT } ;
/// ```
fn module_path(input: &str) -> IResult<&str, ModulePath> {
    // Try string literal first: "path/to/module"
    if input.starts_with('"') {
        let (input, path_str) = string_literal(input)?;
        return Ok((input, parse_module_path_string(&path_str)));
    }
    
    // Parse identifier path: path/to/module
    let (input, first) = identifier(input)?;
    let mut segments = vec![first];
    let mut current = input;
    
    loop {
        let trimmed = skip_ws(current);
        if trimmed.starts_with('/') {
            let (rest, _) = char('/').parse(trimmed)?;
            let rest = skip_ws(rest);
            if let Ok((rest, segment)) = identifier(rest) {
                segments.push(segment);
                current = rest;
                continue;
            }
        }
        break;
    }
    
    Ok((current, ModulePath::package(segments)))
}

/// Parse a module path from a string literal.
fn parse_module_path_string(s: &str) -> ModulePath {
    let mut parent_count = 0;
    let mut is_relative = false;
    let mut path = s;
    
    // Check for relative paths
    if path.starts_with("./") {
        is_relative = true;
        path = &path[2..];
    } else if path.starts_with("../") {
        is_relative = true;
        while path.starts_with("../") {
            parent_count += 1;
            path = &path[3..];
        }
    }
    
    // Split remaining path into segments
    let segments: Vec<String> = path.split('/').map(|s| s.to_string()).collect();
    
    ModulePath {
        segments,
        is_relative,
        parent_count,
    }
}

/// Parse a string literal.
fn string_literal(input: &str) -> IResult<&str, String> {
    let (input, _) = char('"').parse(input)?;
    
    let mut result = String::new();
    let mut chars = input.char_indices();
    
    loop {
        match chars.next() {
            Some((i, '"')) => {
                return Ok((&input[i + 1..], result));
            }
            Some((_, '\\')) => {
                // Handle escape sequences
                match chars.next() {
                    Some((_, 'n')) => result.push('\n'),
                    Some((_, 't')) => result.push('\t'),
                    Some((_, 'r')) => result.push('\r'),
                    Some((_, '\\')) => result.push('\\'),
                    Some((_, '"')) => result.push('"'),
                    _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char))),
                }
            }
            Some((_, c)) => result.push(c),
            None => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Eof))),
        }
    }
}

/// Parse an export declaration.
///
/// From spec Section 11.1 (EBNF Grammar):
/// ```text
/// export_decl    = "export" ( function | const_decl | extern_fn )
///                | "export" "{" export_list "}" [ "from" MODULE_PATH ] ";"
///                | "export" "*" "from" MODULE_PATH ";"
///                | "export" "*" "as" IDENT "from" MODULE_PATH ";" ;
/// ```
///
/// Returns (is_exported, item) for regular items, or handles re-exports separately.
fn export_decl(input: &str) -> IResult<&str, (bool, Item)> {
    let (input, _) = tag("export").parse(input)?;
    let input = skip_ws(input);
    
    // Try export function
    if let Ok((rest, f)) = function(input) {
        return Ok((rest, (true, Item::Function(f))));
    }
    
    // Try export const
    if let Ok((rest, c)) = const_decl(input) {
        return Ok((rest, (true, Item::Const(c))));
    }
    
    // Try export extern fn
    if let Ok((rest, e)) = extern_fn(input) {
        return Ok((rest, (true, Item::ExternFn(e))));
    }
    
    // For now, we don't handle re-exports in this basic implementation
    // (export { ... } from "..." and export * from "...")
    // Those would require more complex handling during module resolution
    
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
}

/// Helper to check if a character can continue an identifier.
fn is_ident_continue(c: Option<char>) -> bool {
    match c {
        Some(c) => c.is_alphanumeric() || c == '_',
        None => false,
    }
}

/// Parse a program.
///
/// From spec Section 10.1 (EBNF Grammar):
/// ```text
/// program     = { item } ;
/// item        = function | const_decl | extern_fn ;
/// ```
fn program(input: &str) -> IResult<&str, Program> {
    let mut constants = Vec::new();
    let mut extern_fns = Vec::new();
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
        
        if let Ok((rest, e)) = extern_fn(trimmed) {
            extern_fns.push(e);
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
    
    Ok((current, Program { constants, extern_fns, functions }))
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

/// Parse an external function declaration.
///
/// Syntax:
/// ```text
/// extern_fn   = "extern" "fn" IDENT "(" [ param_list ] ")" [ "->" type ] ";" ;
/// ```
fn extern_fn(input: &str) -> IResult<&str, ExternFn> {
    let (input, _) = tag("extern").parse(input)?;
    let input = skip_ws(input);
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
    let (input, _) = char(';').parse(input)?;
    
    Ok((input, ExternFn { name, params, return_type }))
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

/// Parse an operation, function call, qualified identifier, or identifier.
///
/// Operations are represented as paths of UPPER_SNAKE_CASE segments:
/// - `FSUB(a, b)` -> path: ["FSUB"], args: [a, b]
/// - `WRITE(GPIO(pin), value)` -> path: ["WRITE", "GPIO"], args: [pin, value]
/// - `A(B(C(x), y), z)` -> path: ["A", "B", "C"], args: [x, y, z]
///
/// Qualified identifiers use `::` syntax:
/// - `namespace::name` -> QualifiedIdentifier { namespace, name }
/// - `namespace::func()` -> QualifiedCall { namespace, function, args }
fn operation_or_call_or_ident(input: &str) -> IResult<&str, Expression> {
    let (input, name) = identifier(input)?;
    let input_after_name = skip_ws(input);
    
    // Check for qualified identifier (namespace::name)
    if input_after_name.starts_with("::") {
        let (input, _) = tag("::").parse(input_after_name)?;
        let input = skip_ws(input);
        let (input, qualified_name) = identifier(input)?;
        let input_after_qualified = skip_ws(input);
        
        // Check if it's a qualified function call
        if input_after_qualified.starts_with('(') {
            let (input, _) = char('(').parse(input_after_qualified)?;
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
            
            return Ok((current, Expression::QualifiedCall {
                namespace: name,
                function: qualified_name,
                args,
            }));
        }
        
        // It's a qualified identifier (namespace::name)
        return Ok((input, Expression::QualifiedIdentifier {
            namespace: name,
            name: qualified_name,
        }));
    }
    
    // Check if followed by '('
    if !input_after_name.starts_with('(') {
        return Ok((input, Expression::Identifier(name)));
    }
    
    // Check if this is an operation (UPPER_SNAKE_CASE)
    if is_upper_snake_case(&name) {
        return parse_operation_call(input_after_name, vec![name]);
    }
    
    // It's a regular function call - parse arguments
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
    
    Ok((current, Expression::Call { function: name, args }))
}

/// Parse an operation call with a given path prefix.
///
/// The input starts at the opening parenthesis.
/// The path contains the UPPER_SNAKE_CASE segments collected so far.
///
/// This function recursively builds the operation path by checking if the first
/// argument is also an UPPER_SNAKE_CASE operation call, or a non-UPPER_SNAKE_CASE
/// function call (for hybrid operations with external functions).
fn parse_operation_call(input: &str, mut path: Vec<String>) -> IResult<&str, Expression> {
    let (input, _) = char('(').parse(input)?;
    let input = skip_ws(input);
    
    // Check if this is an empty argument list
    if input.starts_with(')') {
        return Ok((&input[1..], Expression::Operation { path, extern_fn: None, args: Vec::new() }));
    }
    
    // Try to parse the first argument - it might be a nested operation or hybrid extern fn
    let mut all_args = Vec::new();
    let mut current = input;
    let mut extern_fn_name: Option<String> = None;
    
    // Try to parse as identifier first to check for nested operations or hybrid calls
    if let Ok((first_name_rest, first_name)) = identifier(current) {
        let after_first_name = skip_ws(first_name_rest);
        
        if is_upper_snake_case(&first_name) && after_first_name.starts_with('(') {
            // This is a nested UPPER_SNAKE_CASE operation - add to path and recurse
            path.push(first_name);
            
            // Parse the nested operation's arguments
            let (rest, nested_expr) = parse_operation_call(after_first_name, path)?;
            
            // Extract args and extern_fn from nested operation
            if let Expression::Operation { path: nested_path, extern_fn: nested_extern_fn, args: nested_args } = nested_expr {
                // Return with the full path and nested args as first args
                all_args.extend(nested_args);
                current = rest;
                path = nested_path;
                extern_fn_name = nested_extern_fn;
            } else {
                unreachable!("parse_operation_call always returns Operation");
            }
        } else if !is_upper_snake_case(&first_name) && after_first_name.starts_with('(') {
            // This is a hybrid operation with an external function as the last segment
            // e.g., WRITE(GPIO(my_sensor(ch)), value)
            extern_fn_name = Some(first_name);
            
            // Parse the extern fn's arguments
            let (after_paren, _) = char('(').parse(after_first_name)?;
            let after_paren = skip_ws(after_paren);
            
            // Parse arguments for the extern fn call
            if !after_paren.starts_with(')') {
                let (rest, first_arg) = expression(after_paren)?;
                all_args.push(first_arg);
                let mut rest = skip_ws(rest);
                
                while rest.starts_with(',') {
                    let after_comma = skip_ws(&rest[1..]);
                    let (new_rest, arg) = expression(after_comma)?;
                    all_args.push(arg);
                    rest = skip_ws(new_rest);
                }
                current = rest;
            } else {
                current = after_paren;
            }
            
            // Expect closing parenthesis for the extern fn call
            if !current.starts_with(')') {
                return Err(nom::Err::Error(nom::error::Error::new(current, nom::error::ErrorKind::Char)));
            }
            current = skip_ws(&current[1..]);
        } else {
            // First argument is a regular expression - parse it from the beginning
            let (rest, first_arg) = expression(current)?;
            all_args.push(first_arg);
            current = skip_ws(rest);
        }
    } else {
        // First argument is not an identifier, parse as regular expression
        let (rest, first_arg) = expression(current)?;
        all_args.push(first_arg);
        current = skip_ws(rest);
    }
    
    // Parse remaining arguments (only if we don't have a hybrid with extern_fn)
    while current.starts_with(',') {
        let rest = skip_ws(&current[1..]);
        let (rest, arg) = expression(rest)?;
        all_args.push(arg);
        current = skip_ws(rest);
    }
    
    // Expect closing parenthesis
    if !current.starts_with(')') {
        return Err(nom::Err::Error(nom::error::Error::new(current, nom::error::ErrorKind::Char)));
    }
    let current = &current[1..];
    
    Ok((current, Expression::Operation { path, extern_fn: extern_fn_name, args: all_args }))
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
        "import", "export", "from", "extern",
    ];
    
    if keywords.contains(&name) {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
    }
    
    Ok((rest, name.to_string()))
}

fn is_ident_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}