//! TypeScript code generator for Embeem.
//!
//! This module generates TypeScript code from Embeem AST. It uses the mangling scheme
//! defined in `embeem_ast::mangle` for consistent name generation.
//!
//! See the [`embeem_ast::mangle`] module documentation for details on the
//! operation mangling scheme.

use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use embeem_ast::{
    mangle::{mangle_extern_function_name, mangle_function_name, mangle_operation_path, MangleConfig},
    BinaryOp, Block, ConstDecl, ElseBlock, Expression, ExternFn, Function, Literal,
    PrimitiveType, Program, RangeDirection, Statement, Type, TypeContext, UnaryOp,
    infer_expression_type,
    // Module types
    Module, Item, Import, ModulePath,
};

use crate::c::CodegenError;

/// Output mode for TypeScript generation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TsOutputMode {
    /// Flatten all modules into a single file (default).
    /// Uses mangled names for all non-root module items.
    Flattened,
    /// Generate native ESM modules.
    /// Preserves import/export statements, uses .js extension for imports.
    Esm,
}

impl Default for TsOutputMode {
    fn default() -> Self {
        Self::Flattened
    }
}

/// Options for TypeScript code generation.
#[derive(Clone, Debug)]
pub struct TsCodegenOptions {
    /// Mangling configuration for function and operation names.
    pub mangle_config: MangleConfig,
    /// Whether to emit type annotations (default: true).
    pub emit_types: bool,
    /// Whether to use `const` for immutable variables (default: true).
    pub use_const: bool,
    /// Module name to import external functions from (default: "./extern").
    pub extern_module: String,
    /// Output mode: Flattened or ESM (default: Flattened).
    pub output_mode: TsOutputMode,
    /// File extension for ESM imports (default: ".js").
    pub esm_extension: String,
}

impl Default for TsCodegenOptions {
    fn default() -> Self {
        Self {
            mangle_config: MangleConfig::default(),
            emit_types: true,
            use_const: true,
            extern_module: "./extern".to_string(),
            output_mode: TsOutputMode::default(),
            esm_extension: ".js".to_string(),
        }
    }
}

impl TsCodegenOptions {
    /// Create default options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the mangling configuration.
    pub fn with_mangle_config(mut self, config: MangleConfig) -> Self {
        self.mangle_config = config;
        self
    }

    /// Set whether to emit type annotations.
    pub fn with_types(mut self, emit: bool) -> Self {
        self.emit_types = emit;
        self
    }

    /// Set whether to use `const` for immutable variables.
    pub fn with_const(mut self, use_const: bool) -> Self {
        self.use_const = use_const;
        self
    }

    /// Set the module name to import external functions from.
    pub fn with_extern_module(mut self, module: &str) -> Self {
        self.extern_module = module.to_string();
        self
    }

    /// Set the output mode (Flattened or ESM).
    pub fn with_output_mode(mut self, mode: TsOutputMode) -> Self {
        self.output_mode = mode;
        self
    }

    /// Enable ESM output mode.
    pub fn with_esm(mut self) -> Self {
        self.output_mode = TsOutputMode::Esm;
        self
    }

    /// Set the file extension for ESM imports.
    pub fn with_esm_extension(mut self, ext: &str) -> Self {
        self.esm_extension = ext.to_string();
        self
    }
}

/// TypeScript code generator.
pub struct TsCodegen {
    output: String,
    indent: usize,
    temp_counter: usize,
    options: TsCodegenOptions,
    type_ctx: TypeContext,
}

impl TsCodegen {
    /// Create a new TypeScript code generator with default options.
    pub fn new() -> Self {
        Self::with_options(TsCodegenOptions::default())
    }

    /// Create a new TypeScript code generator with the specified options.
    pub fn with_options(options: TsCodegenOptions) -> Self {
        Self {
            output: String::new(),
            indent: 0,
            temp_counter: 0,
            options,
            type_ctx: TypeContext::new(),
        }
    }

    /// Generate TypeScript code from an Embeem program.
    pub fn generate(&mut self, program: &Program) -> Result<String, CodegenError> {
        self.output.clear();
        self.type_ctx = TypeContext::from_program(program);
        self.temp_counter = 0;

        // Generate header
        self.emit_header();

        // Generate constants
        for constant in &program.constants {
            self.emit_const(constant)?;
        }
        if !program.constants.is_empty() {
            self.emit_line("");
        }

        // Generate external function imports
        if !program.extern_fns.is_empty() {
            self.emit_extern_imports(&program.extern_fns)?;
            self.emit_line("");
        }

        // Generate function definitions
        for function in &program.functions {
            self.emit_function(function)?;
            self.emit_line("");
        }

        Ok(self.output.clone())
    }

    /// Generate TypeScript code from an Embeem module using native ESM.
    ///
    /// This generates a single ESM module file with proper import/export statements.
    /// Unlike `generate`, this preserves the module structure rather than flattening.
    ///
    /// # Arguments
    /// * `module` - The parsed module AST
    /// * `_module_path` - The path segments for this module (reserved for future use)
    pub fn generate_module(&mut self, module: &Module, _module_path: &[String]) -> Result<String, CodegenError> {
        self.output.clear();
        self.type_ctx = self.type_ctx_from_module(module);
        self.temp_counter = 0;

        // Generate header
        self.emit_esm_header();

        // Generate imports
        for import in &module.imports {
            self.emit_esm_import(import)?;
        }
        if !module.imports.is_empty() {
            self.emit_line("");
        }

        // Collect items by type
        let mut constants = Vec::new();
        let mut extern_fns = Vec::new();
        let mut functions = Vec::new();

        for item in &module.items {
            match &item.item {
                Item::Const(c) => constants.push((item.exported, c)),
                Item::ExternFn(e) => extern_fns.push((item.exported, e)),
                Item::Function(f) => functions.push((item.exported, f)),
            }
        }

        // Generate constants
        for (exported, constant) in &constants {
            self.emit_esm_const(constant, *exported)?;
        }
        if !constants.is_empty() {
            self.emit_line("");
        }

        // Generate extern function declarations/imports
        if !extern_fns.is_empty() {
            self.emit_esm_extern_fns(&extern_fns)?;
            self.emit_line("");
        }

        // Generate function definitions
        for (exported, function) in &functions {
            self.emit_esm_function(function, *exported)?;
            self.emit_line("");
        }

        Ok(self.output.clone())
    }

    /// Build a TypeContext from a Module.
    fn type_ctx_from_module(&self, module: &Module) -> TypeContext {
        let mut ctx = TypeContext::new();

        for item in &module.items {
            match &item.item {
                Item::Const(c) => {
                    ctx.add_constant(c.name.clone(), c.ty.clone());
                }
                Item::ExternFn(e) => {
                    ctx.add_extern_fn(e.name.clone(), e.return_type.clone());
                }
                Item::Function(f) => {
                    ctx.add_function(f.name.clone(), f.return_type.clone());
                }
            }
        }

        ctx
    }

    fn emit_esm_header(&mut self) {
        self.emit_line("// Generated by Embeem compiler");
        self.emit_line("// TypeScript ESM output");
        self.emit_line("");
    }

    /// Convert a ModulePath to an ESM import path string.
    fn module_path_to_esm(&self, path: &ModulePath) -> String {
        let mut result = String::new();

        if path.is_relative {
            if path.parent_count > 0 {
                for _ in 0..path.parent_count {
                    result.push_str("../");
                }
            } else {
                result.push_str("./");
            }
        }

        result.push_str(&path.segments.join("/"));
        result.push_str(&self.options.esm_extension);
        result
    }

    /// Emit an ESM import statement.
    fn emit_esm_import(&mut self, import: &Import) -> Result<(), CodegenError> {
        match import {
            Import::Named { items, path } => {
                let imports: Vec<String> = items
                    .iter()
                    .map(|spec| {
                        if let Some(alias) = &spec.alias {
                            format!("{} as {}", spec.name, alias)
                        } else {
                            spec.name.clone()
                        }
                    })
                    .collect();

                self.emit_line(&format!(
                    "import {{ {} }} from \"{}\";",
                    imports.join(", "),
                    self.module_path_to_esm(path)
                ));
            }
            Import::Namespace { alias, path } => {
                self.emit_line(&format!(
                    "import * as {} from \"{}\";",
                    alias,
                    self.module_path_to_esm(path)
                ));
            }
            Import::SideEffect { path } => {
                self.emit_line(&format!(
                    "import \"{}\";",
                    self.module_path_to_esm(path)
                ));
            }
        }
        Ok(())
    }

    /// Emit a constant with optional export.
    fn emit_esm_const(&mut self, constant: &ConstDecl, exported: bool) -> Result<(), CodegenError> {
        let value = self.expr_to_ts(&constant.value)?;
        let export_kw = if exported { "export " } else { "" };

        if self.options.emit_types {
            let ts_type = self.type_to_ts(&constant.ty);
            self.emit_line(&format!("{}const {}: {} = {};", export_kw, constant.name, ts_type, value));
        } else {
            self.emit_line(&format!("{}const {} = {};", export_kw, constant.name, value));
        }
        Ok(())
    }

    /// Emit extern function declarations.
    /// In ESM mode, these become imports from the extern module.
    fn emit_esm_extern_fns(&mut self, extern_fns: &[(bool, &ExternFn)]) -> Result<(), CodegenError> {
        if extern_fns.is_empty() {
            return Ok(());
        }

        // In ESM mode, we import extern functions from the extern module
        let names: Vec<String> = extern_fns
            .iter()
            .map(|(_, ef)| ef.name.clone())
            .collect();

        if self.options.emit_types {
            self.emit_line("// External function types:");
            for (_, extern_fn) in extern_fns {
                let params = extern_fn
                    .params
                    .iter()
                    .map(|p| format!("{}: {}", p.name, self.type_to_ts(&p.ty)))
                    .collect::<Vec<_>>()
                    .join(", ");
                let return_type = match &extern_fn.return_type {
                    Some(ty) => self.type_to_ts(ty),
                    None => "void".to_string(),
                };
                self.emit_line(&format!("//   {}({}): {}", extern_fn.name, params, return_type));
            }
        }

        // Emit the import statement
        self.emit_line(&format!(
            "import {{ {} }} from \"{}\";",
            names.join(", "),
            self.options.extern_module
        ));

        // Re-export if any are exported from this module
        let exported_names: Vec<String> = extern_fns
            .iter()
            .filter(|(exported, _)| *exported)
            .map(|(_, ef)| ef.name.clone())
            .collect();

        if !exported_names.is_empty() {
            self.emit_line(&format!("export {{ {} }};", exported_names.join(", ")));
        }

        Ok(())
    }

    /// Emit a function definition with optional export.
    fn emit_esm_function(&mut self, function: &Function, exported: bool) -> Result<(), CodegenError> {
        // Save the current type context and add function parameters
        let saved_ctx = self.type_ctx.clone();
        for param in &function.params {
            self.type_ctx.add_variable(param.name.clone(), param.ty.clone());
        }

        let export_kw = if exported { "export " } else { "" };

        if self.options.emit_types {
            let params = function
                .params
                .iter()
                .map(|p| format!("{}: {}", p.name, self.type_to_ts(&p.ty)))
                .collect::<Vec<_>>()
                .join(", ");

            let return_type = match &function.return_type {
                Some(ty) => self.type_to_ts(ty),
                None => "void".to_string(),
            };

            self.emit_line(&format!(
                "{}function {}({}): {} {{",
                export_kw, function.name, params, return_type
            ));
        } else {
            let params = function
                .params
                .iter()
                .map(|p| p.name.clone())
                .collect::<Vec<_>>()
                .join(", ");

            self.emit_line(&format!("{}function {}({}) {{", export_kw, function.name, params));
        }

        self.indent += 1;
        self.emit_block(&function.body, function.return_type.is_some())?;
        self.indent -= 1;
        self.emit_line("}");

        // Restore the type context
        self.type_ctx = saved_ctx;
        Ok(())
    }

    fn emit_header(&mut self) {
        self.emit_line("// Generated by Embeem compiler");
        self.emit_line("// TypeScript output");
        self.emit_line("");
        self.emit_line("// Operation Mangling Scheme");
        self.emit_line("// Operations use length-prefixed encoding: <prefix>$<n>_<len>_<SEG>...");
        self.emit_line("// Non-hybrid operations use op_prefix (default: embeem_op_)");
        self.emit_line("// External functions and hybrid operations use extern_prefix (default: embeem_extern_)");
        self.emit_line("");
        self.emit_line("// Note: Basic operations (ADD, SUB, MUL, etc.) are inlined as JS operators.");
        self.emit_line("// Other operations must be provided by the runtime.");
        self.emit_line("");
    }

    fn emit_const(&mut self, constant: &ConstDecl) -> Result<(), CodegenError> {
        let value = self.expr_to_ts(&constant.value)?;
        if self.options.emit_types {
            let ts_type = self.type_to_ts(&constant.ty);
            self.emit_line(&format!("export const {}: {} = {};", constant.name, ts_type, value));
        } else {
            self.emit_line(&format!("export const {} = {};", constant.name, value));
        }
        Ok(())
    }

    /// Emit import statement for all external functions.
    fn emit_extern_imports(&mut self, extern_fns: &[ExternFn]) -> Result<(), CodegenError> {
        let mangled_names: Vec<String> = extern_fns
            .iter()
            .map(|ef| mangle_extern_function_name(&ef.name, &self.options.mangle_config))
            .collect();

        if self.options.emit_types {
            // Emit type declarations as comments for documentation
            self.emit_line("// External function types:");
            for extern_fn in extern_fns {
                let mangled_name = mangle_extern_function_name(&extern_fn.name, &self.options.mangle_config);
                let params = extern_fn
                    .params
                    .iter()
                    .map(|p| format!("{}: {}", p.name, self.type_to_ts(&p.ty)))
                    .collect::<Vec<_>>()
                    .join(", ");
                let return_type = match &extern_fn.return_type {
                    Some(ty) => self.type_to_ts(ty),
                    None => "void".to_string(),
                };
                self.emit_line(&format!("//   {}({}): {}", mangled_name, params, return_type));
            }
        }

        // Emit the import statement
        self.emit_line(&format!(
            "import {{ {} }} from \"{}\";",
            mangled_names.join(", "),
            self.options.extern_module
        ));
        Ok(())
    }

    fn emit_function(&mut self, function: &Function) -> Result<(), CodegenError> {
        // Save the current type context and add function parameters
        let saved_ctx = self.type_ctx.clone();
        for param in &function.params {
            self.type_ctx.add_variable(param.name.clone(), param.ty.clone());
        }

        let mangled_name = mangle_function_name(&function.name, &self.options.mangle_config);

        if self.options.emit_types {
            let params = function
                .params
                .iter()
                .map(|p| format!("{}: {}", p.name, self.type_to_ts(&p.ty)))
                .collect::<Vec<_>>()
                .join(", ");

            let return_type = match &function.return_type {
                Some(ty) => self.type_to_ts(ty),
                None => "void".to_string(),
            };

            self.emit_line(&format!("export function {}({}): {} {{", mangled_name, params, return_type));
        } else {
            let params = function
                .params
                .iter()
                .map(|p| p.name.clone())
                .collect::<Vec<_>>()
                .join(", ");

            self.emit_line(&format!("export function {}({}) {{", mangled_name, params));
        }

        self.indent += 1;
        self.emit_block(&function.body, function.return_type.is_some())?;
        self.indent -= 1;
        self.emit_line("}");

        // Restore the type context
        self.type_ctx = saved_ctx;
        Ok(())
    }

    fn emit_block(&mut self, block: &Block, has_return: bool) -> Result<(), CodegenError> {
        for stmt in &block.statements {
            self.emit_statement(stmt)?;
        }

        if let Some(result) = &block.result {
            if has_return {
                let expr = self.expr_to_ts(result)?;
                self.emit_line(&format!("return {};", expr));
            } else {
                let expr = self.expr_to_ts(result)?;
                self.emit_line(&format!("{};", expr));
            }
        }

        Ok(())
    }

    fn emit_statement(&mut self, stmt: &Statement) -> Result<(), CodegenError> {
        match stmt {
            Statement::Let {
                name,
                mutable,
                ty,
                value,
            } => {
                let keyword = if *mutable || !self.options.use_const {
                    "let"
                } else {
                    "const"
                };

                let expr = self.expr_to_ts(value)?;

                if self.options.emit_types {
                    let inferred_ty = ty.as_ref().cloned().or_else(|| infer_expression_type(value, &self.type_ctx));
                    let ts_type = inferred_ty
                        .as_ref()
                        .map(|t| self.type_to_ts(t))
                        .unwrap_or_else(|| "number".to_string());

                    // Add the variable to the type context for future inference
                    if let Some(var_ty) = inferred_ty {
                        self.type_ctx.add_variable(name.clone(), var_ty);
                    }

                    self.emit_line(&format!("{} {}: {} = {};", keyword, name, ts_type, expr));
                } else {
                    self.emit_line(&format!("{} {} = {};", keyword, name, expr));
                }
            }

            Statement::Assign { target, value } => {
                let expr = self.expr_to_ts(value)?;
                self.emit_line(&format!("{} = {};", target, expr));
            }

            Statement::Expr(expr) => {
                let ts_expr = self.expr_to_ts(expr)?;
                self.emit_line(&format!("{};", ts_expr));
            }

            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond = self.expr_to_ts(condition)?;
                self.emit_line(&format!("if ({}) {{", cond));
                self.indent += 1;
                self.emit_block(then_block, false)?;
                self.indent -= 1;

                match else_block {
                    Some(ElseBlock::Block(blk)) => {
                        self.emit_line("} else {");
                        self.indent += 1;
                        self.emit_block(blk, false)?;
                        self.indent -= 1;
                        self.emit_line("}");
                    }
                    Some(ElseBlock::ElseIf(stmt)) => {
                        self.emit("} else ");
                        self.emit_statement(stmt)?;
                    }
                    None => {
                        self.emit_line("}");
                    }
                }
            }

            Statement::For {
                variable,
                start,
                end,
                direction,
                body,
            } => {
                let start_expr = self.expr_to_ts(start)?;
                let end_expr = self.expr_to_ts(end)?;

                let (cond, update) = match direction {
                    RangeDirection::To => (
                        format!("{} <= {}", variable, end_expr),
                        format!("{}++", variable),
                    ),
                    RangeDirection::DownTo => (
                        format!("{} >= {}", variable, end_expr),
                        format!("{}--", variable),
                    ),
                };

                self.emit_line(&format!(
                    "for (let {} = {}; {}; {}) {{",
                    variable, start_expr, cond, update
                ));
                self.indent += 1;
                self.emit_block(body, false)?;
                self.indent -= 1;
                self.emit_line("}");
            }

            Statement::Repeat { count, body } => {
                let count_expr = self.expr_to_ts(count)?;
                let loop_var = self.fresh_temp();
                self.emit_line(&format!(
                    "for (let {} = 0; {} < {}; {}++) {{",
                    loop_var, loop_var, count_expr, loop_var
                ));
                self.indent += 1;
                self.emit_block(body, false)?;
                self.indent -= 1;
                self.emit_line("}");
            }

            Statement::While {
                condition,
                max_iterations,
                body,
            } => {
                let cond = self.expr_to_ts(condition)?;
                let max = self.expr_to_ts(max_iterations)?;
                let counter = self.fresh_temp();
                self.emit_line(&format!("let {} = 0;", counter));
                self.emit_line(&format!(
                    "while (({}) && ({} < {})) {{",
                    cond, counter, max
                ));
                self.indent += 1;
                self.emit_block(body, false)?;
                self.emit_line(&format!("{}++;", counter));
                self.indent -= 1;
                self.emit_line("}");
            }
        }

        Ok(())
    }

    fn expr_to_ts(&mut self, expr: &Expression) -> Result<String, CodegenError> {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::Integer(n) => Ok(format!("{}", n)),
                Literal::Float(f) => Ok(format!("{}", f)),
                Literal::Bool(b) => Ok(if *b { "true".to_string() } else { "false".to_string() }),
            },

            Expression::Identifier(name) => Ok(name.clone()),

            Expression::QualifiedIdentifier { namespace, name } => {
                // For qualified identifiers (namespace::name), we emit JavaScript dot notation
                // assuming the namespace has been resolved to a module object
                Ok(format!("{}.{}", namespace, name))
            }

            Expression::Binary { op, left, right } => {
                let l = self.expr_to_ts(left)?;
                let r = self.expr_to_ts(right)?;
                let op_str = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                    BinaryOp::Mod => "%",
                    BinaryOp::BitAnd => "&",
                    BinaryOp::BitOr => "|",
                    BinaryOp::BitXor => "^",
                    BinaryOp::Shl => "<<",
                    BinaryOp::Shr => ">>",
                    BinaryOp::LogicalShr => ">>>",
                    BinaryOp::Eq => "===",
                    BinaryOp::Ne => "!==",
                    BinaryOp::Lt => "<",
                    BinaryOp::Le => "<=",
                    BinaryOp::Gt => ">",
                    BinaryOp::Ge => ">=",
                    BinaryOp::And => "&&",
                    BinaryOp::Or => "||",
                };
                Ok(format!("({} {} {})", l, op_str, r))
            }

            Expression::Unary { op, operand } => {
                let o = self.expr_to_ts(operand)?;
                let op_str = match op {
                    UnaryOp::Neg => "-",
                    UnaryOp::BitNot => "~",
                    UnaryOp::Not => "!",
                };
                Ok(format!("({}{})", op_str, o))
            }

            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.expr_to_ts(condition)?;
                // For simple expressions, use ternary operator
                if then_branch.statements.is_empty() && else_branch.statements.is_empty() {
                    if let (Some(then_expr), Some(else_expr)) =
                        (&then_branch.result, &else_branch.result)
                    {
                        let t = self.expr_to_ts(then_expr)?;
                        let e = self.expr_to_ts(else_expr)?;
                        return Ok(format!("({} ? {} : {})", cond, t, e));
                    }
                }
                // For complex blocks, use IIFE (Immediately Invoked Function Expression)
                self.emit_iife_if_expr(condition, then_branch, else_branch)
            }

            Expression::Operation { path, extern_fn, args } => {
                self.emit_operation_path(path, extern_fn.as_deref(), args)
            }

            Expression::Call { function, args } => {
                let arg_strs: Result<Vec<_>, _> =
                    args.iter().map(|a| self.expr_to_ts(a)).collect();
                let arg_strs = arg_strs?;
                let name = if self.type_ctx.is_extern_fn(function) {
                    mangle_extern_function_name(function, &self.options.mangle_config)
                } else {
                    mangle_function_name(function, &self.options.mangle_config)
                };
                Ok(format!("{}({})", name, arg_strs.join(", ")))
            }

            Expression::QualifiedCall { namespace, function, args } => {
                let arg_strs: Result<Vec<_>, _> =
                    args.iter().map(|a| self.expr_to_ts(a)).collect();
                let arg_strs = arg_strs?;
                // For qualified calls (namespace::function()), we emit JavaScript dot notation
                Ok(format!("{}.{}({})", namespace, function, arg_strs.join(", ")))
            }

            Expression::Block(blk) => {
                self.emit_iife_block(blk)
            }

            Expression::Index { array, index } => {
                let arr = self.expr_to_ts(array)?;
                let idx = self.expr_to_ts(index)?;
                Ok(format!("{}[{}]", arr, idx))
            }

            Expression::Array(elements) => {
                let elem_strs: Result<Vec<_>, _> =
                    elements.iter().map(|e| self.expr_to_ts(e)).collect();
                let elem_strs = elem_strs?;
                Ok(format!("[{}]", elem_strs.join(", ")))
            }

            Expression::Cast { value, ty } => {
                let v = self.expr_to_ts(value)?;
                // TypeScript casts use 'as' syntax
                if self.options.emit_types {
                    let ts_type = self.type_to_ts(ty);
                    Ok(format!("({} as {})", v, ts_type))
                } else {
                    // Without types, just pass the value through
                    Ok(v)
                }
            }
        }
    }

    /// Emit an operation call with a path and optional extern function.
    fn emit_operation_path(&mut self, path: &[String], extern_fn: Option<&str>, args: &[Expression]) -> Result<String, CodegenError> {
        let arg_strs: Result<Vec<_>, _> = args.iter().map(|a| self.expr_to_ts(a)).collect();
        let arg_strs = arg_strs?;

        // For non-hybrid operations, check if we can use inline JS operators
        if extern_fn.is_none() && path.len() == 1 {
            let op = path[0].as_str();
            match (op, arg_strs.len()) {
                ("ADD", 2) => return Ok(format!("({} + {})", arg_strs[0], arg_strs[1])),
                ("SUB", 2) => return Ok(format!("({} - {})", arg_strs[0], arg_strs[1])),
                ("MUL", 2) => return Ok(format!("({} * {})", arg_strs[0], arg_strs[1])),
                ("DIV", 2) => return Ok(format!("({1} !== 0 ? ({0}) / ({1}) : 0)", arg_strs[0], arg_strs[1])),
                ("MOD", 2) => return Ok(format!("({1} !== 0 ? ({0}) % ({1}) : 0)", arg_strs[0], arg_strs[1])),
                ("INC", 1) => return Ok(format!("({} + 1)", arg_strs[0])),
                ("DEC", 1) => return Ok(format!("({} - 1)", arg_strs[0])),
                ("NEG", 1) => return Ok(format!("(-{})", arg_strs[0])),
                ("ABS", 1) => return Ok(format!("Math.abs({})", arg_strs[0])),
                ("AND", 2) => return Ok(format!("({} & {})", arg_strs[0], arg_strs[1])),
                ("OR", 2) => return Ok(format!("({} | {})", arg_strs[0], arg_strs[1])),
                ("XOR", 2) => return Ok(format!("({} ^ {})", arg_strs[0], arg_strs[1])),
                ("NOT", 1) => return Ok(format!("(~{})", arg_strs[0])),
                ("SHL", 2) => return Ok(format!("({} << {})", arg_strs[0], arg_strs[1])),
                ("SHR", 2) => return Ok(format!("({} >>> {})", arg_strs[0], arg_strs[1])),
                ("EQ", 2) => return Ok(format!("({} === {})", arg_strs[0], arg_strs[1])),
                ("NE", 2) => return Ok(format!("({} !== {})", arg_strs[0], arg_strs[1])),
                ("LT", 2) => return Ok(format!("({} < {})", arg_strs[0], arg_strs[1])),
                ("LE", 2) => return Ok(format!("({} <= {})", arg_strs[0], arg_strs[1])),
                ("GT", 2) => return Ok(format!("({} > {})", arg_strs[0], arg_strs[1])),
                ("GE", 2) => return Ok(format!("({} >= {})", arg_strs[0], arg_strs[1])),
                ("NOP", 0) => return Ok("undefined".to_string()),
                // Floating point operations
                ("FSQRT", 1) => return Ok(format!("Math.sqrt({})", arg_strs[0])),
                ("FABS", 1) => return Ok(format!("Math.abs({})", arg_strs[0])),
                ("FADD", 2) => return Ok(format!("({} + {})", arg_strs[0], arg_strs[1])),
                ("FSUB", 2) => return Ok(format!("({} - {})", arg_strs[0], arg_strs[1])),
                ("FMUL", 2) => return Ok(format!("({} * {})", arg_strs[0], arg_strs[1])),
                ("FDIV", 2) => return Ok(format!("({} / {})", arg_strs[0], arg_strs[1])),
                _ => {}
            }
        }

        // For all other operations, use the mangled function name
        let op_name = mangle_operation_path(path, extern_fn, &self.options.mangle_config);
        Ok(format!("{}({})", op_name, arg_strs.join(", ")))
    }

    /// Emit a block as an IIFE (Immediately Invoked Function Expression).
    fn emit_iife_block(&mut self, block: &Block) -> Result<String, CodegenError> {
        let mut parts = Vec::new();
        
        for stmt in &block.statements {
            parts.push(self.emit_statement_to_string(stmt)?);
        }
        
        let result = if let Some(ref expr) = block.result {
            format!("return {};", self.expr_to_ts(expr)?)
        } else {
            String::new()
        };

        if parts.is_empty() && !result.is_empty() {
            // Simple case: just return the expression
            Ok(format!("(() => {})()", result.trim_start_matches("return ").trim_end_matches(';')))
        } else {
            let body = if result.is_empty() {
                parts.join(" ")
            } else {
                format!("{} {}", parts.join(" "), result)
            };
            Ok(format!("(() => {{ {} }})()", body))
        }
    }

    /// Emit an if expression as an IIFE.
    fn emit_iife_if_expr(
        &mut self,
        condition: &Expression,
        then_branch: &Block,
        else_branch: &Block,
    ) -> Result<String, CodegenError> {
        let cond = self.expr_to_ts(condition)?;
        
        let mut then_parts = Vec::new();
        for stmt in &then_branch.statements {
            then_parts.push(self.emit_statement_to_string(stmt)?);
        }
        let then_result = if let Some(ref expr) = then_branch.result {
            format!("return {};", self.expr_to_ts(expr)?)
        } else {
            String::new()
        };
        
        let mut else_parts = Vec::new();
        for stmt in &else_branch.statements {
            else_parts.push(self.emit_statement_to_string(stmt)?);
        }
        let else_result = if let Some(ref expr) = else_branch.result {
            format!("return {};", self.expr_to_ts(expr)?)
        } else {
            String::new()
        };
        
        let then_body = if then_result.is_empty() {
            then_parts.join(" ")
        } else {
            format!("{} {}", then_parts.join(" "), then_result)
        };
        
        let else_body = if else_result.is_empty() {
            else_parts.join(" ")
        } else {
            format!("{} {}", else_parts.join(" "), else_result)
        };
        
        Ok(format!(
            "(() => {{ if ({}) {{ {} }} else {{ {} }} }})()",
            cond, then_body, else_body
        ))
    }

    /// Emit a statement to a string (for use in IIFEs).
    fn emit_statement_to_string(&mut self, stmt: &Statement) -> Result<String, CodegenError> {
        match stmt {
            Statement::Let {
                name,
                mutable,
                ty,
                value,
            } => {
                let keyword = if *mutable || !self.options.use_const { "let" } else { "const" };
                let expr = self.expr_to_ts(value)?;
                
                if self.options.emit_types {
                    let ts_type = ty
                        .as_ref()
                        .map(|t| self.type_to_ts(t))
                        .unwrap_or_else(|| "number".to_string());
                    Ok(format!("{} {}: {} = {};", keyword, name, ts_type, expr))
                } else {
                    Ok(format!("{} {} = {};", keyword, name, expr))
                }
            }

            Statement::Assign { target, value } => {
                let expr = self.expr_to_ts(value)?;
                Ok(format!("{} = {};", target, expr))
            }

            Statement::Expr(expr) => {
                let ts_expr = self.expr_to_ts(expr)?;
                Ok(format!("{};", ts_expr))
            }

            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond = self.expr_to_ts(condition)?;
                let mut result = format!("if ({}) {{ ", cond);
                
                for s in &then_block.statements {
                    result.push_str(&self.emit_statement_to_string(s)?);
                    result.push(' ');
                }
                if let Some(ref expr) = then_block.result {
                    result.push_str(&self.expr_to_ts(expr)?);
                    result.push_str("; ");
                }
                
                match else_block {
                    Some(ElseBlock::Block(blk)) => {
                        result.push_str("} else { ");
                        for s in &blk.statements {
                            result.push_str(&self.emit_statement_to_string(s)?);
                            result.push(' ');
                        }
                        if let Some(ref expr) = blk.result {
                            result.push_str(&self.expr_to_ts(expr)?);
                            result.push_str("; ");
                        }
                        result.push('}');
                    }
                    Some(ElseBlock::ElseIf(s)) => {
                        result.push_str("} else ");
                        result.push_str(&self.emit_statement_to_string(s)?);
                    }
                    None => {
                        result.push('}');
                    }
                }
                
                Ok(result)
            }

            Statement::For {
                variable,
                start,
                end,
                direction,
                body,
            } => {
                let start_expr = self.expr_to_ts(start)?;
                let end_expr = self.expr_to_ts(end)?;
                
                let (cond, update) = match direction {
                    RangeDirection::To => (
                        format!("{} <= {}", variable, end_expr),
                        format!("{}++", variable),
                    ),
                    RangeDirection::DownTo => (
                        format!("{} >= {}", variable, end_expr),
                        format!("{}--", variable),
                    ),
                };
                
                let mut result = format!(
                    "for (let {} = {}; {}; {}) {{ ",
                    variable, start_expr, cond, update
                );
                
                for s in &body.statements {
                    result.push_str(&self.emit_statement_to_string(s)?);
                    result.push(' ');
                }
                if let Some(ref expr) = body.result {
                    result.push_str(&self.expr_to_ts(expr)?);
                    result.push_str("; ");
                }
                
                result.push('}');
                Ok(result)
            }

            Statement::Repeat { count, body } => {
                let count_expr = self.expr_to_ts(count)?;
                let loop_var = self.fresh_temp();
                
                let mut result = format!(
                    "for (let {} = 0; {} < {}; {}++) {{ ",
                    loop_var, loop_var, count_expr, loop_var
                );
                
                for s in &body.statements {
                    result.push_str(&self.emit_statement_to_string(s)?);
                    result.push(' ');
                }
                if let Some(ref expr) = body.result {
                    result.push_str(&self.expr_to_ts(expr)?);
                    result.push_str("; ");
                }
                
                result.push('}');
                Ok(result)
            }

            Statement::While {
                condition,
                max_iterations,
                body,
            } => {
                let cond = self.expr_to_ts(condition)?;
                let max = self.expr_to_ts(max_iterations)?;
                let counter = self.fresh_temp();
                
                let mut result = format!("let {} = 0; ", counter);
                result.push_str(&format!(
                    "while (({}) && ({} < {})) {{ ",
                    cond, counter, max
                ));
                
                for s in &body.statements {
                    result.push_str(&self.emit_statement_to_string(s)?);
                    result.push(' ');
                }
                if let Some(ref expr) = body.result {
                    result.push_str(&self.expr_to_ts(expr)?);
                    result.push_str("; ");
                }
                
                result.push_str(&format!("{}++; }}", counter));
                Ok(result)
            }
        }
    }

    fn type_to_ts(&self, ty: &Type) -> String {
        match ty {
            Type::Primitive(prim) => match prim {
                PrimitiveType::U8
                | PrimitiveType::U16
                | PrimitiveType::U32
                | PrimitiveType::U64
                | PrimitiveType::I8
                | PrimitiveType::I16
                | PrimitiveType::I32
                | PrimitiveType::I64
                | PrimitiveType::F32
                | PrimitiveType::F64 => "number".to_string(),
                PrimitiveType::Bool => "boolean".to_string(),
            },
            Type::Array(elem, _size) => {
                format!("{}[]", self.type_to_ts(elem))
            }
            Type::Tuple(types) => {
                let parts: Vec<_> = types.iter().map(|t| self.type_to_ts(t)).collect();
                format!("[{}]", parts.join(", "))
            }
        }
    }

    fn fresh_temp(&mut self) -> String {
        let name = format!("_tmp{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    fn emit(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn emit_line(&mut self, s: &str) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
        self.output.push_str(s);
        self.output.push('\n');
    }
}

impl Default for TsCodegen {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to compile an Embeem program to TypeScript.
pub fn compile_to_ts(program: &Program) -> Result<String, CodegenError> {
    let mut codegen = TsCodegen::new();
    codegen.generate(program)
}

/// Convenience function to compile an Embeem program to TypeScript with specified options.
pub fn compile_to_ts_with_options(program: &Program, options: TsCodegenOptions) -> Result<String, CodegenError> {
    let mut codegen = TsCodegen::with_options(options);
    codegen.generate(program)
}

/// Convenience function to compile an Embeem program to JavaScript (TypeScript without types).
pub fn compile_to_js(program: &Program) -> Result<String, CodegenError> {
    let options = TsCodegenOptions::new().with_types(false);
    let mut codegen = TsCodegen::with_options(options);
    codegen.generate(program)
}

