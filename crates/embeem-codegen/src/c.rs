//! C code generator for Embeem.
//!
//! This module generates C code from Embeem AST. It uses the mangling scheme
//! defined in `embeem_ast::mangle` for consistent name generation.
//!
//! See the [`embeem_ast::mangle`] module documentation for details on the
//! operation mangling scheme.

use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use embeem_ast::{
    mangle::{
        mangle_extern_function_name, mangle_function_name, mangle_operation_path,
        mangle_module_function_name, mangle_module_constant_name, mangle_module_extern_function_name,
        MangleConfig,
    },
    BinaryOp, Block, ConstDecl, ElseBlock, Expression, ExternFn, Function, Literal,
    PrimitiveType, Program, RangeDirection, Statement, Type, TypeContext, UnaryOp,
    infer_expression_type,
    // Module types
    Module, Item,
};

/// Code generation error.
#[derive(Clone, Debug, PartialEq)]
pub struct CodegenError {
    pub message: String,
}

impl CodegenError {
    pub fn new(msg: &str) -> Self {
        Self {
            message: msg.to_string(),
        }
    }
}

/// Options for C code generation.
#[derive(Clone, Debug)]
pub struct CCodegenOptions {
    /// Use GCC statement expressions `({ ... })` for block expressions and complex if expressions.
    /// This is a GCC extension that allows embedding statements inside expressions.
    /// When enabled, complex control flow can be inlined as expressions.
    /// When disabled, such constructs may produce errors or require restructuring.
    pub use_gcc_statement_exprs: bool,
    /// Mangling configuration for function and operation names.
    pub mangle_config: MangleConfig,
}

impl Default for CCodegenOptions {
    fn default() -> Self {
        Self {
            use_gcc_statement_exprs: false,
            mangle_config: MangleConfig::default(),
        }
    }
}

impl CCodegenOptions {
    /// Create default options (standard C, no extensions).
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable GCC statement expressions.
    pub fn with_gcc_extensions(mut self) -> Self {
        self.use_gcc_statement_exprs = true;
        self
    }

    /// Set the mangling configuration.
    pub fn with_mangle_config(mut self, config: MangleConfig) -> Self {
        self.mangle_config = config;
        self
    }

    /// Set the function name mangling prefix.
    pub fn with_mangle_prefix(mut self, prefix: &str) -> Self {
        self.mangle_config.fn_prefix = prefix.to_string();
        self
    }

    /// Set the operation function prefix.
    pub fn with_op_prefix(mut self, prefix: &str) -> Self {
        self.mangle_config.op_prefix = prefix.to_string();
        self
    }

    /// Set the external function and hybrid operation prefix.
    pub fn with_extern_prefix(mut self, prefix: &str) -> Self {
        self.mangle_config.extern_prefix = prefix.to_string();
        self
    }
}

/// C code generator.
pub struct CCodegen {
    output: String,
    indent: usize,
    temp_counter: usize,
    options: CCodegenOptions,
    type_ctx: TypeContext,
}

impl CCodegen {
    /// Create a new code generator with default options.
    pub fn new() -> Self {
        Self::with_options(CCodegenOptions::default())
    }

    /// Create a new code generator with the specified options.
    pub fn with_options(options: CCodegenOptions) -> Self {
        Self {
            output: String::new(),
            indent: 0,
            temp_counter: 0,
            options,
            type_ctx: TypeContext::new(),
        }
    }

    /// Generate C code from an Embeem program.
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

        // Generate external function declarations
        if !program.extern_fns.is_empty() {
            self.emit_line("/* External function declarations */");
            for extern_fn in &program.extern_fns {
                self.emit_extern_fn_decl(extern_fn)?;
            }
            self.emit_line("");
        }

        // Generate function declarations
        for function in &program.functions {
            self.emit_function_decl(function)?;
        }
        if !program.functions.is_empty() {
            self.emit_line("");
        }

        // Generate function definitions
        for function in &program.functions {
            self.emit_function(function)?;
            self.emit_line("");
        }

        Ok(self.output.clone())
    }

    /// Generate C code from an Embeem module.
    ///
    /// This generates code for a module with the given module path.
    /// The module path is used to mangle function and constant names.
    pub fn generate_module(&mut self, module: &Module, module_path: &[String]) -> Result<String, CodegenError> {
        self.output.clear();
        self.temp_counter = 0;
        
        // Build type context from module items
        self.type_ctx = self.type_ctx_from_module(module);

        // Generate header
        self.emit_header();
        
        // Generate module path comment
        if !module_path.is_empty() {
            self.emit_line(&format!("/* Module: {} */", module_path.join("/")));
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
            self.emit_module_const(constant, module_path, *exported)?;
        }
        if !constants.is_empty() {
            self.emit_line("");
        }

        // Generate external function declarations
        if !extern_fns.is_empty() {
            self.emit_line("/* External function declarations */");
            for (exported, extern_fn) in &extern_fns {
                self.emit_module_extern_fn_decl(extern_fn, module_path, *exported)?;
            }
            self.emit_line("");
        }

        // Generate function declarations
        for (_exported, function) in &functions {
            self.emit_module_function_decl(function, module_path)?;
        }
        if !functions.is_empty() {
            self.emit_line("");
        }

        // Generate function definitions
        for (_exported, function) in &functions {
            self.emit_module_function(function, module_path)?;
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

    /// Emit a constant with module path mangling.
    fn emit_module_const(&mut self, constant: &ConstDecl, module_path: &[String], exported: bool) -> Result<(), CodegenError> {
        let value = self.expr_to_c(&constant.value)?;
        let mangled_name = mangle_module_constant_name(module_path, &constant.name, &self.options.mangle_config);
        
        if exported {
            self.emit_line(&format!("/* exported */ #define {} ({})", mangled_name, value));
        } else {
            self.emit_line(&format!("#define {} ({})", mangled_name, value));
        }
        Ok(())
    }

    /// Emit an external function declaration with module path mangling.
    fn emit_module_extern_fn_decl(&mut self, extern_fn: &ExternFn, module_path: &[String], exported: bool) -> Result<(), CodegenError> {
        let return_type = match &extern_fn.return_type {
            Some(ty) => self.type_to_c(ty),
            None => "void".to_string(),
        };

        let params = extern_fn
            .params
            .iter()
            .map(|p| format!("{} {}", self.type_to_c(&p.ty), p.name))
            .collect::<Vec<_>>()
            .join(", ");

        let params = if params.is_empty() {
            "void".to_string()
        } else {
            params
        };

        let mangled_name = mangle_module_extern_function_name(module_path, &extern_fn.name, &self.options.mangle_config);
        
        if exported {
            self.emit_line(&format!("/* exported */ extern {} {}({});", return_type, mangled_name, params));
        } else {
            self.emit_line(&format!("extern {} {}({});", return_type, mangled_name, params));
        }
        Ok(())
    }

    /// Emit a function declaration with module path mangling.
    fn emit_module_function_decl(&mut self, function: &Function, module_path: &[String]) -> Result<(), CodegenError> {
        let return_type = match &function.return_type {
            Some(ty) => self.type_to_c(ty),
            None => "void".to_string(),
        };

        let params = function
            .params
            .iter()
            .map(|p| format!("{} {}", self.type_to_c(&p.ty), p.name))
            .collect::<Vec<_>>()
            .join(", ");

        let params = if params.is_empty() {
            "void".to_string()
        } else {
            params
        };

        let mangled_name = mangle_module_function_name(module_path, &function.name, &self.options.mangle_config);
        self.emit_line(&format!("{} {}({});", return_type, mangled_name, params));
        Ok(())
    }

    /// Emit a function definition with module path mangling.
    fn emit_module_function(&mut self, function: &Function, module_path: &[String]) -> Result<(), CodegenError> {
        // Save the current type context and add function parameters
        let saved_ctx = self.type_ctx.clone();
        for param in &function.params {
            self.type_ctx.add_variable(param.name.clone(), param.ty.clone());
        }

        let return_type = match &function.return_type {
            Some(ty) => self.type_to_c(ty),
            None => "void".to_string(),
        };

        let params = function
            .params
            .iter()
            .map(|p| format!("{} {}", self.type_to_c(&p.ty), p.name))
            .collect::<Vec<_>>()
            .join(", ");

        let params = if params.is_empty() {
            "void".to_string()
        } else {
            params
        };

        let mangled_name = mangle_module_function_name(module_path, &function.name, &self.options.mangle_config);
        self.emit_line(&format!("{} {}({}) {{", return_type, mangled_name, params));
        self.indent += 1;

        self.emit_block(&function.body, function.return_type.is_some())?;

        self.indent -= 1;
        self.emit_line("}");

        // Restore the type context
        self.type_ctx = saved_ctx;
        Ok(())
    }

    /// Generate C code from a resolved module tree.
    ///
    /// This uses the resolver to flatten all modules into a single program,
    /// then generates C code. The root module's names are not mangled.
    ///
    /// # Arguments
    /// * `fs` - The file system abstraction to use for reading modules
    /// * `parser` - The parser function to use for parsing module source
    /// * `root_path` - The path to the root module
    ///
    /// # Example
    /// ```ignore
    /// use embeem_codegen::c::{CCodegen, CCodegenOptions};
    /// use embeem_ast::resolve::FileSystem;
    ///
    /// struct MyFileSystem { /* ... */ }
    /// impl FileSystem for MyFileSystem { /* ... */ }
    ///
    /// let fs = MyFileSystem::new();
    /// let mut codegen = CCodegen::new();
    /// let c_code = codegen.generate_from_modules(&fs, parse_module, "src/main.em")?;
    /// ```
    #[cfg(feature = "resolve")]
    pub fn generate_from_modules<FS: embeem_ast::resolve::FileSystem>(
        &mut self,
        fs: &FS,
        parser: fn(&str) -> Result<embeem_ast::Module, String>,
        root_path: &str,
    ) -> Result<String, CodegenError> {
        use embeem_ast::resolve::Resolver;
        
        let resolver = Resolver::with_config(fs, parser, self.options.mangle_config.clone());
        let program = resolver.resolve_and_flatten(root_path).map_err(|e| {
            CodegenError::new(&alloc::format!("resolution error: {}", e))
        })?;
        
        self.generate(&program)
    }

    fn emit_header(&mut self) {
        self.emit_line("/* Generated by Embeem compiler */");
        if self.options.use_gcc_statement_exprs {
            self.emit_line("/* Using GCC statement expressions */");
        }
        self.emit_line("#include <stdint.h>");
        self.emit_line("#include <stdbool.h>");
        self.emit_line("");

        self.emit_line("/* Operation Mangling Scheme */");
        self.emit_line("/* Operations use length-prefixed encoding: <prefix>$<n>_<len>_<SEG>... */");
        self.emit_line("/* Non-hybrid operations use op_prefix (default: embeem_op_): */");
        self.emit_line("/*   FSUB(a, b)           -> embeem_op_$1_4_FSUB(a, b) */");
        self.emit_line("/*   WRITE(GPIO(pin), v)  -> embeem_op_$2_5_WRITE_4_GPIO(pin, v) */");
        self.emit_line("/* External functions and hybrid operations use extern_prefix (default: embeem_extern_): */");
        self.emit_line("/*   sensor_read(0)       -> embeem_extern_sensor_read(0) */");
        self.emit_line("/*   WRITE(GPIO(fn(x)))   -> embeem_extern_$2_5_WRITE_4_GPIO_fn(x) */");
        self.emit_line("");
        self.emit_line("/* Note: Basic operations (ADD, SUB, MUL, etc.) are inlined as C operators. */");
        self.emit_line("/* Other operations must be provided by the platform. */");
        self.emit_line("");
    }

    fn emit_const(&mut self, constant: &ConstDecl) -> Result<(), CodegenError> {
        let _c_type = self.type_to_c(&constant.ty);
        let value = self.expr_to_c(&constant.value)?;
        self.emit_line(&format!("#define {} ({})", constant.name, value));
        Ok(())
    }

    /// Emit an external function declaration.
    ///
    /// External functions are declared as `extern` in C, meaning they must be
    /// provided by the environment (linked from another object file or library).
    /// The function name is mangled with extern_prefix.
    fn emit_extern_fn_decl(&mut self, extern_fn: &ExternFn) -> Result<(), CodegenError> {
        let return_type = match &extern_fn.return_type {
            Some(ty) => self.type_to_c(ty),
            None => "void".to_string(),
        };

        let params = extern_fn
            .params
            .iter()
            .map(|p| format!("{} {}", self.type_to_c(&p.ty), p.name))
            .collect::<Vec<_>>()
            .join(", ");

        let params = if params.is_empty() {
            "void".to_string()
        } else {
            params
        };

        // External functions are mangled with extern_prefix
        let mangled_name = mangle_extern_function_name(&extern_fn.name, &self.options.mangle_config);
        self.emit_line(&format!("extern {} {}({});", return_type, mangled_name, params));
        Ok(())
    }

    fn emit_function_decl(&mut self, function: &Function) -> Result<(), CodegenError> {
        let return_type = match &function.return_type {
            Some(ty) => self.type_to_c(ty),
            None => "void".to_string(),
        };

        let params = function
            .params
            .iter()
            .map(|p| format!("{} {}", self.type_to_c(&p.ty), p.name))
            .collect::<Vec<_>>()
            .join(", ");

        let params = if params.is_empty() {
            "void".to_string()
        } else {
            params
        };

        let mangled_name = mangle_function_name(&function.name, &self.options.mangle_config);
        self.emit_line(&format!("{} {}({});", return_type, mangled_name, params));
        Ok(())
    }

    fn emit_function(&mut self, function: &Function) -> Result<(), CodegenError> {
        // Save the current type context and add function parameters
        let saved_ctx = self.type_ctx.clone();
        for param in &function.params {
            self.type_ctx.add_variable(param.name.clone(), param.ty.clone());
        }

        let return_type = match &function.return_type {
            Some(ty) => self.type_to_c(ty),
            None => "void".to_string(),
        };

        let params = function
            .params
            .iter()
            .map(|p| format!("{} {}", self.type_to_c(&p.ty), p.name))
            .collect::<Vec<_>>()
            .join(", ");

        let params = if params.is_empty() {
            "void".to_string()
        } else {
            params
        };

        let mangled_name = mangle_function_name(&function.name, &self.options.mangle_config);
        self.emit_line(&format!("{} {}({}) {{", return_type, mangled_name, params));
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
                let expr = self.expr_to_c(result)?;
                self.emit_line(&format!("return {};", expr));
            } else {
                let expr = self.expr_to_c(result)?;
                self.emit_line(&format!("{};", expr));
            }
        }

        Ok(())
    }

    fn emit_statement(&mut self, stmt: &Statement) -> Result<(), CodegenError> {
        match stmt {
            Statement::Let {
                name,
                mutable: _,
                ty,
                value,
            } => {
                // Use explicit type if provided, otherwise infer the type
                let inferred_ty = ty.as_ref().cloned().or_else(|| infer_expression_type(value, &self.type_ctx));
                let c_type = inferred_ty
                    .as_ref()
                    .map(|t| self.type_to_c(t))
                    .unwrap_or_else(|| "uint64_t".to_string());
                
                // Add the variable to the type context for future inference
                if let Some(var_ty) = inferred_ty {
                    self.type_ctx.add_variable(name.clone(), var_ty);
                }
                
                let expr = self.expr_to_c(value)?;
                self.emit_line(&format!("{} {} = {};", c_type, name, expr));
            }

            Statement::Assign { target, value } => {
                let expr = self.expr_to_c(value)?;
                self.emit_line(&format!("{} = {};", target, expr));
            }

            Statement::Expr(expr) => {
                let c_expr = self.expr_to_c(expr)?;
                self.emit_line(&format!("{};", c_expr));
            }

            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond = self.expr_to_c(condition)?;
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
                let start_expr = self.expr_to_c(start)?;
                let end_expr = self.expr_to_c(end)?;

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
                    "for (int64_t {} = {}; {}; {}) {{",
                    variable, start_expr, cond, update
                ));
                self.indent += 1;
                self.emit_block(body, false)?;
                self.indent -= 1;
                self.emit_line("}");
            }

            Statement::Repeat { count, body } => {
                let count_expr = self.expr_to_c(count)?;
                let loop_var = self.fresh_temp();
                self.emit_line(&format!(
                    "for (uint64_t {} = 0; {} < {}; {}++) {{",
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
                let cond = self.expr_to_c(condition)?;
                let max = self.expr_to_c(max_iterations)?;
                let counter = self.fresh_temp();
                self.emit_line(&format!("uint64_t {} = 0;", counter));
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

    fn expr_to_c(&mut self, expr: &Expression) -> Result<String, CodegenError> {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::Integer(n) => Ok(format!("{}ULL", n)),
                Literal::Float(f) => Ok(format!("{}", f)),
                Literal::Bool(b) => Ok(if *b { "1".to_string() } else { "0".to_string() }),
            },

            Expression::Identifier(name) => Ok(name.clone()),

            Expression::QualifiedIdentifier { namespace, name } => {
                // For qualified identifiers (namespace::name), we need to resolve
                // the full module path. For now, we emit a simple concatenation
                // that assumes the namespace has been resolved to a module path.
                // In a full implementation, this would look up the import table.
                Ok(format!("{}_{}", namespace, name))
            }

            Expression::Binary { op, left, right } => {
                let l = self.expr_to_c(left)?;
                let r = self.expr_to_c(right)?;
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
                    BinaryOp::LogicalShr => ">>",
                    BinaryOp::Eq => "==",
                    BinaryOp::Ne => "!=",
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
                let o = self.expr_to_c(operand)?;
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
                let cond = self.expr_to_c(condition)?;
                // For simple expressions, use ternary operator
                if then_branch.statements.is_empty() && else_branch.statements.is_empty() {
                    if let (Some(then_expr), Some(else_expr)) =
                        (&then_branch.result, &else_branch.result)
                    {
                        let t = self.expr_to_c(then_expr)?;
                        let e = self.expr_to_c(else_expr)?;
                        return Ok(format!("({} ? {} : {})", cond, t, e));
                    }
                }
                // For complex blocks, use GCC statement expressions if enabled
                if self.options.use_gcc_statement_exprs {
                    self.emit_if_expr_gcc(condition, then_branch, else_branch)
                } else {
                    Err(CodegenError::new(
                        "complex if expressions not supported in expression context (enable GCC extensions)",
                    ))
                }
            }

            Expression::Operation { path, extern_fn, args } => {
                self.emit_operation_path(path, extern_fn.as_deref(), args)
            }

            Expression::Call { function, args } => {
                let arg_strs: Result<Vec<_>, _> =
                    args.iter().map(|a| self.expr_to_c(a)).collect();
                let arg_strs = arg_strs?;
                // External functions use extern_prefix
                // Regular functions get the mangle prefix
                let name = if self.type_ctx.is_extern_fn(function) {
                    mangle_extern_function_name(function, &self.options.mangle_config)
                } else {
                    mangle_function_name(function, &self.options.mangle_config)
                };
                Ok(format!("{}({})", name, arg_strs.join(", ")))
            }

            Expression::QualifiedCall { namespace, function, args } => {
                let arg_strs: Result<Vec<_>, _> =
                    args.iter().map(|a| self.expr_to_c(a)).collect();
                let arg_strs = arg_strs?;
                // For qualified calls (namespace::function()), we need to resolve
                // the full module path. For now, we emit a simple concatenation
                // that assumes the namespace has been resolved to a module path.
                // In a full implementation, this would look up the import table.
                Ok(format!("{}_{}({})", namespace, function, arg_strs.join(", ")))
            }

            Expression::Block(blk) => {
                if self.options.use_gcc_statement_exprs {
                    self.emit_block_expr_gcc(blk)
                } else {
                    Err(CodegenError::new("block expressions not supported in expression context (enable GCC extensions)"))
                }
            }

            Expression::Index { array, index } => {
                let arr = self.expr_to_c(array)?;
                let idx = self.expr_to_c(index)?;
                Ok(format!("{}[{}]", arr, idx))
            }

            Expression::Array(elements) => {
                let elem_strs: Result<Vec<_>, _> =
                    elements.iter().map(|e| self.expr_to_c(e)).collect();
                let elem_strs = elem_strs?;
                Ok(format!("{{{}}}", elem_strs.join(", ")))
            }

            Expression::Cast { value, ty } => {
                let v = self.expr_to_c(value)?;
                let c_type = self.type_to_c(ty);
                Ok(format!("(({}){})", c_type, v))
            }
        }
    }

    /// Emit an operation call with a path and optional extern function.
    ///
    /// Some common single-segment operations (like ADD, SUB, etc.) are optimized
    /// to emit inline C operators for efficiency. All other operations use the
    /// mangled function name.
    ///
    /// For hybrid operations (where extern_fn is Some), the extern function name
    /// is appended to the mangled name.
    ///
    /// Example: `WRITE(GPIO(sensor_read(pin)))` where `sensor_read` is an extern fn
    /// generates: `$2_5_WRITE_4_GPIO_sensor_read(pin)`
    fn emit_operation_path(&mut self, path: &[String], extern_fn: Option<&str>, args: &[Expression]) -> Result<String, CodegenError> {
        let arg_strs: Result<Vec<_>, _> = args.iter().map(|a| self.expr_to_c(a)).collect();
        let arg_strs = arg_strs?;

        // Hybrid operations cannot use inline C operators - they call the extern fn
        // as part of a combined operation
        if extern_fn.is_none() {
            // For single-segment operations, check if we can use inline C operators
            if path.len() == 1 {
                let op = path[0].as_str();
                match (op, arg_strs.len()) {
                    ("ADD", 2) => return Ok(format!("({} + {})", arg_strs[0], arg_strs[1])),
                    ("SUB", 2) => return Ok(format!("({} - {})", arg_strs[0], arg_strs[1])),
                    ("MUL", 2) => return Ok(format!("({} * {})", arg_strs[0], arg_strs[1])),
                    ("DIV", 2) => return Ok(format!("({1} != 0 ? ({0}) / ({1}) : 0)", arg_strs[0], arg_strs[1])),
                    ("MOD", 2) => return Ok(format!("({1} != 0 ? ({0}) % ({1}) : 0)", arg_strs[0], arg_strs[1])),
                    ("INC", 1) => return Ok(format!("({} + 1)", arg_strs[0])),
                    ("DEC", 1) => return Ok(format!("({} - 1)", arg_strs[0])),
                    ("NEG", 1) => return Ok(format!("(-{})", arg_strs[0])),
                    ("ABS", 1) => return Ok(format!("(({0}) < 0 ? -({0}) : ({0}))", arg_strs[0])),
                    ("AND", 2) => return Ok(format!("({} & {})", arg_strs[0], arg_strs[1])),
                    ("OR", 2) => return Ok(format!("({} | {})", arg_strs[0], arg_strs[1])),
                    ("XOR", 2) => return Ok(format!("({} ^ {})", arg_strs[0], arg_strs[1])),
                    ("NOT", 1) => return Ok(format!("(~{})", arg_strs[0])),
                    ("SHL", 2) => return Ok(format!("({} << {})", arg_strs[0], arg_strs[1])),
                    ("SHR", 2) => return Ok(format!("({} >> {})", arg_strs[0], arg_strs[1])),
                    ("EQ", 2) => return Ok(format!("({} == {})", arg_strs[0], arg_strs[1])),
                    ("NE", 2) => return Ok(format!("({} != {})", arg_strs[0], arg_strs[1])),
                    ("LT", 2) => return Ok(format!("({} < {})", arg_strs[0], arg_strs[1])),
                    ("LE", 2) => return Ok(format!("({} <= {})", arg_strs[0], arg_strs[1])),
                    ("GT", 2) => return Ok(format!("({} > {})", arg_strs[0], arg_strs[1])),
                    ("GE", 2) => return Ok(format!("({} >= {})", arg_strs[0], arg_strs[1])),
                    ("NOP", 0) => return Ok("((void)0)".to_string()),
                    _ => {}
                }
            }
        }

        // For all operations (including hybrid), use the mangled function name
        let op_name = mangle_operation_path(path, extern_fn, &self.options.mangle_config);
        Ok(format!("{}({})", op_name, arg_strs.join(", ")))
    }

    /// Emit a block expression using GCC statement expression syntax: `({ ... })`
    fn emit_block_expr_gcc(&mut self, block: &Block) -> Result<String, CodegenError> {
        let mut result = String::from("({ ");
        
        // Emit statements inline
        for stmt in &block.statements {
            let stmt_str = self.emit_statement_to_string(stmt)?;
            result.push_str(&stmt_str);
            result.push(' ');
        }
        
        // Emit result expression if present
        if let Some(ref expr) = block.result {
            let expr_str = self.expr_to_c(expr)?;
            result.push_str(&expr_str);
            result.push_str("; ");
        }
        
        result.push_str("})");
        Ok(result)
    }

    /// Emit an if expression using GCC statement expression syntax.
    fn emit_if_expr_gcc(
        &mut self,
        condition: &Expression,
        then_branch: &Block,
        else_branch: &Block,
    ) -> Result<String, CodegenError> {
        let cond = self.expr_to_c(condition)?;
        let result_var = self.fresh_temp();
        
        let mut result = format!("({{ uint64_t {}; if ({}) {{ ", result_var, cond);
        
        // Then branch
        for stmt in &then_branch.statements {
            let stmt_str = self.emit_statement_to_string(stmt)?;
            result.push_str(&stmt_str);
            result.push(' ');
        }
        if let Some(ref expr) = then_branch.result {
            let expr_str = self.expr_to_c(expr)?;
            result.push_str(&format!("{} = {}; ", result_var, expr_str));
        }
        
        result.push_str("} else { ");
        
        // Else branch
        for stmt in &else_branch.statements {
            let stmt_str = self.emit_statement_to_string(stmt)?;
            result.push_str(&stmt_str);
            result.push(' ');
        }
        if let Some(ref expr) = else_branch.result {
            let expr_str = self.expr_to_c(expr)?;
            result.push_str(&format!("{} = {}; ", result_var, expr_str));
        }
        
        result.push_str(&format!("}} {}; }})", result_var));
        Ok(result)
    }

    /// Emit a statement to a string (for use in GCC statement expressions).
    fn emit_statement_to_string(&mut self, stmt: &Statement) -> Result<String, CodegenError> {
        match stmt {
            Statement::Let {
                name,
                mutable: _,
                ty,
                value,
            } => {
                let c_type = ty
                    .as_ref()
                    .map(|t| self.type_to_c(t))
                    .unwrap_or_else(|| "uint64_t".to_string());
                let expr = self.expr_to_c(value)?;
                Ok(format!("{} {} = {};", c_type, name, expr))
            }

            Statement::Assign { target, value } => {
                let expr = self.expr_to_c(value)?;
                Ok(format!("{} = {};", target, expr))
            }

            Statement::Expr(expr) => {
                let c_expr = self.expr_to_c(expr)?;
                Ok(format!("{};", c_expr))
            }

            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond = self.expr_to_c(condition)?;
                let mut result = format!("if ({}) {{ ", cond);
                
                for s in &then_block.statements {
                    let s_str = self.emit_statement_to_string(s)?;
                    result.push_str(&s_str);
                    result.push(' ');
                }
                if let Some(ref expr) = then_block.result {
                    let expr_str = self.expr_to_c(expr)?;
                    result.push_str(&expr_str);
                    result.push_str("; ");
                }
                
                match else_block {
                    Some(ElseBlock::Block(blk)) => {
                        result.push_str("} else { ");
                        for s in &blk.statements {
                            let s_str = self.emit_statement_to_string(s)?;
                            result.push_str(&s_str);
                            result.push(' ');
                        }
                        if let Some(ref expr) = blk.result {
                            let expr_str = self.expr_to_c(expr)?;
                            result.push_str(&expr_str);
                            result.push_str("; ");
                        }
                        result.push('}');
                    }
                    Some(ElseBlock::ElseIf(stmt)) => {
                        result.push_str("} else ");
                        let stmt_str = self.emit_statement_to_string(stmt)?;
                        result.push_str(&stmt_str);
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
                let start_expr = self.expr_to_c(start)?;
                let end_expr = self.expr_to_c(end)?;

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
                    "for (int64_t {} = {}; {}; {}) {{ ",
                    variable, start_expr, cond, update
                );
                
                for s in &body.statements {
                    let s_str = self.emit_statement_to_string(s)?;
                    result.push_str(&s_str);
                    result.push(' ');
                }
                if let Some(ref expr) = body.result {
                    let expr_str = self.expr_to_c(expr)?;
                    result.push_str(&expr_str);
                    result.push_str("; ");
                }
                
                result.push('}');
                Ok(result)
            }

            Statement::Repeat { count, body } => {
                let count_expr = self.expr_to_c(count)?;
                let loop_var = self.fresh_temp();
                
                let mut result = format!(
                    "for (uint64_t {} = 0; {} < {}; {}++) {{ ",
                    loop_var, loop_var, count_expr, loop_var
                );
                
                for s in &body.statements {
                    let s_str = self.emit_statement_to_string(s)?;
                    result.push_str(&s_str);
                    result.push(' ');
                }
                if let Some(ref expr) = body.result {
                    let expr_str = self.expr_to_c(expr)?;
                    result.push_str(&expr_str);
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
                let cond = self.expr_to_c(condition)?;
                let max = self.expr_to_c(max_iterations)?;
                let counter = self.fresh_temp();
                
                let mut result = format!("uint64_t {} = 0; ", counter);
                result.push_str(&format!(
                    "while (({}) && ({} < {})) {{ ",
                    cond, counter, max
                ));
                
                for s in &body.statements {
                    let s_str = self.emit_statement_to_string(s)?;
                    result.push_str(&s_str);
                    result.push(' ');
                }
                if let Some(ref expr) = body.result {
                    let expr_str = self.expr_to_c(expr)?;
                    result.push_str(&expr_str);
                    result.push_str("; ");
                }
                
                result.push_str(&format!("{}++; }}", counter));
                Ok(result)
            }
        }
    }

    fn type_to_c(&self, ty: &Type) -> String {
        match ty {
            Type::Primitive(prim) => match prim {
                PrimitiveType::U8 => "uint8_t".to_string(),
                PrimitiveType::U16 => "uint16_t".to_string(),
                PrimitiveType::U32 => "uint32_t".to_string(),
                PrimitiveType::U64 => "uint64_t".to_string(),
                PrimitiveType::I8 => "int8_t".to_string(),
                PrimitiveType::I16 => "int16_t".to_string(),
                PrimitiveType::I32 => "int32_t".to_string(),
                PrimitiveType::I64 => "int64_t".to_string(),
                PrimitiveType::F32 => "float".to_string(),
                PrimitiveType::F64 => "double".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
            },
            Type::Array(elem, size) => {
                format!("{}[{}]", self.type_to_c(elem), size)
            }
            Type::Tuple(types) => {
                // C doesn't have tuples, use a struct
                format!("struct {{ {} }}", 
                    types.iter().enumerate()
                        .map(|(i, t)| format!("{} _{}", self.type_to_c(t), i))
                        .collect::<Vec<_>>()
                        .join("; "))
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

impl Default for CCodegen {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to compile an Embeem program to C (standard C, no extensions).
pub fn compile_to_c(program: &Program) -> Result<String, CodegenError> {
    let mut codegen = CCodegen::new();
    codegen.generate(program)
}

/// Convenience function to compile an Embeem program to C with specified options.
pub fn compile_to_c_with_options(program: &Program, options: CCodegenOptions) -> Result<String, CodegenError> {
    let mut codegen = CCodegen::with_options(options);
    codegen.generate(program)
}

/// Convenience function to compile an Embeem program to C using GCC extensions.
/// This enables GCC statement expressions `({ ... })` for block and complex if expressions.
pub fn compile_to_c_gcc(program: &Program) -> Result<String, CodegenError> {
    let options = CCodegenOptions::new().with_gcc_extensions();
    let mut codegen = CCodegen::with_options(options);
    codegen.generate(program)
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use embeem_parser::parse_program;

    #[test]
    fn test_simple_program() {
        let src = r#"
            fn main() {
                GPIO_SET_MODE(13, 1);
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        // Function names are mangled with prefix
        assert!(c_code.contains("void embeem_main(void)"));
        // GPIO operations use length-prefixed mangling: embeem_op_$1_13_GPIO_SET_MODE
        assert!(c_code.contains("embeem_op_$1_13_GPIO_SET_MODE"), "Expected length-prefixed mangling, got:\n{}", c_code);
    }

    #[test]
    fn test_constants() {
        let src = r#"
            const LED_PIN: u8 = 13;
            fn main() {}
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        assert!(c_code.contains("#define LED_PIN"));
    }

    #[test]
    fn test_repeat_loop() {
        let src = r#"
            fn main() {
                repeat 10 {
                    DELAY_MS(100);
                }
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        assert!(c_code.contains("for"));
        assert!(c_code.contains("< 10"));
    }

    #[test]
    fn test_repeat_variable_bound() {
        // Totality is preserved because iterations is an immutable parameter
        let src = r#"
            fn blink_n_times(iterations: u32) {
                repeat iterations {
                    GPIO_TOGGLE(13);
                    DELAY_MS(500);
                }
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        // Check that the variable is used in the loop bound
        assert!(c_code.contains("< iterations"), "Expected variable bound in for loop, got:\n{}", c_code);
    }

    #[test]
    fn test_while_variable_max() {
        // Totality is preserved because max_attempts is an immutable parameter
        let src = r#"
            fn wait_for_input(max_attempts: u32) -> bool {
                let mut count: u32 = 0;
                while UART_AVAILABLE(0) == 0 max max_attempts {
                    count = count + 1;
                    DELAY_MS(10);
                }
                count < max_attempts
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        // Check that the variable is used in the max iteration check
        assert!(c_code.contains("max_attempts"), "Expected variable max in while loop, got:\n{}", c_code);
    }

    #[test]
    fn test_for_loop() {
        let src = r#"
            fn main() {
                for i in 0 to 9 {
                    GPIO_TOGGLE(i);
                }
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        assert!(c_code.contains("for"));
        assert!(c_code.contains("i++"));
    }

    #[test]
    fn test_if_statement() {
        let src = r#"
            fn main() {
                let x: u32 = 42;
                if x > 0 {
                    GPIO_WRITE(13, 1);
                } else {
                    GPIO_WRITE(13, 0);
                }
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        assert!(c_code.contains("if"));
        assert!(c_code.contains("else"));
    }

    #[test]
    fn test_gcc_statement_expressions_header() {
        let src = r#"
            fn main() {}
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c_gcc(&program).unwrap();
        assert!(c_code.contains("Using GCC statement expressions"));
    }

    #[test]
    fn test_gcc_block_expression() {
        let src = r#"
            fn get_value() -> u32 {
                let result = {
                    let x: u32 = 10;
                    x + 5
                };
                result
            }
        "#;
        let program = parse_program(src).unwrap();
        // Should fail without GCC extensions
        let result = compile_to_c(&program);
        assert!(result.is_err());
        
        // Should succeed with GCC extensions
        let c_code = compile_to_c_gcc(&program).unwrap();
        assert!(c_code.contains("({"));
        assert!(c_code.contains("})"));
    }

    #[test]
    fn test_gcc_complex_if_expression() {
        let src = r#"
            fn get_max(a: u32, b: u32) -> u32 {
                let result = if a > b {
                    let temp: u32 = a;
                    temp
                } else {
                    let temp: u32 = b;
                    temp
                };
                result
            }
        "#;
        let program = parse_program(src).unwrap();
        // Should fail without GCC extensions
        let result = compile_to_c(&program);
        assert!(result.is_err());
        
        // Should succeed with GCC extensions
        let c_code = compile_to_c_gcc(&program).unwrap();
        assert!(c_code.contains("({"));
    }

    #[test]
    fn test_simple_ternary_still_works() {
        // Simple if expressions should use ternary even without GCC extensions
        let src = r#"
            fn max_val(a: u32, b: u32) -> u32 {
                if a > b { a } else { b }
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        assert!(c_code.contains("?"), "Expected ternary operator '?', got:\n{}", c_code);
        assert!(c_code.contains(":"));
    }

    #[test]
    fn test_type_inference_for_let() {
        // Test that type inference works for let statements without explicit types
        let src = r#"
            fn main() {
                let x = 42;
                let y = true;
                let z = 3.14;
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        // x should be inferred as uint64_t (integer literal)
        assert!(c_code.contains("uint64_t x = "), "Expected uint64_t for integer literal, got:\n{}", c_code);
        // y should be inferred as bool
        assert!(c_code.contains("bool y = "), "Expected bool for boolean literal, got:\n{}", c_code);
        // z should be inferred as double (float literal)
        assert!(c_code.contains("double z = "), "Expected double for float literal, got:\n{}", c_code);
    }

    #[test]
    fn test_type_inference_comparison() {
        // Test that comparison results are inferred as bool
        let src = r#"
            fn test() {
                let x: u32 = 10;
                let y: u32 = 5;
                let is_greater = x > y;
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        // is_greater should be inferred as bool
        assert!(c_code.contains("bool is_greater = "), "Expected bool for comparison result, got:\n{}", c_code);
    }

    #[test]
    fn test_type_inference_from_variable() {
        // Test that a variable's type is inferred from another variable
        let src = r#"
            fn test() {
                let x: u16 = 100;
                let y = x;
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        // y should be inferred as uint16_t from x
        assert!(c_code.contains("uint16_t y = "), "Expected uint16_t for variable reference, got:\n{}", c_code);
    }

    #[test]
    fn test_extern_fn() {
        let src = r#"
            extern fn get_sensor_value(channel: u8) -> i32;
            extern fn set_led(pin: u8, value: bool);
            extern fn init_system();
            
            fn main() {
                init_system();
                let val = get_sensor_value(0);
                set_led(13, val > 100);
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        
        // Check external function declarations are generated with extern_prefix
        assert!(c_code.contains("extern int32_t embeem_extern_get_sensor_value(uint8_t channel);"), 
            "Expected extern declaration, got:\n{}", c_code);
        assert!(c_code.contains("extern void embeem_extern_set_led(uint8_t pin, bool value);"), 
            "Expected extern declaration, got:\n{}", c_code);
        assert!(c_code.contains("extern void embeem_extern_init_system(void);"), 
            "Expected extern declaration, got:\n{}", c_code);
        
        // Check that external functions are called with extern_prefix
        assert!(c_code.contains("embeem_extern_init_system();"), 
            "Expected call to embeem_extern_init_system, got:\n{}", c_code);
        assert!(c_code.contains("embeem_extern_get_sensor_value("), 
            "Expected call to embeem_extern_get_sensor_value, got:\n{}", c_code);
        assert!(c_code.contains("embeem_extern_set_led("), 
            "Expected call to embeem_extern_set_led, got:\n{}", c_code);
    }

    #[test]
    fn test_hybrid_operation_mangling() {
        let src = r#"
            extern fn sensor_read(channel: u8) -> u16;
            
            fn main() {
                // Hybrid operation: extern fn as last segment of operation path
                WRITE(GPIO(sensor_read(0)));
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        
        // Hybrid operations use extern_prefix with length-prefixed mangling and extern fn appended
        // WRITE(GPIO(sensor_read(0))) -> embeem_extern_$2_5_WRITE_4_GPIO_sensor_read(0)
        assert!(c_code.contains("embeem_extern_$2_5_WRITE_4_GPIO_sensor_read("), 
            "Expected hybrid mangled name embeem_extern_$2_5_WRITE_4_GPIO_sensor_read, got:\n{}", c_code);
    }

    #[test]
    fn test_operation_mangling_examples() {
        // Test various mangling examples
        let src = r#"
            fn main() {
                FSUB(1.0, 2.0);
                READ(ADC(0));
            }
        "#;
        let program = parse_program(src).unwrap();
        let c_code = compile_to_c(&program).unwrap();
        
        // FSUB -> embeem_op_$1_4_FSUB
        assert!(c_code.contains("embeem_op_$1_4_FSUB("), "Expected embeem_op_$1_4_FSUB, got:\n{}", c_code);
        // READ(ADC(...)) -> embeem_op_$2_4_READ_3_ADC
        assert!(c_code.contains("embeem_op_$2_4_READ_3_ADC("), "Expected embeem_op_$2_4_READ_3_ADC, got:\n{}", c_code);
    }

    #[test]
    fn test_module_codegen() {
        use embeem_parser::parse_module;
        
        let src = r#"
            export const LED_PIN: u8 = 13;
            
            export fn blink(times: u32) {
                for i in 0 to times {
                    GPIO_TOGGLE(LED_PIN);
                }
            }
            
            fn helper() {
                // Not exported
            }
        "#;
        let module = parse_module(src).unwrap();
        let mut codegen = CCodegen::new();
        let c_code = codegen.generate_module(&module, &["gpio".to_string()]).unwrap();
        
        // Check module path in comment
        assert!(c_code.contains("/* Module: gpio */"), "Expected module comment, got:\n{}", c_code);
        
        // Check that constants are mangled with module path
        assert!(c_code.contains("embeem_mod_$1_4_gpio_LED_PIN"), 
            "Expected module-mangled constant, got:\n{}", c_code);
        
        // Check that functions are mangled with module path
        assert!(c_code.contains("embeem_mod_$1_4_gpio_blink"), 
            "Expected module-mangled function, got:\n{}", c_code);
        
        // Check that exported items have a comment
        assert!(c_code.contains("/* exported */"), 
            "Expected exported comment, got:\n{}", c_code);
    }

    #[test]
    fn test_module_codegen_nested_path() {
        use embeem_parser::parse_module;
        
        let src = r#"
            export fn init() {
                // Initialize sensor
            }
        "#;
        let module = parse_module(src).unwrap();
        let mut codegen = CCodegen::new();
        let c_code = codegen.generate_module(
            &module, 
            &["drivers".to_string(), "bme280".to_string()]
        ).unwrap();
        
        // Check that functions use the full module path in mangling
        assert!(c_code.contains("embeem_mod_$2_7_drivers_6_bme280_init"), 
            "Expected nested module-mangled function, got:\n{}", c_code);
    }
}
