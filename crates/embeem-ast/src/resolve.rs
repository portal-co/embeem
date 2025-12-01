//! Module resolution and flattening for Embeem.
//!
//! This module provides the infrastructure for resolving module imports and
//! flattening a collection of modules into a single `Program`.
//!
//! # Overview
//!
//! The resolver works in three phases:
//! 1. **Loading**: Read module source files using a `FileSystem` trait
//! 2. **Resolution**: Build a directed acyclic graph (DAG) of module dependencies
//! 3. **Flattening**: Combine all modules into a single `Program` with mangled names
//!
//! # Example
//!
//! ```ignore
//! use embeem_ast::resolve::{Resolver, FileSystem};
//!
//! struct MyFileSystem { /* ... */ }
//! impl FileSystem for MyFileSystem { /* ... */ }
//!
//! let fs = MyFileSystem::new();
//! let resolver = Resolver::new(&fs);
//! let program = resolver.resolve_and_flatten("src/main.em")?;
//! ```

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::collections::BTreeSet;
use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;

use crate::ast::{
    Block, ConstDecl, ElseBlock, Expression, ExternFn, Function, Import, Item, Module, 
    ModulePath, Program, Statement,
};
use crate::mangle::{mangle_module_constant_name, mangle_module_extern_function_name, mangle_module_function_name, MangleConfig};

/// Error types for module resolution.
#[derive(Clone, Debug, PartialEq)]
pub enum ResolveError {
    /// Failed to read a module file.
    FileNotFound { path: String },
    /// Failed to parse a module.
    ParseError { path: String, message: String },
    /// Circular dependency detected.
    CyclicDependency { cycle: Vec<String> },
    /// Import refers to a non-existent module.
    ModuleNotFound { from: String, path: String },
    /// Import refers to a non-existent item.
    ItemNotFound { module: String, item: String },
    /// Re-export cycle detected.
    ReExportCycle { path: String },
    /// Other resolution error.
    Other { message: String },
}

impl core::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ResolveError::FileNotFound { path } => write!(f, "file not found: {}", path),
            ResolveError::ParseError { path, message } => {
                write!(f, "parse error in {}: {}", path, message)
            }
            ResolveError::CyclicDependency { cycle } => {
                write!(f, "cyclic dependency: {}", cycle.join(" -> "))
            }
            ResolveError::ModuleNotFound { from, path } => {
                write!(f, "module '{}' not found (imported from '{}')", path, from)
            }
            ResolveError::ItemNotFound { module, item } => {
                write!(f, "item '{}' not found in module '{}'", item, module)
            }
            ResolveError::ReExportCycle { path } => {
                write!(f, "re-export cycle in module '{}'", path)
            }
            ResolveError::Other { message } => write!(f, "{}", message),
        }
    }
}

/// Trait for abstracting file system operations.
///
/// Implement this trait to provide module source files to the resolver.
/// This abstraction allows testing without real file I/O and enables
/// custom module loading strategies.
pub trait FileSystem {
    /// Read the contents of a file.
    ///
    /// Returns `Some(content)` if the file exists and is readable,
    /// `None` otherwise.
    fn read_file(&self, path: &str) -> Option<String>;

    /// Check if a file exists.
    fn exists(&self, path: &str) -> bool;

    /// Normalize a path (resolve `.` and `..`, make absolute).
    ///
    /// The default implementation handles basic path normalization.
    fn normalize_path(&self, base: &str, relative: &ModulePath) -> String {
        // Get the directory of the base file
        let base_dir = if let Some(pos) = base.rfind('/') {
            &base[..pos]
        } else {
            "" // File is in current directory
        };

        let mut segments: Vec<&str> = if relative.is_relative {
            base_dir.split('/').filter(|s| !s.is_empty() && *s != ".").collect()
        } else {
            Vec::new()
        };

        // Handle parent directory references
        for _ in 0..relative.parent_count {
            segments.pop();
        }

        // Add the module path segments
        for seg in &relative.segments {
            if seg == "." {
                continue;
            } else if seg == ".." {
                segments.pop();
            } else {
                segments.push(seg);
            }
        }

        // Join with slashes and add .em extension if needed
        let mut path = segments.join("/");
        if !path.ends_with(".em") {
            path.push_str(".em");
        }
        path
    }
}

/// A resolved module with its path and exports.
#[derive(Clone, Debug)]
pub struct ResolvedModule {
    /// The normalized path to this module.
    pub path: String,
    /// The parsed module AST.
    pub module: Module,
    /// Module path segments (derived from file path).
    pub module_path: Vec<String>,
    /// Exported item names mapped to their items.
    pub exports: BTreeMap<String, ExportedItem>,
    /// Namespace imports (alias -> module path).
    pub namespace_imports: BTreeMap<String, String>,
    /// Named imports (local name -> (source module, original name)).
    pub named_imports: BTreeMap<String, (String, String)>,
}

/// An exported item from a module.
#[derive(Clone, Debug)]
pub enum ExportedItem {
    /// An exported constant.
    Const(ConstDecl),
    /// An exported extern function.
    ExternFn(ExternFn),
    /// An exported function.
    Function(Function),
}

/// The module resolver.
///
/// Resolves and flattens a module tree into a single `Program`.
pub struct Resolver<'a, FS: FileSystem> {
    /// The file system abstraction.
    fs: &'a FS,
    /// Mangle configuration.
    config: MangleConfig,
    /// Parser function (injected for flexibility).
    parser: fn(&str) -> Result<Module, String>,
}

impl<'a, FS: FileSystem> Resolver<'a, FS> {
    /// Create a new resolver with the given file system and parser.
    pub fn new(fs: &'a FS, parser: fn(&str) -> Result<Module, String>) -> Self {
        Self {
            fs,
            config: MangleConfig::default(),
            parser,
        }
    }

    /// Create a new resolver with custom mangle configuration.
    pub fn with_config(
        fs: &'a FS,
        parser: fn(&str) -> Result<Module, String>,
        config: MangleConfig,
    ) -> Self {
        Self { fs, config, parser }
    }

    /// Resolve a module tree starting from the root module.
    ///
    /// This loads all modules reachable from the root and checks for cycles.
    pub fn resolve(&self, root_path: &str) -> Result<BTreeMap<String, ResolvedModule>, ResolveError> {
        let mut modules: BTreeMap<String, ResolvedModule> = BTreeMap::new();
        let mut pending: Vec<String> = vec![root_path.to_string()];
        let mut in_progress: BTreeSet<String> = BTreeSet::new();

        while let Some(path) = pending.pop() {
            if modules.contains_key(&path) {
                continue;
            }

            if in_progress.contains(&path) {
                // Build cycle path for error message
                let cycle: Vec<String> = in_progress.iter().cloned().collect();
                return Err(ResolveError::CyclicDependency { cycle });
            }

            // Read and parse the module
            let source = self
                .fs
                .read_file(&path)
                .ok_or_else(|| ResolveError::FileNotFound { path: path.clone() })?;

            let module = (self.parser)(&source)
                .map_err(|msg| ResolveError::ParseError {
                    path: path.clone(),
                    message: msg,
                })?;

            // Extract module path from file path
            let module_path = self.path_to_module_path(&path);

            // Build exports map
            let mut exports = BTreeMap::new();
            for item in &module.items {
                if item.exported {
                    let (name, exported) = match &item.item {
                        Item::Const(c) => (c.name.clone(), ExportedItem::Const(c.clone())),
                        Item::ExternFn(e) => (e.name.clone(), ExportedItem::ExternFn(e.clone())),
                        Item::Function(f) => (f.name.clone(), ExportedItem::Function(f.clone())),
                    };
                    exports.insert(name, exported);
                }
            }

            // Build import maps
            let mut namespace_imports = BTreeMap::new();
            let mut named_imports = BTreeMap::new();

            for import in &module.imports {
                match import {
                    Import::Named { items, path: import_path } => {
                        let resolved_path = self.fs.normalize_path(&path, import_path);
                        pending.push(resolved_path.clone());
                        for spec in items {
                            let local = spec.local_name().to_string();
                            named_imports.insert(local, (resolved_path.clone(), spec.name.clone()));
                        }
                    }
                    Import::Namespace { alias, path: import_path } => {
                        let resolved_path = self.fs.normalize_path(&path, import_path);
                        pending.push(resolved_path.clone());
                        namespace_imports.insert(alias.clone(), resolved_path);
                    }
                    Import::SideEffect { path: import_path } => {
                        let resolved_path = self.fs.normalize_path(&path, import_path);
                        pending.push(resolved_path);
                    }
                }
            }

            in_progress.insert(path.clone());

            modules.insert(
                path.clone(),
                ResolvedModule {
                    path: path.clone(),
                    module,
                    module_path,
                    exports,
                    namespace_imports,
                    named_imports,
                },
            );

            in_progress.remove(&path);
        }

        Ok(modules)
    }

    /// Resolve and flatten a module tree into a single `Program`.
    ///
    /// The root module's items are not mangled (they keep their original names).
    /// All other modules have their items mangled with their module path prefix.
    pub fn resolve_and_flatten(&self, root_path: &str) -> Result<Program, ResolveError> {
        let modules = self.resolve(root_path)?;
        self.flatten(&modules, root_path)
    }

    /// Flatten resolved modules into a single `Program`.
    ///
    /// # Arguments
    /// * `modules` - The resolved module map
    /// * `root_path` - The path to the root module (its items won't be mangled)
    pub fn flatten(
        &self,
        modules: &BTreeMap<String, ResolvedModule>,
        root_path: &str,
    ) -> Result<Program, ResolveError> {
        let mut constants = Vec::new();
        let mut extern_fns = Vec::new();
        let mut functions = Vec::new();

        // Build a mapping from module path to its resolved module for lookups
        let path_to_module: BTreeMap<&str, &ResolvedModule> = modules
            .iter()
            .map(|(k, v)| (k.as_str(), v))
            .collect();

        // Process modules in topological order (dependencies first)
        let order = self.topological_sort(modules, root_path)?;

        for path in order {
            let resolved = modules.get(&path).unwrap();
            let is_root = path == root_path;
            let module_path = if is_root {
                &[] as &[String]
            } else {
                &resolved.module_path[..]
            };

            for item in &resolved.module.items {
                match &item.item {
                    Item::Const(c) => {
                        let mangled_name = if is_root {
                            c.name.clone()
                        } else {
                            mangle_module_constant_name(module_path, &c.name, &self.config)
                        };
                        let transformed_value = self.transform_expression(
                            &c.value,
                            resolved,
                            &path_to_module,
                            is_root,
                        )?;
                        constants.push(ConstDecl {
                            name: mangled_name,
                            ty: c.ty.clone(),
                            value: transformed_value,
                        });
                    }
                    Item::ExternFn(e) => {
                        let mangled_name = if is_root {
                            e.name.clone()
                        } else {
                            mangle_module_extern_function_name(module_path, &e.name, &self.config)
                        };
                        extern_fns.push(ExternFn {
                            name: mangled_name,
                            params: e.params.clone(),
                            return_type: e.return_type.clone(),
                        });
                    }
                    Item::Function(f) => {
                        let mangled_name = if is_root {
                            f.name.clone()
                        } else {
                            mangle_module_function_name(module_path, &f.name, &self.config)
                        };
                        let transformed_body = self.transform_block(
                            &f.body,
                            resolved,
                            &path_to_module,
                            is_root,
                        )?;
                        functions.push(Function {
                            name: mangled_name,
                            params: f.params.clone(),
                            return_type: f.return_type.clone(),
                            body: transformed_body,
                        });
                    }
                }
            }
        }

        Ok(Program {
            constants,
            extern_fns,
            functions,
        })
    }

    /// Topologically sort modules (dependencies first).
    fn topological_sort(
        &self,
        modules: &BTreeMap<String, ResolvedModule>,
        root_path: &str,
    ) -> Result<Vec<String>, ResolveError> {
        let mut result = Vec::new();
        let mut visited = BTreeSet::new();
        let mut in_progress = BTreeSet::new();

        fn visit(
            path: &str,
            modules: &BTreeMap<String, ResolvedModule>,
            result: &mut Vec<String>,
            visited: &mut BTreeSet<String>,
            in_progress: &mut BTreeSet<String>,
        ) -> Result<(), ResolveError> {
            if visited.contains(path) {
                return Ok(());
            }
            if in_progress.contains(path) {
                return Err(ResolveError::CyclicDependency {
                    cycle: vec![path.to_string()],
                });
            }

            in_progress.insert(path.to_string());

            if let Some(resolved) = modules.get(path) {
                // Visit dependencies from resolved imports
                // (paths were already resolved during the resolve() phase)
                for dep_path in resolved.namespace_imports.values() {
                    visit(dep_path, modules, result, visited, in_progress)?;
                }
                for (_, (dep_path, _)) in &resolved.named_imports {
                    visit(dep_path, modules, result, visited, in_progress)?;
                }
            }

            in_progress.remove(path);
            visited.insert(path.to_string());
            result.push(path.to_string());

            Ok(())
        }

        visit(root_path, modules, &mut result, &mut visited, &mut in_progress)?;

        // Also visit any modules that might not be reachable from root
        // (side-effect imports transitively)
        for path in modules.keys() {
            visit(path, modules, &mut result, &mut visited, &mut in_progress)?;
        }

        Ok(result)
    }

    /// Convert a file path to module path segments.
    fn path_to_module_path(&self, file_path: &str) -> Vec<String> {
        // Strip .em extension
        let path = file_path.strip_suffix(".em").unwrap_or(file_path);
        // Split by / and collect non-empty segments
        path.split('/')
            .filter(|s| !s.is_empty() && *s != ".")
            .map(|s| s.to_string())
            .collect()
    }

    /// Transform an expression, resolving imports and mangling names.
    fn transform_expression(
        &self,
        expr: &Expression,
        current_module: &ResolvedModule,
        modules: &BTreeMap<&str, &ResolvedModule>,
        is_root: bool,
    ) -> Result<Expression, ResolveError> {
        match expr {
            Expression::Literal(lit) => Ok(Expression::Literal(lit.clone())),

            Expression::Identifier(name) => {
                // Check if this is an imported name
                if let Some((source_path, original_name)) = current_module.named_imports.get(name) {
                    // Look up the source module
                    if let Some(source_module) = modules.get(source_path.as_str()) {
                        let source_module_path = &source_module.module_path;
                        // Mangle the name with the source module's path
                        let mangled = mangle_module_function_name(
                            source_module_path,
                            original_name,
                            &self.config,
                        );
                        return Ok(Expression::Identifier(mangled));
                    }
                }
                // Not imported, keep as-is (local reference)
                Ok(Expression::Identifier(name.clone()))
            }

            Expression::QualifiedIdentifier { namespace, name } => {
                // Look up the namespace import
                if let Some(source_path) = current_module.namespace_imports.get(namespace) {
                    if let Some(source_module) = modules.get(source_path.as_str()) {
                        let source_module_path = &source_module.module_path;
                        // Mangle the name with the source module's path
                        let mangled = mangle_module_function_name(
                            source_module_path,
                            name,
                            &self.config,
                        );
                        return Ok(Expression::Identifier(mangled));
                    }
                }
                Err(ResolveError::ModuleNotFound {
                    from: current_module.path.clone(),
                    path: namespace.clone(),
                })
            }

            Expression::Binary { op, left, right } => {
                let left = self.transform_expression(left, current_module, modules, is_root)?;
                let right = self.transform_expression(right, current_module, modules, is_root)?;
                Ok(Expression::Binary {
                    op: *op,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            Expression::Unary { op, operand } => {
                let operand = self.transform_expression(operand, current_module, modules, is_root)?;
                Ok(Expression::Unary {
                    op: *op,
                    operand: Box::new(operand),
                })
            }

            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.transform_expression(condition, current_module, modules, is_root)?;
                let then_branch = self.transform_block(then_branch, current_module, modules, is_root)?;
                let else_branch = self.transform_block(else_branch, current_module, modules, is_root)?;
                Ok(Expression::If {
                    condition: Box::new(condition),
                    then_branch,
                    else_branch,
                })
            }

            Expression::Operation { path, extern_fn, args } => {
                let args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|a| self.transform_expression(a, current_module, modules, is_root))
                    .collect();
                Ok(Expression::Operation {
                    path: path.clone(),
                    extern_fn: extern_fn.clone(),
                    args: args?,
                })
            }

            Expression::Call { function, args } => {
                let args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|a| self.transform_expression(a, current_module, modules, is_root))
                    .collect();
                
                // Check if this is an imported function
                if let Some((source_path, original_name)) = current_module.named_imports.get(function) {
                    if let Some(source_module) = modules.get(source_path.as_str()) {
                        let source_module_path = &source_module.module_path;
                        let mangled = mangle_module_function_name(
                            source_module_path,
                            original_name,
                            &self.config,
                        );
                        return Ok(Expression::Call {
                            function: mangled,
                            args: args?,
                        });
                    }
                }
                
                Ok(Expression::Call {
                    function: function.clone(),
                    args: args?,
                })
            }

            Expression::QualifiedCall { namespace, function, args } => {
                let args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|a| self.transform_expression(a, current_module, modules, is_root))
                    .collect();

                // Look up the namespace import
                if let Some(source_path) = current_module.namespace_imports.get(namespace) {
                    if let Some(source_module) = modules.get(source_path.as_str()) {
                        let source_module_path = &source_module.module_path;
                        let mangled = mangle_module_function_name(
                            source_module_path,
                            function,
                            &self.config,
                        );
                        return Ok(Expression::Call {
                            function: mangled,
                            args: args?,
                        });
                    }
                }
                Err(ResolveError::ModuleNotFound {
                    from: current_module.path.clone(),
                    path: namespace.clone(),
                })
            }

            Expression::Block(block) => {
                let block = self.transform_block(block, current_module, modules, is_root)?;
                Ok(Expression::Block(block))
            }

            Expression::Index { array, index } => {
                let array = self.transform_expression(array, current_module, modules, is_root)?;
                let index = self.transform_expression(index, current_module, modules, is_root)?;
                Ok(Expression::Index {
                    array: Box::new(array),
                    index: Box::new(index),
                })
            }

            Expression::Array(elements) => {
                let elements: Result<Vec<_>, _> = elements
                    .iter()
                    .map(|e| self.transform_expression(e, current_module, modules, is_root))
                    .collect();
                Ok(Expression::Array(elements?))
            }

            Expression::Cast { value, ty } => {
                let value = self.transform_expression(value, current_module, modules, is_root)?;
                Ok(Expression::Cast {
                    value: Box::new(value),
                    ty: ty.clone(),
                })
            }
        }
    }

    /// Transform a block, resolving imports and mangling names.
    fn transform_block(
        &self,
        block: &Block,
        current_module: &ResolvedModule,
        modules: &BTreeMap<&str, &ResolvedModule>,
        is_root: bool,
    ) -> Result<Block, ResolveError> {
        let statements: Result<Vec<_>, _> = block
            .statements
            .iter()
            .map(|s| self.transform_statement(s, current_module, modules, is_root))
            .collect();

        let result = match &block.result {
            Some(expr) => Some(Box::new(self.transform_expression(
                expr,
                current_module,
                modules,
                is_root,
            )?)),
            None => None,
        };

        Ok(Block {
            statements: statements?,
            result,
        })
    }

    /// Transform a statement, resolving imports and mangling names.
    fn transform_statement(
        &self,
        stmt: &Statement,
        current_module: &ResolvedModule,
        modules: &BTreeMap<&str, &ResolvedModule>,
        is_root: bool,
    ) -> Result<Statement, ResolveError> {
        match stmt {
            Statement::Let { name, mutable, ty, value } => {
                let value = self.transform_expression(value, current_module, modules, is_root)?;
                Ok(Statement::Let {
                    name: name.clone(),
                    mutable: *mutable,
                    ty: ty.clone(),
                    value,
                })
            }

            Statement::Assign { target, value } => {
                let value = self.transform_expression(value, current_module, modules, is_root)?;
                Ok(Statement::Assign {
                    target: target.clone(),
                    value,
                })
            }

            Statement::Expr(expr) => {
                let expr = self.transform_expression(expr, current_module, modules, is_root)?;
                Ok(Statement::Expr(expr))
            }

            Statement::If { condition, then_block, else_block } => {
                let condition = self.transform_expression(condition, current_module, modules, is_root)?;
                let then_block = self.transform_block(then_block, current_module, modules, is_root)?;
                let else_block = match else_block {
                    Some(ElseBlock::Block(b)) => {
                        Some(ElseBlock::Block(self.transform_block(b, current_module, modules, is_root)?))
                    }
                    Some(ElseBlock::ElseIf(stmt)) => {
                        Some(ElseBlock::ElseIf(Box::new(self.transform_statement(
                            stmt,
                            current_module,
                            modules,
                            is_root,
                        )?)))
                    }
                    None => None,
                };
                Ok(Statement::If {
                    condition,
                    then_block,
                    else_block,
                })
            }

            Statement::For { variable, start, end, direction, body } => {
                let start = self.transform_expression(start, current_module, modules, is_root)?;
                let end = self.transform_expression(end, current_module, modules, is_root)?;
                let body = self.transform_block(body, current_module, modules, is_root)?;
                Ok(Statement::For {
                    variable: variable.clone(),
                    start,
                    end,
                    direction: *direction,
                    body,
                })
            }

            Statement::Repeat { count, body } => {
                let count = self.transform_expression(count, current_module, modules, is_root)?;
                let body = self.transform_block(body, current_module, modules, is_root)?;
                Ok(Statement::Repeat { count, body })
            }

            Statement::While { condition, max_iterations, body } => {
                let condition = self.transform_expression(condition, current_module, modules, is_root)?;
                let max_iterations = self.transform_expression(max_iterations, current_module, modules, is_root)?;
                let body = self.transform_block(body, current_module, modules, is_root)?;
                Ok(Statement::While {
                    condition,
                    max_iterations,
                    body,
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::collections::BTreeMap;
    use crate::ast::ImportSpec;

    /// A simple in-memory file system for testing.
    struct MemoryFileSystem {
        files: BTreeMap<String, String>,
    }

    impl MemoryFileSystem {
        fn new() -> Self {
            Self {
                files: BTreeMap::new(),
            }
        }

        fn add_file(&mut self, path: &str, content: &str) {
            self.files.insert(path.to_string(), content.to_string());
        }
    }

    impl FileSystem for MemoryFileSystem {
        fn read_file(&self, path: &str) -> Option<String> {
            self.files.get(path).cloned()
        }

        fn exists(&self, path: &str) -> bool {
            self.files.contains_key(path)
        }
    }

    fn mock_parser(_source: &str) -> Result<Module, String> {
        // Minimal mock parser for testing
        // In real usage, this would be embeem_parser::parse_module
        Ok(Module {
            imports: Vec::new(),
            items: Vec::new(),
        })
    }

    #[test]
    fn test_path_normalization() {
        let fs = MemoryFileSystem::new();
        let _resolver = Resolver::new(&fs, mock_parser);

        // Relative path from root
        let path = ModulePath::relative(vec!["gpio".to_string()]);
        assert_eq!(fs.normalize_path("main.em", &path), "gpio.em");

        // Relative path from nested file
        let path = ModulePath::relative(vec!["utils".to_string()]);
        assert_eq!(fs.normalize_path("drivers/bme280.em", &path), "drivers/utils.em");

        // Parent reference
        let path = ModulePath::relative(vec!["gpio".to_string()]).with_parent_count(1);
        assert_eq!(fs.normalize_path("drivers/sensors/temp.em", &path), "drivers/gpio.em");
    }

    #[test]
    fn test_path_to_module_path() {
        let fs = MemoryFileSystem::new();
        let resolver = Resolver::new(&fs, mock_parser);

        assert_eq!(
            resolver.path_to_module_path("gpio.em"),
            vec!["gpio".to_string()]
        );
        assert_eq!(
            resolver.path_to_module_path("drivers/bme280.em"),
            vec!["drivers".to_string(), "bme280".to_string()]
        );
        assert_eq!(
            resolver.path_to_module_path("./drivers/sensors/temp.em"),
            vec!["drivers".to_string(), "sensors".to_string(), "temp".to_string()]
        );
    }

    #[test]
    fn test_resolve_single_module() {
        let mut fs = MemoryFileSystem::new();
        fs.add_file("main.em", "");

        let resolver = Resolver::new(&fs, mock_parser);
        let result = resolver.resolve("main.em");

        assert!(result.is_ok());
        let modules = result.unwrap();
        assert_eq!(modules.len(), 1);
        assert!(modules.contains_key("main.em"));
    }

    #[test]
    fn test_file_not_found() {
        let fs = MemoryFileSystem::new();
        let resolver = Resolver::new(&fs, mock_parser);
        let result = resolver.resolve("nonexistent.em");

        assert!(matches!(result, Err(ResolveError::FileNotFound { .. })));
    }
    
    #[test]
    fn test_resolve_multiple_modules() {
        use crate::ast::{ModuleItem, Item, Function, Block, PrimitiveType, Type};
        
        fn parser_with_imports(source: &str) -> Result<Module, String> {
            // A simple mock parser that creates modules with imports
            if source.contains("import gpio") {
                Ok(Module {
                    imports: vec![Import::Named {
                        items: vec![ImportSpec { name: "read_pin".to_string(), alias: None }],
                        path: ModulePath::relative(vec!["gpio".to_string()]),
                    }],
                    items: vec![ModuleItem {
                        exported: true,
                        item: Item::Function(Function {
                            name: "main".to_string(),
                            params: vec![],
                            return_type: Some(Type::Primitive(PrimitiveType::U32)),
                            body: Block { statements: vec![], result: None },
                        }),
                    }],
                })
            } else if source.contains("gpio module") {
                Ok(Module {
                    imports: vec![],
                    items: vec![ModuleItem {
                        exported: true,
                        item: Item::Function(Function {
                            name: "read_pin".to_string(),
                            params: vec![],
                            return_type: Some(Type::Primitive(PrimitiveType::Bool)),
                            body: Block { statements: vec![], result: None },
                        }),
                    }],
                })
            } else {
                Ok(Module { imports: vec![], items: vec![] })
            }
        }
        
        let mut fs = MemoryFileSystem::new();
        fs.add_file("main.em", "import gpio");
        fs.add_file("gpio.em", "gpio module");
        
        let resolver = Resolver::new(&fs, parser_with_imports);
        let result = resolver.resolve("main.em");
        
        assert!(result.is_ok());
        let modules = result.unwrap();
        assert_eq!(modules.len(), 2);
        assert!(modules.contains_key("main.em"));
        assert!(modules.contains_key("gpio.em"));
    }
    
    #[test]
    fn test_flatten_mangles_non_root() {
        use crate::ast::{ModuleItem, Item, Function, Block, PrimitiveType, Type, Literal, Expression};
        
        fn parser_with_functions(source: &str) -> Result<Module, String> {
            if source.contains("main module") {
                Ok(Module {
                    imports: vec![],
                    items: vec![ModuleItem {
                        exported: true,
                        item: Item::Function(Function {
                            name: "main".to_string(),
                            params: vec![],
                            return_type: Some(Type::Primitive(PrimitiveType::U32)),
                            body: Block { 
                                statements: vec![], 
                                result: Some(Box::new(Expression::Literal(Literal::Integer(42))))
                            },
                        }),
                    }],
                })
            } else {
                Ok(Module { imports: vec![], items: vec![] })
            }
        }
        
        let mut fs = MemoryFileSystem::new();
        fs.add_file("main.em", "main module");
        
        let resolver = Resolver::new(&fs, parser_with_functions);
        let result = resolver.resolve_and_flatten("main.em");
        
        assert!(result.is_ok());
        let program = result.unwrap();
        
        // Root module function should NOT be mangled
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "main");
    }
    
    #[test]
    fn test_flatten_imports_mangled() {
        use crate::ast::{ModuleItem, Item, Function, Block, PrimitiveType, Type, Literal, Expression};
        
        fn parser_with_imports_and_calls(source: &str) -> Result<Module, String> {
            if source.contains("main module") {
                Ok(Module {
                    imports: vec![Import::Named {
                        items: vec![ImportSpec { name: "helper".to_string(), alias: None }],
                        path: ModulePath::relative(vec!["utils".to_string()]),
                    }],
                    items: vec![ModuleItem {
                        exported: true,
                        item: Item::Function(Function {
                            name: "main".to_string(),
                            params: vec![],
                            return_type: Some(Type::Primitive(PrimitiveType::U32)),
                            body: Block { 
                                statements: vec![], 
                                result: Some(Box::new(Expression::Call {
                                    function: "helper".to_string(),
                                    args: vec![],
                                }))
                            },
                        }),
                    }],
                })
            } else if source.contains("utils module") {
                Ok(Module {
                    imports: vec![],
                    items: vec![ModuleItem {
                        exported: true,
                        item: Item::Function(Function {
                            name: "helper".to_string(),
                            params: vec![],
                            return_type: Some(Type::Primitive(PrimitiveType::U32)),
                            body: Block { 
                                statements: vec![], 
                                result: Some(Box::new(Expression::Literal(Literal::Integer(42))))
                            },
                        }),
                    }],
                })
            } else {
                Ok(Module { imports: vec![], items: vec![] })
            }
        }
        
        let mut fs = MemoryFileSystem::new();
        fs.add_file("main.em", "main module");
        fs.add_file("utils.em", "utils module");
        
        let resolver = Resolver::new(&fs, parser_with_imports_and_calls);
        let result = resolver.resolve_and_flatten("main.em");
        
        assert!(result.is_ok());
        let program = result.unwrap();
        
        // Should have 2 functions total
        assert_eq!(program.functions.len(), 2);
        
        // Find the main function (not mangled since it's from root)
        let main_fn = program.functions.iter().find(|f| f.name == "main").unwrap();
        
        // The call to helper should be mangled
        if let Some(Expression::Call { function, .. }) = &main_fn.body.result.as_ref().map(|b| b.as_ref()) {
            // The helper function is from utils module, so it should be mangled
            assert!(function.contains("utils"), "Function call should reference mangled name: {}", function);
        }
        
        // The helper function from utils should be mangled
        let helper_fn = program.functions.iter().find(|f| f.name.contains("helper")).unwrap();
        assert!(helper_fn.name.contains("utils"), "Helper function should be mangled: {}", helper_fn.name);
    }
}