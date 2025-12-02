//! Validates inline Embeem code found in test source files.
//!
//! This test finds string literals in Rust test files throughout the workspace,
//! attempts to parse them as Embeem programs or modules, and validates those
//! that parse successfully against the Embeem specification.

use std::fs;
use std::path::{Path, PathBuf};

use embeem_ast::validate::{validate_program, validate_module, ValidationResult};
use embeem_parser::{parse_program, parse_module};
use syn::visit::Visit;
use syn::{Expr, ExprLit, Lit, File};
use walkdir::WalkDir;

/// Visitor that collects string literals from Rust source code.
struct StringLiteralVisitor {
    /// Collected string literals.
    strings: Vec<String>,
}

impl StringLiteralVisitor {
    fn new() -> Self {
        Self { strings: Vec::new() }
    }
}

impl<'ast> Visit<'ast> for StringLiteralVisitor {
    fn visit_expr(&mut self, node: &'ast Expr) {
        if let Expr::Lit(ExprLit { lit: Lit::Str(lit_str), .. }) = node {
            self.strings.push(lit_str.value());
        }
        // Continue visiting nested expressions
        syn::visit::visit_expr(self, node);
    }
}

/// Extract all string literals from a Rust source file.
fn extract_string_literals(source: &str) -> Vec<String> {
    let syntax_tree: File = match syn::parse_file(source) {
        Ok(tree) => tree,
        Err(_) => return Vec::new(),
    };

    let mut visitor = StringLiteralVisitor::new();
    visitor.visit_file(&syntax_tree);
    visitor.strings
}

/// Information about an inline Embeem script that was validated.
#[derive(Debug)]
struct InlineScript {
    /// Path to the source file containing the script.
    file_path: PathBuf,
    /// The Embeem source code.
    source: String,
    /// Whether it was parsed as a program (vs module).
    is_program: bool,
    /// Validation result.
    result: ValidationResult,
}

/// Try to parse a string as Embeem code and validate it if successful.
fn try_validate_embeem(source: &str) -> Option<(bool, ValidationResult)> {
    // Skip strings that are obviously not Embeem code
    if source.len() < 10 {
        return None;
    }
    
    // Skip strings that don't contain any Embeem keywords
    let has_embeem_keyword = source.contains("fn ") 
        || source.contains("const ")
        || source.contains("extern fn")
        || source.contains("import ");
    
    if !has_embeem_keyword {
        return None;
    }
    
    // Try parsing as a program first
    if let Ok(program) = parse_program(source) {
        let result = validate_program(&program);
        return Some((true, result));
    }
    
    // Try parsing as a module
    if let Ok(module) = parse_module(source) {
        let result = validate_module(&module);
        return Some((false, result));
    }
    
    None
}

/// Find all Rust test files in the workspace.
fn find_rust_test_files(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    
    for entry in WalkDir::new(root)
        .into_iter()
        .filter_entry(|e| {
            // Skip target directory and hidden directories
            let name = e.file_name().to_string_lossy();
            !name.starts_with('.') && name != "target"
        })
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        
        // Only look at Rust files in tests directories or with _test suffix
        if path.extension().map(|e| e == "rs").unwrap_or(false) {
            let path_str = path.to_string_lossy();
            if path_str.contains("/tests/") 
                || path_str.contains("_test.rs")
                || path_str.contains("_tests.rs") 
            {
                files.push(path.to_path_buf());
            }
        }
    }
    
    files
}

/// Validate all inline Embeem scripts found in test files.
#[test]
fn validate_all_inline_embeem_scripts() {
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    
    let test_files = find_rust_test_files(workspace_root);
    
    let mut all_scripts: Vec<InlineScript> = Vec::new();
    let mut total_validated = 0;
    let mut total_valid = 0;
    let mut total_invalid = 0;
    
    for file_path in &test_files {
        let source = match fs::read_to_string(file_path) {
            Ok(s) => s,
            Err(_) => continue,
        };
        
        let strings = extract_string_literals(&source);
        
        for string in strings {
            if let Some((is_program, result)) = try_validate_embeem(&string) {
                total_validated += 1;
                
                if result.is_valid() {
                    total_valid += 1;
                } else {
                    total_invalid += 1;
                    all_scripts.push(InlineScript {
                        file_path: file_path.clone(),
                        source: string,
                        is_program,
                        result,
                    });
                }
            }
        }
    }
    
    // Report results
    println!("\n=== Inline Embeem Validation Report ===");
    println!("Test files scanned: {}", test_files.len());
    println!("Embeem scripts found: {}", total_validated);
    println!("Valid: {}", total_valid);
    println!("Invalid: {}", total_invalid);
    
    if !all_scripts.is_empty() {
        println!("\n=== Invalid Scripts ===");
        for script in &all_scripts {
            println!("\nFile: {}", script.file_path.display());
            println!("Type: {}", if script.is_program { "Program" } else { "Module" });
            println!("Source preview: {}...", 
                script.source.chars().take(60).collect::<String>().replace('\n', " "));
            println!("Errors:");
            for error in &script.result.errors {
                println!("  - [{:?}] {} (at {})", error.kind, error.message, error.context);
            }
        }
        
        panic!(
            "{} inline Embeem scripts failed validation. See output above for details.",
            total_invalid
        );
    }
    
    println!("\nAll {} inline Embeem scripts validated successfully!", total_valid);
}

/// Test that the string extraction works correctly.
#[test]
fn test_string_extraction() {
    // Note: We use a simple example without nested raw strings
    let simple_source = r##"
        fn test() {
            let x = "hello world";
            let y = "fn main() { }";
        }
    "##;
    
    let strings = extract_string_literals(simple_source);
    assert!(strings.len() >= 2, "Should find at least 2 string literals");
    assert!(strings.iter().any(|s| s == "hello world"));
    assert!(strings.iter().any(|s| s == "fn main() { }"));
}

/// Test the Embeem detection heuristic.
#[test]
fn test_embeem_detection() {
    // Should detect Embeem
    let embeem_code = r##"
        fn main() {
            GPIO_SET_MODE(13, 1);
        }
    "##;
    assert!(try_validate_embeem(embeem_code).is_some());
    
    // Should not detect as Embeem (no fn/const/extern/import keywords)
    let not_embeem = "just a regular string with no keywords";
    assert!(try_validate_embeem(not_embeem).is_none());
    
    // Short strings should be skipped
    let short = "fn main";
    assert!(try_validate_embeem(short).is_none());
}
