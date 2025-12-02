use embeem_codegen::{compile_to_ts, compile_to_js, TsCodegen, TsCodegenOptions};
use embeem_parser::parse_program;

#[test]
fn test_simple_program() {
    let src = r#"
        fn main() {
            GPIO_SET_MODE(13, 1);
        }
    "#;
    let program = parse_program(src).unwrap();
    let ts_code = compile_to_ts(&program).unwrap();
    assert!(ts_code.contains("export function embeem_main()"), "Expected export function declaration, got:\n{}", ts_code);
    assert!(ts_code.contains("embeem_op_$1_13_GPIO_SET_MODE"), "Expected mangled op, got:\n{}", ts_code);
}

#[test]
fn test_constants() {
    let src = r#"
        const LedPin: u8 = 13;
        fn main() {}
    "#;
    let program = parse_program(src).unwrap();
    let ts_code = compile_to_ts(&program).unwrap();
    assert!(ts_code.contains("export const LedPin: number = 13;"), "Expected export const, got:\n{}", ts_code);
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
    let ts_code = compile_to_ts(&program).unwrap();
    assert!(ts_code.contains("for"));
    assert!(ts_code.contains("< 10"));
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
    let ts_code = compile_to_ts(&program).unwrap();
    assert!(ts_code.contains("for"));
    assert!(ts_code.contains("i++"));
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
    let ts_code = compile_to_ts(&program).unwrap();
    assert!(ts_code.contains("if"));
    assert!(ts_code.contains("else"));
}

#[test]
fn test_extern_fn() {
    let src = r#"
        extern fn get_sensor_value(channel: u8) -> i32;
        extern fn set_led(pin: u8, value: bool);
        
        fn main() {
            let val = get_sensor_value(0);
            set_led(13, val > 100);
        }
    "#;
    let program = parse_program(src).unwrap();
    let ts_code = compile_to_ts(&program).unwrap();
    
    // Check import statement for external functions
    assert!(ts_code.contains("import { embeem_extern_get_sensor_value, embeem_extern_set_led } from"), 
        "Expected import statement, got:\n{}", ts_code);
    
    // Check that external functions are called with extern_prefix
    assert!(ts_code.contains("embeem_extern_get_sensor_value("), 
        "Expected call to embeem_extern_get_sensor_value, got:\n{}", ts_code);
    assert!(ts_code.contains("embeem_extern_set_led("), 
        "Expected call to embeem_extern_set_led, got:\n{}", ts_code);
}

#[test]
fn test_hybrid_operation_mangling() {
    let src = r#"
        extern fn sensor_read(channel: u8) -> u16;
        
        fn main() {
            WRITE(GPIO(sensor_read(0)));
        }
    "#;
    let program = parse_program(src).unwrap();
    let ts_code = compile_to_ts(&program).unwrap();
    
    assert!(ts_code.contains("embeem_extern_$2_5_WRITE_4_GPIO_sensor_read("), 
        "Expected hybrid mangled name, got:\n{}", ts_code);
}

#[test]
fn test_operation_mangling() {
    let src = r#"
        fn main() {
            FSUB(1.0, 2.0);
            READ(ADC(0));
        }
    "#;
    let program = parse_program(src).unwrap();
    let ts_code = compile_to_ts(&program).unwrap();
    
    // FSUB is inlined as subtraction in TS
    assert!(ts_code.contains("(1 - 2)"), "Expected FSUB inline, got:\n{}", ts_code);
    // READ(ADC(...)) -> embeem_op_$2_4_READ_3_ADC
    assert!(ts_code.contains("embeem_op_$2_4_READ_3_ADC("), "Expected mangled op, got:\n{}", ts_code);
}

#[test]
fn test_js_output() {
    let src = r#"
        fn main() {
            let x: u32 = 42;
        }
    "#;
    let program = parse_program(src).unwrap();
    let js_code = compile_to_js(&program).unwrap();
    
    // JS output should not have type annotations
    assert!(!js_code.contains(": number"), "JS output should not have types, got:\n{}", js_code);
    assert!(js_code.contains("const x = 42;"), "Expected const declaration, got:\n{}", js_code);
}

#[test]
fn test_ternary_operator() {
    let src = r#"
        fn max_val(a: u32, b: u32) -> u32 {
            if a > b { a } else { b }
        }
    "#;
    let program = parse_program(src).unwrap();
    let ts_code = compile_to_ts(&program).unwrap();
    assert!(ts_code.contains("?"), "Expected ternary operator '?', got:\n{}", ts_code);
    assert!(ts_code.contains(":"));
}

#[test]
fn test_strict_equality() {
    let src = r#"
        fn test() {
            let x: u32 = 10;
            let y: u32 = 5;
            let is_equal = x == y;
        }
    "#;
    let program = parse_program(src).unwrap();
    let ts_code = compile_to_ts(&program).unwrap();
    // TypeScript should use strict equality ===
    assert!(ts_code.contains("==="), "Expected strict equality ===, got:\n{}", ts_code);
}

#[test]
fn test_esm_module_basic() {
    use embeem_ast::{Module, ModuleItem, Item, Function, Block, PrimitiveType, Type, Expression, Literal};
    
    let module = Module {
        imports: vec![],
        items: vec![
            ModuleItem {
                exported: true,
                item: Item::Function(Function {
                    name: "greet".to_string(),
                    params: vec![],
                    return_type: Some(Type::Primitive(PrimitiveType::U32)),
                    body: Block {
                        statements: vec![],
                        result: Some(Box::new(Expression::Literal(Literal::Integer(42)))),
                    },
                }),
            },
            ModuleItem {
                exported: false,
                item: Item::Function(Function {
                    name: "helper".to_string(),
                    params: vec![],
                    return_type: None,
                    body: Block { statements: vec![], result: None },
                }),
            },
        ],
    };

    let mut codegen = TsCodegen::with_options(
        TsCodegenOptions::new().with_esm()
    );
    let ts_code = codegen.generate_module(&module, &[]).unwrap();

    // Exported function should have `export` keyword
    assert!(ts_code.contains("export function greet"), "Expected exported function, got:\n{}", ts_code);
    // Non-exported function should NOT have `export` keyword
    assert!(ts_code.contains("function helper"), "Expected non-exported function, got:\n{}", ts_code);
    assert!(!ts_code.contains("export function helper"), "helper should not be exported");
}

#[test]
fn test_esm_imports() {
    use embeem_ast::{Module, ModuleItem, Item, Function, Block, Import, ImportSpec, ModulePath};
    
    let module = Module {
        imports: vec![
            Import::Named {
                items: vec![
                    ImportSpec { name: "foo".to_string(), alias: None },
                    ImportSpec { name: "bar".to_string(), alias: Some("baz".to_string()) },
                ],
                path: ModulePath::relative(vec!["utils".to_string()]),
            },
            Import::Namespace {
                alias: "gpio".to_string(),
                path: ModulePath::relative(vec!["drivers".to_string(), "gpio".to_string()]),
            },
        ],
        items: vec![
            ModuleItem {
                exported: true,
                item: Item::Function(Function {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: None,
                    body: Block { statements: vec![], result: None },
                }),
            },
        ],
    };

    let mut codegen = TsCodegen::with_options(
        TsCodegenOptions::new().with_esm()
    );
    let ts_code = codegen.generate_module(&module, &[]).unwrap();

    // Check named import
    assert!(ts_code.contains("import { foo, bar as baz } from \"./utils.js\";"), 
        "Expected named import, got:\n{}", ts_code);
    // Check namespace import  
    assert!(ts_code.contains("import * as gpio from \"./drivers/gpio.js\";"),
        "Expected namespace import, got:\n{}", ts_code);
}

#[test]
fn test_esm_parent_path() {
    use embeem_ast::{Module, Import, ImportSpec, ModulePath};
    
    let module = Module {
        imports: vec![
            Import::Named {
                items: vec![ImportSpec { name: "util".to_string(), alias: None }],
                path: ModulePath::relative(vec!["common".to_string()]).with_parent_count(2),
            },
        ],
        items: vec![],
    };

    let mut codegen = TsCodegen::with_options(
        TsCodegenOptions::new().with_esm()
    );
    let ts_code = codegen.generate_module(&module, &[]).unwrap();

    // Check parent path resolution
    assert!(ts_code.contains("import { util } from \"../../common.js\";"), 
        "Expected parent path import, got:\n{}", ts_code);
}

#[test]
fn test_esm_constants() {
    use embeem_ast::{Module, ModuleItem, Item, ConstDecl, PrimitiveType, Type, Expression, Literal};
    
    let module = Module {
        imports: vec![],
        items: vec![
            ModuleItem {
                exported: true,
                item: Item::Const(ConstDecl {
                    name: "LedPin".to_string(),
                    ty: Type::Primitive(PrimitiveType::U8),
                    value: Expression::Literal(Literal::Integer(13)),
                }),
            },
            ModuleItem {
                exported: false,
                item: Item::Const(ConstDecl {
                    name: "Internal".to_string(),
                    ty: Type::Primitive(PrimitiveType::U32),
                    value: Expression::Literal(Literal::Integer(100)),
                }),
            },
        ],
    };

    let mut codegen = TsCodegen::with_options(
        TsCodegenOptions::new().with_esm()
    );
    let ts_code = codegen.generate_module(&module, &[]).unwrap();

    assert!(ts_code.contains("export const LedPin: number = 13;"), 
        "Expected exported constant, got:\n{}", ts_code);
    assert!(ts_code.contains("const Internal: number = 100;"), 
        "Expected non-exported constant, got:\n{}", ts_code);
    assert!(!ts_code.contains("export const Internal"), 
        "Internal should not be exported");
}

#[test]
fn test_esm_custom_extension() {
    use embeem_ast::{Module, Import, ImportSpec, ModulePath};
    
    let module = Module {
        imports: vec![
            Import::Named {
                items: vec![ImportSpec { name: "foo".to_string(), alias: None }],
                path: ModulePath::relative(vec!["utils".to_string()]),
            },
        ],
        items: vec![],
    };

    let mut codegen = TsCodegen::with_options(
        TsCodegenOptions::new()
            .with_esm()
            .with_esm_extension(".ts")
    );
    let ts_code = codegen.generate_module(&module, &[]).unwrap();

    assert!(ts_code.contains("from \"./utils.ts\";"), 
        "Expected .ts extension, got:\n{}", ts_code);
}
