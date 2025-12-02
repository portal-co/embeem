use embeem_codegen::{compile_to_c, compile_to_c_gcc, CCodegen};
use embeem_parser::{parse_program, parse_module};

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
        const LedPin: u8 = 13;
        fn main() {}
    "#;
    let program = parse_program(src).unwrap();
    let c_code = compile_to_c(&program).unwrap();
    assert!(c_code.contains("#define LedPin"));
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
    let src = r#"
        export const LedPin: u8 = 13;
        
        export fn blink(times: u32) {
            for i in 0 to times {
                GPIO_TOGGLE(LedPin);
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
    assert!(c_code.contains("embeem_mod_$1_4_gpio_LedPin"), 
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
