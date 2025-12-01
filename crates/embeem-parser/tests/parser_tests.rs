use embeem_parser::*;
use embeem_ast::{Expression, Import, Item, Literal, Statement};

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
fn test_parse_repeat_variable_bound() {
    // Totality is preserved because count is immutable
    let src = r#"
        fn process(count: u32) {
            repeat count {
                GPIO_TOGGLE(13);
            }
        }
    "#;
    let result = parse_program(src);
    assert!(result.is_ok());
    let prog = result.unwrap();
    let func = &prog.functions[0];
    // Check that the repeat statement has a variable bound
    if let Statement::Repeat { count, .. } = &func.body.statements[0] {
        if let Expression::Identifier(name) = count {
            assert_eq!(name, "count");
        } else {
            panic!("Expected identifier for repeat count");
        }
    } else {
        panic!("Expected Repeat statement");
    }
}

#[test]
fn test_parse_while_variable_max() {
    // Totality is preserved because max_iter is immutable
    let src = r#"
        fn search(max_iter: u32) {
            let mut found = false;
            while not found max max_iter {
                found = CHECK_CONDITION();
            }
        }
    "#;
    let result = parse_program(src);
    assert!(result.is_ok());
    let prog = result.unwrap();
    let func = &prog.functions[0];
    // Check that the while statement has a variable max
    if let Statement::While { max_iterations, .. } = &func.body.statements[1] {
        if let Expression::Identifier(name) = max_iterations {
            assert_eq!(name, "max_iter");
        } else {
            panic!("Expected identifier for max_iterations");
        }
    } else {
        panic!("Expected While statement");
    }
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
    // The virtual call should be parsed as an Operation with path ["WRITE", "GPIO"]
    if let Statement::Expr(Expression::Operation { path, extern_fn, args }) = &main.body.statements[0] {
        assert_eq!(path, &["WRITE", "GPIO"]);
        assert!(extern_fn.is_none());
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
    let prog = result.unwrap();
    let main = &prog.functions[0];
    // Check that it's parsed as a let statement with Operation
    if let Statement::Let { value, .. } = &main.body.statements[0] {
        if let Expression::Operation { path, extern_fn, args } = value {
            assert_eq!(path, &["READ", "GPIO"]);
            assert!(extern_fn.is_none());
            assert_eq!(args.len(), 1);
        } else {
            panic!("Expected Operation expression");
        }
    } else {
        panic!("Expected Let statement");
    }
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
    let prog = result.unwrap();
    let main = &prog.functions[0];
    
    // Check first statement: START(PWM(0))
    if let Statement::Expr(Expression::Operation { path, extern_fn, .. }) = &main.body.statements[0] {
        assert_eq!(path, &["START", "PWM"]);
        assert!(extern_fn.is_none());
    } else {
        panic!("Expected Operation expression");
    }
    
    // Check second statement: SET_DUTY_CYCLE(PWM(0), 128)
    if let Statement::Expr(Expression::Operation { path, extern_fn, args }) = &main.body.statements[1] {
        assert_eq!(path, &["SET_DUTY_CYCLE", "PWM"]);
        assert!(extern_fn.is_none());
        assert_eq!(args.len(), 2); // 0 and 128
    } else {
        panic!("Expected Operation expression");
    }
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
    let prog = result.unwrap();
    let main = &prog.functions[0];
    
    // Check first statement: ENABLE(WDT())
    if let Statement::Expr(Expression::Operation { path, extern_fn, args }) = &main.body.statements[0] {
        assert_eq!(path, &["ENABLE", "WDT"]);
        assert!(extern_fn.is_none());
        assert_eq!(args.len(), 0);
    } else {
        panic!("Expected Operation expression");
    }
}

#[test]
fn test_parse_non_virtual_operation() {
    let src = r#"
        fn main() {
            GPIO_WRITE(13, 1);
        }
    "#;
    let result = parse_program(src);
    assert!(result.is_ok());
    let prog = result.unwrap();
    let main = &prog.functions[0];
    
    // Non-virtual operation should be parsed with single-element path
    if let Statement::Expr(Expression::Operation { path, extern_fn, args }) = &main.body.statements[0] {
        assert_eq!(path, &["GPIO_WRITE"]);
        assert!(extern_fn.is_none());
        assert_eq!(args.len(), 2);
    } else {
        panic!("Expected Operation expression");
    }
}

#[test]
fn test_parse_triple_nested_operation() {
    let src = r#"
        fn main() {
            A(B(C(1)));
        }
    "#;
    let result = parse_program(src);
    assert!(result.is_ok());
    let prog = result.unwrap();
    let main = &prog.functions[0];
    
    // Triple-nested operation: A(B(C(1))) -> path: ["A", "B", "C"], args: [1]
    if let Statement::Expr(Expression::Operation { path, extern_fn, args }) = &main.body.statements[0] {
        assert_eq!(path, &["A", "B", "C"]);
        assert!(extern_fn.is_none());
        assert_eq!(args.len(), 1);
    } else {
        panic!("Expected Operation expression");
    }
}

#[test]
fn test_parse_extern_fn() {
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
    let result = parse_program(src);
    assert!(result.is_ok());
    let prog = result.unwrap();
    
    // Check external functions
    assert_eq!(prog.extern_fns.len(), 3);
    
    assert_eq!(prog.extern_fns[0].name, "get_sensor_value");
    assert_eq!(prog.extern_fns[0].params.len(), 1);
    assert_eq!(prog.extern_fns[0].params[0].name, "channel");
    assert!(prog.extern_fns[0].return_type.is_some());
    
    assert_eq!(prog.extern_fns[1].name, "set_led");
    assert_eq!(prog.extern_fns[1].params.len(), 2);
    assert!(prog.extern_fns[1].return_type.is_none());
    
    assert_eq!(prog.extern_fns[2].name, "init_system");
    assert_eq!(prog.extern_fns[2].params.len(), 0);
    assert!(prog.extern_fns[2].return_type.is_none());
    
    // Check that the main function can call external functions
    assert_eq!(prog.functions.len(), 1);
}

#[test]
fn test_parse_hybrid_operation() {
    let src = r#"
        extern fn sensor_read(channel: u8) -> u16;
        
        fn main() {
            // Hybrid operation: extern fn as last segment of operation path
            WRITE(GPIO(sensor_read(0)));
        }
    "#;
    let result = parse_program(src);
    assert!(result.is_ok());
    let prog = result.unwrap();
    
    // Check the hybrid operation
    let main = &prog.functions[0];
    if let Statement::Expr(Expression::Operation { path, extern_fn, args }) = &main.body.statements[0] {
        assert_eq!(path, &["WRITE", "GPIO"]);
        assert_eq!(extern_fn.as_deref(), Some("sensor_read"));
        assert_eq!(args.len(), 1); // The argument is 0
        if let Expression::Literal(Literal::Integer(0)) = &args[0] {
            // Correct
        } else {
            panic!("Expected integer literal 0, got {:?}", args[0]);
        }
    } else {
        panic!("Expected Operation expression");
    }
}

#[test]
fn test_parse_hybrid_operation_with_multiple_args() {
    let src = r#"
        extern fn read_sensor(channel: u8) -> u16;
        
        fn main() {
            // Hybrid operation with additional args
            WRITE(SPI(read_sensor(1)), 42, 0xFF);
        }
    "#;
    let result = parse_program(src);
    assert!(result.is_ok());
    let prog = result.unwrap();
    
    let main = &prog.functions[0];
    if let Statement::Expr(Expression::Operation { path, extern_fn, args }) = &main.body.statements[0] {
        assert_eq!(path, &["WRITE", "SPI"]);
        assert_eq!(extern_fn.as_deref(), Some("read_sensor"));
        assert_eq!(args.len(), 3); // 1, 42, 0xFF
    } else {
        panic!("Expected Operation expression");
    }
}

#[test]
fn test_parse_module_with_imports() {
    let src = r#"
        import { blink, LED_PIN } from "./gpio";
        import * as sensors from "drivers/bme280";
        
        fn main() {
            blink(10);
        }
    "#;
    let result = parse_module(src);
    assert!(result.is_ok());
    let module = result.unwrap();
    
    // Check imports
    assert_eq!(module.imports.len(), 2);
    
    // Named import
    if let Import::Named { items, path } = &module.imports[0] {
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].name, "blink");
        assert!(items[0].alias.is_none());
        assert_eq!(items[1].name, "LED_PIN");
        assert!(path.is_relative);
        assert_eq!(path.segments, vec!["gpio"]);
    } else {
        panic!("Expected named import");
    }
    
    // Namespace import
    if let Import::Namespace { alias, path } = &module.imports[1] {
        assert_eq!(alias, "sensors");
        assert!(!path.is_relative);
        assert_eq!(path.segments, vec!["drivers", "bme280"]);
    } else {
        panic!("Expected namespace import");
    }
    
    // Check functions
    assert_eq!(module.items.len(), 1);
}

#[test]
fn test_parse_module_with_exports() {
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
    let result = parse_module(src);
    assert!(result.is_ok());
    let module = result.unwrap();
    
    assert_eq!(module.items.len(), 3);
    
    // First item: exported const
    assert!(module.items[0].exported);
    if let Item::Const(c) = &module.items[0].item {
        assert_eq!(c.name, "LED_PIN");
    } else {
        panic!("Expected const");
    }
    
    // Second item: exported function
    assert!(module.items[1].exported);
    if let Item::Function(f) = &module.items[1].item {
        assert_eq!(f.name, "blink");
    } else {
        panic!("Expected function");
    }
    
    // Third item: not exported
    assert!(!module.items[2].exported);
    if let Item::Function(f) = &module.items[2].item {
        assert_eq!(f.name, "helper");
    } else {
        panic!("Expected function");
    }
}

#[test]
fn test_parse_import_with_alias() {
    let src = r#"
        import { blink as gpio_blink, LED_PIN as PIN } from "./gpio";
        
        fn main() {}
    "#;
    let result = parse_module(src);
    assert!(result.is_ok());
    let module = result.unwrap();
    
    if let Import::Named { items, .. } = &module.imports[0] {
        assert_eq!(items[0].name, "blink");
        assert_eq!(items[0].alias.as_deref(), Some("gpio_blink"));
        assert_eq!(items[0].local_name(), "gpio_blink");
        
        assert_eq!(items[1].name, "LED_PIN");
        assert_eq!(items[1].alias.as_deref(), Some("PIN"));
        assert_eq!(items[1].local_name(), "PIN");
    } else {
        panic!("Expected named import");
    }
}

#[test]
fn test_parse_qualified_identifier() {
    let src = r#"
        import * as gpio from "./gpio";
        
        fn main() {
            let pin = gpio::LED_PIN;
            gpio::blink(10);
        }
    "#;
    let result = parse_module(src);
    assert!(result.is_ok());
    let module = result.unwrap();
    
    // Find the main function
    let main_fn = module.items.iter().find_map(|item| {
        if let Item::Function(f) = &item.item {
            if f.name == "main" { Some(f) } else { None }
        } else { None }
    }).expect("main function not found");
    
    // First statement: let pin = gpio::LED_PIN;
    if let Statement::Let { value, .. } = &main_fn.body.statements[0] {
        if let Expression::QualifiedIdentifier { namespace, name } = value {
            assert_eq!(namespace, "gpio");
            assert_eq!(name, "LED_PIN");
        } else {
            panic!("Expected qualified identifier, got {:?}", value);
        }
    } else {
        panic!("Expected let statement");
    }
    
    // Second statement: gpio::blink(10);
    if let Statement::Expr(Expression::QualifiedCall { namespace, function, args }) = &main_fn.body.statements[1] {
        assert_eq!(namespace, "gpio");
        assert_eq!(function, "blink");
        assert_eq!(args.len(), 1);
    } else {
        panic!("Expected qualified call");
    }
}

#[test]
fn test_parse_module_path_relative() {
    let src = r#"
        import { a } from "./foo";
        import { b } from "../bar";
        import { c } from "../../baz/qux";
        
        fn main() {}
    "#;
    let result = parse_module(src);
    assert!(result.is_ok());
    let module = result.unwrap();
    
    // Check relative paths
    if let Import::Named { path, .. } = &module.imports[0] {
        assert!(path.is_relative);
        assert_eq!(path.parent_count, 0);
        assert_eq!(path.segments, vec!["foo"]);
    }
    
    if let Import::Named { path, .. } = &module.imports[1] {
        assert!(path.is_relative);
        assert_eq!(path.parent_count, 1);
        assert_eq!(path.segments, vec!["bar"]);
    }
    
    if let Import::Named { path, .. } = &module.imports[2] {
        assert!(path.is_relative);
        assert_eq!(path.parent_count, 2);
        assert_eq!(path.segments, vec!["baz", "qux"]);
    }
}

#[test]
fn test_parse_export_extern_fn() {
    let src = r#"
        export extern fn platform_init();
        
        fn main() {
            platform_init();
        }
    "#;
    let result = parse_module(src);
    assert!(result.is_ok());
    let module = result.unwrap();
    
    assert!(module.items[0].exported);
    if let Item::ExternFn(e) = &module.items[0].item {
        assert_eq!(e.name, "platform_init");
    } else {
        panic!("Expected extern fn");
    }
}
