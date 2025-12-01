//! Validation tests for example files against the specification.

use std::fs;
use std::path::Path;

use embeem_parser::parse_program;
use embeem_ast::validate::validate_program;

fn validate_example(filename: &str) -> (bool, Vec<String>) {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("examples")
        .join(filename);

    let source = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));

    let program = match parse_program(&source) {
        Ok(p) => p,
        Err(e) => {
            return (false, vec![format!("Parse error: {:?}", e)]);
        }
    };

    let result = validate_program(&program);
    
    let error_msgs: Vec<String> = result.errors.iter()
        .map(|e| format!("[{:?}] {} (at {})", e.kind, e.message, e.context))
        .collect();

    (result.is_valid(), error_msgs)
}

#[test]
fn validate_01_blink() {
    let (valid, errors) = validate_example("01_blink.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "01_blink.em should be valid");
}

#[test]
fn validate_02_button() {
    let (valid, errors) = validate_example("02_button.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "02_button.em should be valid");
}

#[test]
fn validate_03_pwm_fade() {
    let (valid, errors) = validate_example("03_pwm_fade.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "03_pwm_fade.em should be valid");
}

#[test]
fn validate_04_analog_sensor() {
    let (valid, errors) = validate_example("04_analog_sensor.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "04_analog_sensor.em should be valid");
}

#[test]
fn validate_05_uart_echo() {
    let (valid, errors) = validate_example("05_uart_echo.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "05_uart_echo.em should be valid");
}

#[test]
fn validate_06_i2c_sensor() {
    let (valid, errors) = validate_example("06_i2c_sensor.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "06_i2c_sensor.em should be valid");
}

#[test]
fn validate_07_state_machine() {
    let (valid, errors) = validate_example("07_state_machine.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "07_state_machine.em should be valid");
}

#[test]
fn validate_08_bit_manipulation() {
    let (valid, errors) = validate_example("08_bit_manipulation.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "08_bit_manipulation.em should be valid");
}

#[test]
fn validate_09_timer_watchdog() {
    let (valid, errors) = validate_example("09_timer_watchdog.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "09_timer_watchdog.em should be valid");
}

#[test]
fn validate_10_floating_point() {
    let (valid, errors) = validate_example("10_floating_point.em");
    if !valid {
        for e in &errors {
            eprintln!("{}", e);
        }
    }
    assert!(valid, "10_floating_point.em should be valid");
}

/// Run all validations and print comprehensive report
#[test]
fn validate_all_examples_report() {
    let examples = [
        "01_blink.em",
        "02_button.em",
        "03_pwm_fade.em",
        "04_analog_sensor.em",
        "05_uart_echo.em",
        "06_i2c_sensor.em",
        "07_state_machine.em",
        "08_bit_manipulation.em",
        "09_timer_watchdog.em",
        "10_floating_point.em",
    ];

    let mut all_valid = true;

    for example in &examples {
        let (valid, errors) = validate_example(example);
        if !valid {
            all_valid = false;
            eprintln!("\n=== {} ===", example);
            for e in &errors {
                eprintln!("  {}", e);
            }
        }
    }

    if !all_valid {
        eprintln!("\n=== VALIDATION SUMMARY ===");
        eprintln!("Some examples have validation errors.");
    }
}
