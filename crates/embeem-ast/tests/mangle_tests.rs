extern crate alloc;

use alloc::vec;
use alloc::string::ToString;
use embeem_ast::mangle::*;

#[test]
fn test_mangle_function_name() {
    let config = MangleConfig::default();
    assert_eq!(mangle_function_name("main", &config), "embeem_main");
    assert_eq!(mangle_function_name("blink_led", &config), "embeem_blink_led");
}

#[test]
fn test_mangle_extern_function_name() {
    let config = MangleConfig::default();
    assert_eq!(mangle_extern_function_name("sensor_read", &config), "embeem_extern_sensor_read");
}

#[test]
fn test_mangle_single_segment() {
    let config = MangleConfig::default();
    assert_eq!(
        mangle_operation_path(&["FSUB".to_string()], None, &config),
        "embeem_op_$1_4_FSUB"
    );
    assert_eq!(
        mangle_operation_path(&["GPIO_READ".to_string()], None, &config),
        "embeem_op_$1_9_GPIO_READ"
    );
}

#[test]
fn test_mangle_multi_segment() {
    let config = MangleConfig::default();
    assert_eq!(
        mangle_operation_path(&["WRITE".to_string(), "GPIO".to_string()], None, &config),
        "embeem_op_$2_5_WRITE_4_GPIO"
    );
    assert_eq!(
        mangle_operation_path(&["READ".to_string(), "ADC".to_string()], None, &config),
        "embeem_op_$2_4_READ_3_ADC"
    );
    assert_eq!(
        mangle_operation_path(&["A".to_string(), "B".to_string(), "C".to_string()], None, &config),
        "embeem_op_$3_1_A_1_B_1_C"
    );
}

#[test]
fn test_mangle_hybrid() {
    let config = MangleConfig::default();
    assert_eq!(
        mangle_operation_path(&["WRITE".to_string(), "GPIO".to_string()], Some("sensor_read"), &config),
        "embeem_extern_$2_5_WRITE_4_GPIO_sensor_read"
    );
}

#[test]
fn test_demangle_single_segment() {
    let config = MangleConfig::default();
    let (path, extern_fn) = demangle_operation_path("embeem_op_$1_4_FSUB", &config).unwrap();
    assert_eq!(path, vec!["FSUB"]);
    assert_eq!(extern_fn, None);
}

#[test]
fn test_demangle_multi_segment() {
    let config = MangleConfig::default();
    let (path, extern_fn) = demangle_operation_path("embeem_op_$2_5_WRITE_4_GPIO", &config).unwrap();
    assert_eq!(path, vec!["WRITE", "GPIO"]);
    assert_eq!(extern_fn, None);
}

#[test]
fn test_demangle_hybrid() {
    let config = MangleConfig::default();
    let (path, extern_fn) = demangle_operation_path("embeem_extern_$2_5_WRITE_4_GPIO_sensor_read", &config).unwrap();
    assert_eq!(path, vec!["WRITE", "GPIO"]);
    assert_eq!(extern_fn, Some("sensor_read".to_string()));
}

#[test]
fn test_roundtrip() {
    let config = MangleConfig::default();

    // Non-hybrid
    let path = vec!["SET".to_string(), "FREQUENCY".to_string(), "PWM".to_string()];
    let mangled = mangle_operation_path(&path, None, &config);
    let (demangled_path, extern_fn) = demangle_operation_path(&mangled, &config).unwrap();
    assert_eq!(demangled_path, path);
    assert_eq!(extern_fn, None);

    // Hybrid
    let path = vec!["WRITE".to_string(), "GPIO".to_string()];
    let mangled = mangle_operation_path(&path, Some("get_value"), &config);
    let (demangled_path, extern_fn) = demangle_operation_path(&mangled, &config).unwrap();
    assert_eq!(demangled_path, path);
    assert_eq!(extern_fn, Some("get_value".to_string()));
}

#[test]
fn test_custom_prefix() {
    let config = MangleConfig::new()
        .with_fn_prefix("my_")
        .with_op_prefix("my_op_")
        .with_extern_prefix("my_ext_");

    assert_eq!(mangle_function_name("test", &config), "my_test");
    assert_eq!(
        mangle_operation_path(&["READ".to_string()], None, &config),
        "my_op_$1_4_READ"
    );
    assert_eq!(
        mangle_operation_path(&["READ".to_string()], Some("fn"), &config),
        "my_ext_$1_4_READ_fn"
    );
}

#[test]
fn test_mangle_module_path() {
    let config = MangleConfig::default();

    // Single segment
    assert_eq!(
        mangle_module_path(&["gpio".to_string()], &config),
        "embeem_mod_$1_4_gpio"
    );

    // Multi segment
    assert_eq!(
        mangle_module_path(&["utils".to_string(), "gpio".to_string()], &config),
        "embeem_mod_$2_5_utils_4_gpio"
    );

    // Empty path returns empty string
    assert_eq!(mangle_module_path(&[], &config), "");
}

#[test]
fn test_mangle_module_function_name() {
    let config = MangleConfig::default();

    // Root module
    assert_eq!(
        mangle_module_function_name(&[], "main", &config),
        "embeem_main"
    );

    // Single segment module
    assert_eq!(
        mangle_module_function_name(&["gpio".to_string()], "blink", &config),
        "embeem_mod_$1_4_gpio_blink"
    );

    // Multi segment module
    assert_eq!(
        mangle_module_function_name(&["drivers".to_string(), "bme280".to_string()], "init", &config),
        "embeem_mod_$2_7_drivers_6_bme280_init"
    );
}

#[test]
fn test_mangle_module_constant_name() {
    let config = MangleConfig::default();

    // Root module constant
    assert_eq!(
        mangle_module_constant_name(&[], "LED_PIN", &config),
        "LED_PIN"
    );

    // Module constant
    assert_eq!(
        mangle_module_constant_name(&["gpio".to_string()], "LED_PIN", &config),
        "embeem_mod_$1_4_gpio_LED_PIN"
    );
}

#[test]
fn test_demangle_module_path() {
    let config = MangleConfig::default();

    let path = demangle_module_path("embeem_mod_$2_5_utils_4_gpio", &config).unwrap();
    assert_eq!(path, vec!["utils", "gpio"]);

    let path = demangle_module_path("embeem_mod_$1_4_gpio", &config).unwrap();
    assert_eq!(path, vec!["gpio"]);
}

#[test]
fn test_module_path_roundtrip() {
    let config = MangleConfig::default();

    let path = vec!["drivers".to_string(), "sensors".to_string(), "bme280".to_string()];
    let mangled = mangle_module_path(&path, &config);
    let demangled = demangle_module_path(&mangled, &config).unwrap();
    assert_eq!(demangled, path);
}
