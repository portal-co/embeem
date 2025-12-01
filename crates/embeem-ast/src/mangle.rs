//! Name mangling for Embeem operations and functions.
//!
//! This module provides the mangling scheme used to convert Embeem operation paths
//! and function names into backend-specific identifiers.
//!
//! # Operation Mangling Scheme
//!
//! Operations in Embeem are represented as paths of UPPER_SNAKE_CASE segments.
//! These paths are mangled into identifiers using a length-prefixed encoding scheme:
//!
//! 1. Start with the appropriate prefix (op_prefix or extern_prefix)
//! 2. Append `$` and the number of path segments
//! 3. For each segment: `_` + length + `_` + segment name
//! 4. For hybrid operations: `_` + extern function name
//!
//! ## Examples
//!
//! | Embeem Syntax | Path | Mangled Name (default prefix) |
//! |---------------|------|-------------------------------|
//! | `FSUB(a, b)` | `["FSUB"]` | `embeem_op_$1_4_FSUB` |
//! | `GPIO_READ(pin)` | `["GPIO_READ"]` | `embeem_op_$1_9_GPIO_READ` |
//! | `WRITE(GPIO(pin), val)` | `["WRITE", "GPIO"]` | `embeem_op_$2_5_WRITE_4_GPIO` |
//! | `READ(ADC(ch))` | `["READ", "ADC"]` | `embeem_op_$2_4_READ_3_ADC` |
//! | Hybrid with extern fn | `["WRITE", "GPIO"]` + `sensor_read` | `embeem_extern_$2_5_WRITE_4_GPIO_sensor_read` |
//!
//! This scheme is unambiguous and allows the original path to be decoded.

use alloc::format;
use alloc::string::{String, ToString};

/// Configuration for name mangling.
#[derive(Clone, Debug)]
pub struct MangleConfig {
    /// Prefix for user-defined function names.
    /// Default: `"embeem_"`
    pub fn_prefix: String,
    /// Prefix for non-hybrid operation function names.
    /// Default: `"embeem_op_"`
    pub op_prefix: String,
    /// Prefix for external functions and hybrid operations.
    /// Default: `"embeem_extern_"`
    pub extern_prefix: String,
    /// Prefix for module-scoped items.
    /// Default: `"embeem_mod_"`
    pub mod_prefix: String,
}

impl Default for MangleConfig {
    fn default() -> Self {
        Self {
            fn_prefix: "embeem_".to_string(),
            op_prefix: "embeem_op_".to_string(),
            extern_prefix: "embeem_extern_".to_string(),
            mod_prefix: "embeem_mod_".to_string(),
        }
    }
}

impl MangleConfig {
    /// Create a new mangle configuration with default prefixes.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the function name prefix.
    pub fn with_fn_prefix(mut self, prefix: &str) -> Self {
        self.fn_prefix = prefix.to_string();
        self
    }

    /// Set the operation function prefix.
    pub fn with_op_prefix(mut self, prefix: &str) -> Self {
        self.op_prefix = prefix.to_string();
        self
    }

    /// Set the external function and hybrid operation prefix.
    pub fn with_extern_prefix(mut self, prefix: &str) -> Self {
        self.extern_prefix = prefix.to_string();
        self
    }

    /// Set the module prefix.
    pub fn with_mod_prefix(mut self, prefix: &str) -> Self {
        self.mod_prefix = prefix.to_string();
        self
    }
}

/// Mangle a user-defined function name.
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, mangle_function_name};
///
/// let config = MangleConfig::default();
/// assert_eq!(mangle_function_name("main", &config), "embeem_main");
/// assert_eq!(mangle_function_name("blink_led", &config), "embeem_blink_led");
/// ```
pub fn mangle_function_name(name: &str, config: &MangleConfig) -> String {
    format!("{}{}", config.fn_prefix, name)
}

/// Mangle an external function name.
///
/// External functions use the extern_prefix.
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, mangle_extern_function_name};
///
/// let config = MangleConfig::default();
/// assert_eq!(mangle_extern_function_name("sensor_read", &config), "embeem_extern_sensor_read");
/// ```
pub fn mangle_extern_function_name(name: &str, config: &MangleConfig) -> String {
    format!("{}{}", config.extern_prefix, name)
}

/// Mangle a module path into a prefix string.
///
/// Module paths are encoded using length-prefixed segments:
/// 1. Start with mod_prefix (default: `embeem_mod_`)
/// 2. Append `$` and number of path segments
/// 3. For each segment: `_` + length + `_` + segment name
///
/// # Arguments
/// * `module_path` - The module path segments (e.g., `["utils", "gpio"]`)
/// * `config` - Mangling configuration
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, mangle_module_path};
///
/// let config = MangleConfig::default();
///
/// // Single segment path
/// assert_eq!(
///     mangle_module_path(&["gpio".to_string()], &config),
///     "embeem_mod_$1_4_gpio"
/// );
///
/// // Multi-segment path
/// assert_eq!(
///     mangle_module_path(&["utils".to_string(), "gpio".to_string()], &config),
///     "embeem_mod_$2_5_utils_4_gpio"
/// );
/// ```
pub fn mangle_module_path(module_path: &[String], config: &MangleConfig) -> String {
    if module_path.is_empty() {
        return String::new();
    }

    let mut result = format!("{}${}", config.mod_prefix, module_path.len());
    for segment in module_path {
        result.push_str(&format!("_{}_", segment.len()));
        result.push_str(segment);
    }
    result
}

/// Mangle a function name with its module path.
///
/// For the root/main module (empty path), uses simple function mangling.
/// For other modules, prepends the module path prefix.
///
/// # Arguments
/// * `module_path` - The module path segments (empty for root module)
/// * `name` - The function name
/// * `config` - Mangling configuration
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, mangle_module_function_name};
///
/// let config = MangleConfig::default();
///
/// // Root module function
/// assert_eq!(
///     mangle_module_function_name(&[], "main", &config),
///     "embeem_main"
/// );
///
/// // Module function
/// assert_eq!(
///     mangle_module_function_name(&["gpio".to_string()], "blink", &config),
///     "embeem_mod_$1_4_gpio_blink"
/// );
///
/// // Nested module function
/// assert_eq!(
///     mangle_module_function_name(&["drivers".to_string(), "bme280".to_string()], "init", &config),
///     "embeem_mod_$2_7_drivers_6_bme280_init"
/// );
/// ```
pub fn mangle_module_function_name(module_path: &[String], name: &str, config: &MangleConfig) -> String {
    if module_path.is_empty() {
        // Root module uses simple function mangling
        mangle_function_name(name, config)
    } else {
        // Non-root module uses module prefix + function name
        let module_prefix = mangle_module_path(module_path, config);
        format!("{}_{}", module_prefix, name)
    }
}

/// Mangle a constant name with its module path.
///
/// # Arguments
/// * `module_path` - The module path segments (empty for root module)
/// * `name` - The constant name
/// * `config` - Mangling configuration
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, mangle_module_constant_name};
///
/// let config = MangleConfig::default();
///
/// // Root module constant - just the name (for #define)
/// assert_eq!(
///     mangle_module_constant_name(&[], "LED_PIN", &config),
///     "LED_PIN"
/// );
///
/// // Module constant
/// assert_eq!(
///     mangle_module_constant_name(&["gpio".to_string()], "LED_PIN", &config),
///     "embeem_mod_$1_4_gpio_LED_PIN"
/// );
/// ```
pub fn mangle_module_constant_name(module_path: &[String], name: &str, config: &MangleConfig) -> String {
    if module_path.is_empty() {
        // Root module constants use the name directly
        name.to_string()
    } else {
        let module_prefix = mangle_module_path(module_path, config);
        format!("{}_{}", module_prefix, name)
    }
}

/// Mangle an external function name with its module path.
///
/// # Arguments
/// * `module_path` - The module path segments (empty for root module)
/// * `name` - The external function name
/// * `config` - Mangling configuration
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, mangle_module_extern_function_name};
///
/// let config = MangleConfig::default();
///
/// // Root module extern fn
/// assert_eq!(
///     mangle_module_extern_function_name(&[], "sensor_read", &config),
///     "embeem_extern_sensor_read"
/// );
///
/// // Module extern fn
/// assert_eq!(
///     mangle_module_extern_function_name(&["drivers".to_string()], "sensor_read", &config),
///     "embeem_extern_mod_$1_7_drivers_sensor_read"
/// );
/// ```
pub fn mangle_module_extern_function_name(module_path: &[String], name: &str, config: &MangleConfig) -> String {
    if module_path.is_empty() {
        mangle_extern_function_name(name, config)
    } else {
        // For module extern fns, combine extern prefix with module path
        let mut result = format!("{}mod_${}", config.extern_prefix, module_path.len());
        for segment in module_path {
            result.push_str(&format!("_{}_", segment.len()));
            result.push_str(segment);
        }
        result.push('_');
        result.push_str(name);
        result
    }
}

/// Demangle a module path from a mangled identifier.
///
/// Returns `None` if the identifier doesn't match the expected format.
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, demangle_module_path};
///
/// let config = MangleConfig::default();
///
/// let path = demangle_module_path("embeem_mod_$2_5_utils_4_gpio", &config).unwrap();
/// assert_eq!(path, vec!["utils", "gpio"]);
/// ```
pub fn demangle_module_path(mangled: &str, config: &MangleConfig) -> Option<alloc::vec::Vec<String>> {
    use alloc::vec::Vec;

    let rest = mangled.strip_prefix(&config.mod_prefix)?;

    // Parse the segment count: $N_...
    let rest = rest.strip_prefix('$')?;
    let underscore_pos = rest.find('_')?;
    let segment_count: usize = rest[..underscore_pos].parse().ok()?;

    let mut remaining = &rest[underscore_pos..];
    let mut path = Vec::with_capacity(segment_count);

    // Parse each segment: _LEN_NAME
    for _ in 0..segment_count {
        remaining = remaining.strip_prefix('_')?;
        let underscore_pos = remaining.find('_')?;
        let len: usize = remaining[..underscore_pos].parse().ok()?;
        remaining = &remaining[underscore_pos + 1..];

        if remaining.len() < len {
            return None;
        }
        path.push(remaining[..len].to_string());
        remaining = &remaining[len..];
    }

    Some(path)
}

/// Mangle an operation path into an identifier.
///
/// The mangling scheme uses length-prefixed encoding:
/// 1. Start with op_prefix (for non-hybrid) or extern_prefix (for hybrid)
/// 2. Append `$` and number of path segments
/// 3. For each segment: `_` + length + `_` + segment name
/// 4. For hybrid operations: `_` + extern function name
///
/// # Arguments
/// * `path` - The operation path segments (e.g., `["WRITE", "GPIO"]`)
/// * `extern_fn` - Optional extern function name for hybrid operations
/// * `config` - Mangling configuration
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, mangle_operation_path};
///
/// let config = MangleConfig::default();
///
/// // Non-hybrid operation
/// assert_eq!(
///     mangle_operation_path(&["FSUB".to_string()], None, &config),
///     "embeem_op_$1_4_FSUB"
/// );
///
/// // Multi-segment operation
/// assert_eq!(
///     mangle_operation_path(&["WRITE".to_string(), "GPIO".to_string()], None, &config),
///     "embeem_op_$2_5_WRITE_4_GPIO"
/// );
///
/// // Hybrid operation with extern function
/// assert_eq!(
///     mangle_operation_path(&["WRITE".to_string(), "GPIO".to_string()], Some("sensor_read"), &config),
///     "embeem_extern_$2_5_WRITE_4_GPIO_sensor_read"
/// );
/// ```
pub fn mangle_operation_path(path: &[String], extern_fn: Option<&str>, config: &MangleConfig) -> String {
    // Choose prefix based on whether this is a hybrid operation
    let prefix = if extern_fn.is_some() {
        &config.extern_prefix
    } else {
        &config.op_prefix
    };

    let mut result = format!("{}${}", prefix, path.len());
    for segment in path {
        result.push_str(&format!("_{}_", segment.len()));
        result.push_str(segment);
    }
    if let Some(fn_name) = extern_fn {
        result.push('_');
        result.push_str(fn_name);
    }
    result
}

/// Demangle an operation path from a mangled identifier.
///
/// Returns `None` if the identifier doesn't match the expected format.
///
/// # Example
/// ```
/// use embeem_ast::mangle::{MangleConfig, demangle_operation_path};
///
/// let config = MangleConfig::default();
///
/// let (path, extern_fn) = demangle_operation_path("embeem_op_$2_5_WRITE_4_GPIO", &config).unwrap();
/// assert_eq!(path, vec!["WRITE", "GPIO"]);
/// assert_eq!(extern_fn, None);
///
/// let (path, extern_fn) = demangle_operation_path("embeem_extern_$2_5_WRITE_4_GPIO_sensor_read", &config).unwrap();
/// assert_eq!(path, vec!["WRITE", "GPIO"]);
/// assert_eq!(extern_fn, Some("sensor_read".to_string()));
/// ```
pub fn demangle_operation_path(mangled: &str, config: &MangleConfig) -> Option<(alloc::vec::Vec<String>, Option<String>)> {
    use alloc::vec::Vec;

    // Determine which prefix was used
    let (rest, is_hybrid) = if let Some(rest) = mangled.strip_prefix(&config.extern_prefix) {
        (rest, true)
    } else if let Some(rest) = mangled.strip_prefix(&config.op_prefix) {
        (rest, false)
    } else {
        return None;
    };

    // Parse the segment count: $N_...
    let rest = rest.strip_prefix('$')?;
    let underscore_pos = rest.find('_')?;
    let segment_count: usize = rest[..underscore_pos].parse().ok()?;

    let mut remaining = &rest[underscore_pos..];
    let mut path = Vec::with_capacity(segment_count);

    // Parse each segment: _LEN_NAME
    for _ in 0..segment_count {
        remaining = remaining.strip_prefix('_')?;
        let underscore_pos = remaining.find('_')?;
        let len: usize = remaining[..underscore_pos].parse().ok()?;
        remaining = &remaining[underscore_pos + 1..];

        if remaining.len() < len {
            return None;
        }
        path.push(remaining[..len].to_string());
        remaining = &remaining[len..];
    }

    // Parse optional extern function name for hybrid operations
    let extern_fn = if is_hybrid && !remaining.is_empty() {
        let fn_name = remaining.strip_prefix('_')?;
        Some(fn_name.to_string())
    } else {
        None
    };

    Some((path, extern_fn))
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

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
}
