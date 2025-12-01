# Embeem Versioning Policy

This document describes the semantic versioning policy for both the Embeem language specification and programs written in Embeem.

## Version Format

Embeem follows [Semantic Versioning 2.0.0](https://semver.org/):

```
MAJOR.MINOR.PATCH
```

Where:
- **MAJOR**: Incompatible changes that may break existing programs
- **MINOR**: New features added in a backward-compatible manner
- **PATCH**: Backward-compatible bug fixes

## Language Specification Versioning

### Pre-1.0 Releases (0.x.y)

During pre-1.0 development, the language is considered unstable:

| Change Type | Version Bump | Notes |
|-------------|--------------|-------|
| New operations added | **PATCH** | e.g., 0.1.0 → 0.1.1 |
| Operations **removed** | **PATCH** | e.g., 0.1.0 → 0.1.2 |
| Syntax additions | MINOR | New keywords, constructs |
| Breaking syntax changes | MINOR | During 0.x only |
| Bug fixes | PATCH | Clarifications, typos |
| Semantic clarifications | PATCH | |

**Important**: Operations may be added or removed in pre-1.0 **patch** releases. This allows rapid iteration on the operation set before stabilization.

### Post-1.0 Releases (1.x.y and beyond)

After 1.0, the language becomes stable:

| Change Type | Version Bump | Notes |
|-------------|--------------|-------|
| New operations added | MINOR | Backward compatible |
| Operations removed | **MAJOR** | Breaking change |
| Deprecation of operations | MINOR | With warning period |
| New syntax (backward compatible) | MINOR | |
| Breaking syntax changes | MAJOR | |
| Bug fixes | PATCH | |

## Program Compatibility

### Source Compatibility

An Embeem program is **source compatible** with a language version if:
1. It parses successfully under that version's grammar
2. All operations it uses are defined in that version
3. All semantic rules are satisfied

### Forward Compatibility

Programs written for Embeem version X.Y.Z are guaranteed to work on:
- **Post-1.0**: Any version ≥ X.Y.Z with the same major version
- **Pre-1.0**: Not guaranteed (operations may be removed in patches)

### Backward Compatibility

Newer language versions should parse and run programs from:
- **Same major version**: Always (post-1.0)
- **Earlier minor versions**: Always (post-1.0)
- **Pre-1.0 versions**: Best effort, no guarantees

## Reserved Namespaces

### UPPER_SNAKE_CASE Operations

All identifiers matching `[A-Z][A-Z0-9_]*` are reserved for operations:

- **New operations**: May be added in any MINOR release
- **Existing programs**: Using reserved identifiers will fail to parse on upgrade
- **User code**: Must not define identifiers in this namespace

This reservation ensures:
1. Operations can be added without breaking user variable/function names
2. Clear visual distinction between operations and user code
3. Parser can unambiguously identify operation calls

### Keywords

New keywords may be added in:
- **Pre-1.0**: Any MINOR release
- **Post-1.0**: Any MINOR release (with appropriate deprecation period for conflicting identifiers)

## Deprecation Policy

### Pre-1.0

No formal deprecation period required. Operations may be removed in patch releases.

### Post-1.0

1. **Deprecation notice**: Operation marked deprecated in MINOR release
2. **Warning period**: At least one MINOR release cycle
3. **Removal**: In next MAJOR release

Example timeline:
```
1.2.0 - GPIO_LEGACY_READ deprecated, warning emitted
1.3.0 - Continued deprecation warnings
2.0.0 - GPIO_LEGACY_READ removed
```

## Version Declaration

### In Programs

Programs should declare their minimum required version:

```embeem
#![embeem(version = "0.1")]

fn main() {
    // ...
}
```

Or in a manifest file:

```toml
[package]
embeem = "0.1"
```

### In Tooling

Compilers and tools should:
1. Report their supported Embeem version
2. Warn on deprecated operations
3. Error on removed operations

## Implementation Crate Versioning

The `embeem-ast` crate follows Rust's semver conventions:

| Crate Change | Version Bump |
|--------------|--------------|
| New AST nodes | MINOR |
| Changed AST structure | MAJOR |
| New validation functions | MINOR |
| Bug fixes | PATCH |

The crate version may differ from the language specification version.

## Examples

### Adding a New Operation (Pre-1.0)

**Version**: 0.1.0 → 0.2.0

```
+ Added: SPI_TRANSFER_DMA(channel, buffer)
```

Programs using the new operation require 0.2.0 or later.

### Removing an Operation (Pre-1.0)

**Version**: 0.1.0 → 0.1.1

```
- Removed: LEGACY_DELAY(ms)
  Migration: Use DELAY_MS(ms) instead
```

Programs using `LEGACY_DELAY` must be updated.

### Adding an Operation (Post-1.0)

**Version**: 1.2.0 → 1.3.0

```
+ Added: USB_BULK_TRANSFER(endpoint, buffer, size)
```

All 1.2.x programs continue to work on 1.3.0.

### Removing an Operation (Post-1.0)

**Version**: 1.3.0 → 1.4.0 → 2.0.0

```
1.4.0:
  ! Deprecated: OLD_TIMER_API(timer)
    Use TIMER_START(timer) instead
    
2.0.0:
  - Removed: OLD_TIMER_API(timer)
```

## Changelog Requirements

Each release must document:

1. **Added**: New operations, syntax, features
2. **Changed**: Modified behavior (with migration guide)
3. **Deprecated**: Operations scheduled for removal
4. **Removed**: Operations no longer available
5. **Fixed**: Bug fixes
6. **Security**: Security-related changes

## Summary

| Aspect | Pre-1.0 | Post-1.0 |
|--------|---------|----------|
| Operation additions | MINOR | MINOR |
| Operation removals | PATCH | MAJOR |
| Breaking changes | MINOR | MAJOR |
| Deprecation period | None required | Required |
| Forward compatibility | Not guaranteed | Guaranteed within major |
