//! AST types for the Embeem surface syntax.

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

/// A complete Embeem program.
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    /// Constant declarations.
    pub constants: Vec<ConstDecl>,
    /// External function declarations.
    pub extern_fns: Vec<ExternFn>,
    /// Function definitions.
    pub functions: Vec<Function>,
}

/// A constant declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct ConstDecl {
    /// Name of the constant.
    pub name: String,
    /// Type annotation.
    pub ty: Type,
    /// Value expression.
    pub value: Expression,
}

/// An external function declaration.
///
/// External functions are provided by the environment and called like regular functions.
/// They must be declared with their signature before use.
///
/// The totality of a program with external functions depends on the totality of those
/// external functions. See the TOTALITY documentation for details.
#[derive(Clone, Debug, PartialEq)]
pub struct ExternFn {
    /// Name of the external function.
    pub name: String,
    /// Parameters.
    pub params: Vec<Param>,
    /// Return type (optional, defaults to unit).
    pub return_type: Option<Type>,
}

/// A function definition.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    /// Name of the function.
    pub name: String,
    /// Parameters.
    pub params: Vec<Param>,
    /// Return type (optional, defaults to unit).
    pub return_type: Option<Type>,
    /// Function body.
    pub body: Block,
}

/// A function parameter.
#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub ty: Type,
}

/// A type annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// Primitive type (u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, bool).
    Primitive(PrimitiveType),
    /// Fixed-size array type.
    Array(Box<Type>, u64),
    /// Tuple type.
    Tuple(Vec<Type>),
}

/// Primitive types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
}

/// A block of statements with an optional result expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    /// Statements in the block.
    pub statements: Vec<Statement>,
    /// Optional final expression (the block's value).
    pub result: Option<Box<Expression>>,
}

/// A statement.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    /// Let binding.
    Let {
        name: String,
        mutable: bool,
        ty: Option<Type>,
        value: Expression,
    },
    /// Assignment.
    Assign {
        target: String,
        value: Expression,
    },
    /// Expression statement.
    Expr(Expression),
    /// If statement.
    If {
        condition: Expression,
        then_block: Block,
        else_block: Option<ElseBlock>,
    },
    /// For loop.
    For {
        variable: String,
        start: Expression,
        end: Expression,
        direction: RangeDirection,
        body: Block,
    },
    /// Repeat loop.
    Repeat {
        count: Expression,
        body: Block,
    },
    /// Bounded while loop.
    While {
        condition: Expression,
        max_iterations: Expression,
        body: Block,
    },
}

/// Direction of a for loop range.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RangeDirection {
    /// Ascending (to).
    To,
    /// Descending (downto).
    DownTo,
}

/// Else block can be either a block or an if statement.
#[derive(Clone, Debug, PartialEq)]
pub enum ElseBlock {
    /// Plain else block.
    Block(Block),
    /// Else-if chain.
    ElseIf(Box<Statement>),
}

/// An expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    /// Literal value.
    Literal(Literal),
    /// Identifier reference.
    Identifier(String),
    /// Binary operation.
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// Unary operation.
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
    },
    /// If expression.
    If {
        condition: Box<Expression>,
        then_branch: Block,
        else_branch: Block,
    },
    /// Operation call (e.g., GPIO_READ(pin) or WRITE(GPIO(pin), value)).
    ///
    /// Operations are represented as a path of UPPER_SNAKE_CASE segments.
    /// - `FSUB(a, b)` is stored as `path: ["FSUB"], args: [a, b]`
    /// - `WRITE(GPIO(pin), value)` is stored as `path: ["WRITE", "GPIO"], args: [pin, value]`
    /// - Deeper nesting is supported: `A(B(C(x), y), z)` -> `path: ["A", "B", "C"], args: [x, y, z]`
    ///
    /// Hybrid operations with external functions:
    /// - `WRITE(GPIO(my_sensor(ch)), value)` where `my_sensor` is an extern fn
    /// - Stored as `path: ["WRITE", "GPIO"], extern_fn: Some("my_sensor"), args: [ch, value]`
    /// - The extern fn call is semantically part of the operation for readability
    Operation {
        path: Vec<String>,
        /// Optional external function as the final "segment" of the operation path.
        /// When present, the operation is a "hybrid" that combines an operation path
        /// with an external function call for improved readability.
        extern_fn: Option<String>,
        args: Vec<Expression>,
    },
    /// Function call.
    Call {
        function: String,
        args: Vec<Expression>,
    },
    /// Block expression.
    Block(Block),
    /// Array index.
    Index {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    /// Array literal.
    Array(Vec<Expression>),
    /// Type cast.
    Cast {
        value: Box<Expression>,
        ty: Type,
    },
}

/// A literal value.
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    /// Integer literal.
    Integer(u64),
    /// Float literal.
    Float(f64),
    /// Boolean literal.
    Bool(bool),
}

/// Binary operators.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    LogicalShr,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical
    And,
    Or,
}

/// Unary operators.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    /// Negation (-).
    Neg,
    /// Bitwise not (~).
    BitNot,
    /// Logical not.
    Not,
}
