//! Integration tests for the embeem-wasm crate.

use embeem_ast::{
    AssignTarget, BinaryOp, Block, ElseBlock, Expression, Function, Literal, Param,
    PrimitiveType, RangeDirection, Statement, Type, UnaryOp,
};
use embeem_wasm::{generate_function_operators, FunctionResolver};
use wasmparser::{Operator, ValType};

/// Test function resolver that maps known functions to indices.
struct TestResolver;

impl FunctionResolver for TestResolver {
    fn resolve_function(&self, name: &str) -> Option<u32> {
        match name {
            "test_fn" => Some(0),
            "helper" => Some(1),
            _ => None,
        }
    }

    fn resolve_extern(&self, name: &str) -> Option<u32> {
        match name {
            "GPIO_READ" => Some(100),
            "GPIO_WRITE" => Some(101),
            "WRITE_GPIO" => Some(102),
            _ => None,
        }
    }
}

fn make_simple_func(body: Block) -> Function {
    Function {
        name: "test".into(),
        params: vec![],
        return_type: Some(Type::Primitive(PrimitiveType::I32)),
        body,
    }
}

fn make_func_with_params(params: Vec<Param>, body: Block) -> Function {
    Function {
        name: "test".into(),
        params,
        return_type: Some(Type::Primitive(PrimitiveType::I32)),
        body,
    }
}

#[test]
fn test_literal_integer() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Literal(Literal::Integer(42)))),
    });

    let resolver = TestResolver;
    let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    assert_eq!(locals.len(), 0);
    assert!(matches!(ops[0], Operator::I32Const { value: 42 }));
    assert!(matches!(ops[1], Operator::End));
}

#[test]
fn test_literal_large_integer() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Literal(Literal::Integer(
            0x1_0000_0000,
        )))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    assert!(matches!(ops[0], Operator::I64Const { value: 0x1_0000_0000 }));
}

#[test]
fn test_literal_float() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Literal(Literal::Float(3.14)))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should be f64.const
    match &ops[0] {
        Operator::F64Const { value } => {
            let f = f64::from_bits(value.bits());
            assert!((f - 3.14).abs() < 0.0001);
        }
        other => panic!("Expected F64Const, got {:?}", other),
    }
}

#[test]
fn test_literal_bool_true() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Literal(Literal::Bool(true)))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    assert!(matches!(ops[0], Operator::I32Const { value: 1 }));
}

#[test]
fn test_literal_bool_false() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Literal(Literal::Bool(false)))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    assert!(matches!(ops[0], Operator::I32Const { value: 0 }));
}

#[test]
fn test_binary_add() {
    let func = make_func_with_params(
        vec![
            Param {
                name: "a".into(),
                ty: Type::Primitive(PrimitiveType::I32),
            },
            Param {
                name: "b".into(),
                ty: Type::Primitive(PrimitiveType::I32),
            },
        ],
        Block {
            statements: vec![],
            result: Some(Box::new(Expression::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expression::Identifier("a".into())),
                right: Box::new(Expression::Identifier("b".into())),
            })),
        },
    );

    let resolver = TestResolver;
    let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    assert_eq!(locals.len(), 0); // Only additional locals, not params
    assert!(matches!(ops[0], Operator::LocalGet { local_index: 0 }));
    assert!(matches!(ops[1], Operator::LocalGet { local_index: 1 }));
    assert!(matches!(ops[2], Operator::I32Add));
    assert!(matches!(ops[3], Operator::End));
}

#[test]
fn test_binary_operators() {
    let test_cases = vec![
        (BinaryOp::Sub, "I32Sub"),
        (BinaryOp::Mul, "I32Mul"),
        (BinaryOp::Div, "I32DivS"),
        (BinaryOp::Mod, "I32RemS"),
        (BinaryOp::BitAnd, "I32And"),
        (BinaryOp::BitOr, "I32Or"),
        (BinaryOp::BitXor, "I32Xor"),
        (BinaryOp::Shl, "I32Shl"),
        (BinaryOp::Shr, "I32ShrS"),
        (BinaryOp::LogicalShr, "I32ShrU"),
        (BinaryOp::Eq, "I32Eq"),
        (BinaryOp::Ne, "I32Ne"),
        (BinaryOp::Lt, "I32LtS"),
        (BinaryOp::Le, "I32LeS"),
        (BinaryOp::Gt, "I32GtS"),
        (BinaryOp::Ge, "I32GeS"),
    ];

    for (op, expected_name) in test_cases {
        let func = make_simple_func(Block {
            statements: vec![],
            result: Some(Box::new(Expression::Binary {
                op,
                left: Box::new(Expression::Literal(Literal::Integer(1))),
                right: Box::new(Expression::Literal(Literal::Integer(2))),
            })),
        });

        let resolver = TestResolver;
        let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
        let ops: Vec<_> = iter.collect();

        // ops[0] = i32.const 1, ops[1] = i32.const 2, ops[2] = operator
        let op_str = format!("{:?}", ops[2]);
        assert!(
            op_str.starts_with(expected_name),
            "For {:?}, expected {} but got {}",
            op,
            expected_name,
            op_str
        );
    }
}

#[test]
fn test_local_variable() {
    let func = make_simple_func(Block {
        statements: vec![Statement::Let {
            name: "x".into(),
            mutable: false,
            ty: Some(Type::Primitive(PrimitiveType::I32)),
            value: Expression::Literal(Literal::Integer(10)),
        }],
        result: Some(Box::new(Expression::Identifier("x".into()))),
    });

    let resolver = TestResolver;
    let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have one additional local
    assert_eq!(locals.len(), 1);
    assert_eq!(locals[0], ValType::I32);

    // i32.const 10, local.set 0, local.get 0, end
    assert!(matches!(ops[0], Operator::I32Const { value: 10 }));
    assert!(matches!(ops[1], Operator::LocalSet { local_index: 0 }));
    assert!(matches!(ops[2], Operator::LocalGet { local_index: 0 }));
    assert!(matches!(ops[3], Operator::End));
}

#[test]
fn test_assignment() {
    let func = make_simple_func(Block {
        statements: vec![
            Statement::Let {
                name: "x".into(),
                mutable: true,
                ty: Some(Type::Primitive(PrimitiveType::I32)),
                value: Expression::Literal(Literal::Integer(10)),
            },
            Statement::Assign {
                target: AssignTarget::Identifier("x".into()),
                value: Expression::Literal(Literal::Integer(20)),
            },
        ],
        result: Some(Box::new(Expression::Identifier("x".into()))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // i32.const 10, local.set 0, i32.const 20, local.set 0, local.get 0, end
    assert!(matches!(ops[0], Operator::I32Const { value: 10 }));
    assert!(matches!(ops[1], Operator::LocalSet { local_index: 0 }));
    assert!(matches!(ops[2], Operator::I32Const { value: 20 }));
    assert!(matches!(ops[3], Operator::LocalSet { local_index: 0 }));
    assert!(matches!(ops[4], Operator::LocalGet { local_index: 0 }));
}

#[test]
fn test_if_statement() {
    let func = make_simple_func(Block {
        statements: vec![Statement::If {
            condition: Expression::Literal(Literal::Bool(true)),
            then_block: Block {
                statements: vec![],
                result: None,
            },
            else_block: None,
        }],
        result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // i32.const 1 (true), if, end, i32.const 0, end
    assert!(matches!(ops[0], Operator::I32Const { value: 1 }));
    assert!(matches!(ops[1], Operator::If { .. }));
    assert!(matches!(ops[2], Operator::End));
}

#[test]
fn test_if_else_statement() {
    let func = make_simple_func(Block {
        statements: vec![Statement::If {
            condition: Expression::Literal(Literal::Bool(true)),
            then_block: Block {
                statements: vec![],
                result: None,
            },
            else_block: Some(ElseBlock::Block(Block {
                statements: vec![],
                result: None,
            })),
        }],
        result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // i32.const 1, if, else, end, ...
    assert!(matches!(ops[0], Operator::I32Const { value: 1 }));
    assert!(matches!(ops[1], Operator::If { .. }));
    assert!(matches!(ops[2], Operator::Else));
    assert!(matches!(ops[3], Operator::End));
}

#[test]
fn test_for_loop() {
    let func = make_simple_func(Block {
        statements: vec![Statement::For {
            variable: "i".into(),
            start: Expression::Literal(Literal::Integer(0)),
            end: Expression::Literal(Literal::Integer(10)),
            direction: RangeDirection::To,
            body: Block {
                statements: vec![],
                result: None,
            },
        }],
        result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
    });

    let resolver = TestResolver;
    let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have one local for loop variable
    assert_eq!(locals.len(), 1);

    // Should contain block, loop, br_if, br, end, end
    let has_block = ops.iter().any(|o| matches!(o, Operator::Block { .. }));
    let has_loop = ops.iter().any(|o| matches!(o, Operator::Loop { .. }));
    let has_br_if = ops.iter().any(|o| matches!(o, Operator::BrIf { .. }));
    let has_br = ops.iter().any(|o| matches!(o, Operator::Br { .. }));

    assert!(has_block, "Missing block");
    assert!(has_loop, "Missing loop");
    assert!(has_br_if, "Missing br_if");
    assert!(has_br, "Missing br");
}

#[test]
fn test_repeat_loop() {
    let func = make_simple_func(Block {
        statements: vec![Statement::Repeat {
            count: Expression::Literal(Literal::Integer(5)),
            body: Block {
                statements: vec![],
                result: None,
            },
        }],
        result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
    });

    let resolver = TestResolver;
    let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have one local for counter
    assert_eq!(locals.len(), 1);

    // Should contain loop structure
    let has_loop = ops.iter().any(|o| matches!(o, Operator::Loop { .. }));
    assert!(has_loop, "Missing loop");
}

#[test]
fn test_while_loop() {
    let func = make_simple_func(Block {
        statements: vec![Statement::While {
            condition: Expression::Literal(Literal::Bool(true)),
            max_iterations: Expression::Literal(Literal::Integer(100)),
            body: Block {
                statements: vec![],
                result: None,
            },
        }],
        result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
    });

    let resolver = TestResolver;
    let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have one local for counter
    assert_eq!(locals.len(), 1);

    // Should contain loop structure
    let has_loop = ops.iter().any(|o| matches!(o, Operator::Loop { .. }));
    assert!(has_loop, "Missing loop");
}

#[test]
fn test_function_call() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Call {
            function: "helper".into(),
            args: vec![Expression::Literal(Literal::Integer(42))],
        })),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // i32.const 42, call 1, end
    assert!(matches!(ops[0], Operator::I32Const { value: 42 }));
    assert!(matches!(ops[1], Operator::Call { function_index: 1 }));
}

#[test]
fn test_undefined_function_error() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Call {
            function: "nonexistent".into(),
            args: vec![],
        })),
    });

    let resolver = TestResolver;
    let result = generate_function_operators(&func, &resolver);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("unresolved function"));
}

#[test]
fn test_undefined_variable_error() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Identifier("nonexistent".into()))),
    });

    let resolver = TestResolver;
    let result = generate_function_operators(&func, &resolver);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("undefined variable"));
}

#[test]
fn test_unary_not() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Unary {
            op: UnaryOp::Not,
            operand: Box::new(Expression::Literal(Literal::Bool(true))),
        })),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // i32.const 1 (true), i32.eqz, end
    assert!(matches!(ops[0], Operator::I32Const { value: 1 }));
    assert!(matches!(ops[1], Operator::I32Eqz));
}

#[test]
fn test_unary_bitnot() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Unary {
            op: UnaryOp::BitNot,
            operand: Box::new(Expression::Literal(Literal::Integer(0))),
        })),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // i32.const 0, i32.const -1, i32.xor, end
    assert!(matches!(ops[0], Operator::I32Const { value: 0 }));
    assert!(matches!(ops[1], Operator::I32Const { value: -1 }));
    assert!(matches!(ops[2], Operator::I32Xor));
}

#[test]
fn test_if_expression() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::If {
            condition: Box::new(Expression::Literal(Literal::Bool(true))),
            then_branch: Block {
                statements: vec![],
                result: Some(Box::new(Expression::Literal(Literal::Integer(1)))),
            },
            else_branch: Block {
                statements: vec![],
                result: Some(Box::new(Expression::Literal(Literal::Integer(2)))),
            },
        })),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have if with result type
    let has_typed_if = ops.iter().any(|o| {
        matches!(
            o,
            Operator::If {
                blockty: wasmparser::BlockType::Type(_)
            }
        )
    });
    assert!(has_typed_if, "Expected if with result type");
}

#[test]
fn test_operation_call() {
    let func = make_simple_func(Block {
        statements: vec![Statement::Expr(Expression::Operation {
            path: vec!["WRITE".into(), "GPIO".into()],
            extern_fn: None,
            args: vec![
                Expression::Literal(Literal::Integer(13)),
                Expression::Literal(Literal::Integer(1)),
            ],
        })],
        result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have call to WRITE_GPIO (index 102)
    let has_call = ops
        .iter()
        .any(|o| matches!(o, Operator::Call { function_index: 102 }));
    assert!(has_call, "Expected call to WRITE_GPIO");
}

#[test]
fn test_iterator_size_hint() {
    let func = make_simple_func(Block {
        statements: vec![],
        result: Some(Box::new(Expression::Literal(Literal::Integer(42)))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();

    let (lower, upper) = iter.size_hint();
    assert_eq!(lower, 2); // i32.const, end
    assert_eq!(upper, Some(2));
}

#[test]
fn test_expression_statement_drops_result() {
    let func = make_simple_func(Block {
        statements: vec![Statement::Expr(Expression::Literal(Literal::Integer(42)))],
        result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
    });

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // i32.const 42, drop, i32.const 0, end
    assert!(matches!(ops[0], Operator::I32Const { value: 42 }));
    assert!(matches!(ops[1], Operator::Drop));
}

#[test]
fn test_array_literal_splatted() {
    // Test that array literals are splatted into multiple values on the stack
    let func = Function {
        name: "test".into(),
        params: vec![],
        return_type: Some(Type::Primitive(PrimitiveType::I32)),
        body: Block {
            statements: vec![Statement::Let {
                name: "arr".into(),
                mutable: false,
                ty: Some(Type::Array(
                    Box::new(Type::Primitive(PrimitiveType::I32)),
                    3,
                )),
                value: Expression::Array(vec![
                    Expression::Literal(Literal::Integer(10)),
                    Expression::Literal(Literal::Integer(20)),
                    Expression::Literal(Literal::Integer(30)),
                ]),
            }],
            result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
        },
    };

    let resolver = TestResolver;
    let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have 3 additional locals for the array elements
    assert_eq!(locals.len(), 3);
    assert!(locals.iter().all(|t| *t == ValType::I32));

    // Should push 3 values then set 3 locals (in reverse order)
    assert!(matches!(ops[0], Operator::I32Const { value: 10 }));
    assert!(matches!(ops[1], Operator::I32Const { value: 20 }));
    assert!(matches!(ops[2], Operator::I32Const { value: 30 }));
    // Then local.set 2, local.set 1, local.set 0 (reverse order)
    assert!(matches!(ops[3], Operator::LocalSet { local_index: 2 }));
    assert!(matches!(ops[4], Operator::LocalSet { local_index: 1 }));
    assert!(matches!(ops[5], Operator::LocalSet { local_index: 0 }));
}

#[test]
fn test_array_index_read_splatted() {
    // Test that array indexing reads from splatted locals
    let func = Function {
        name: "test".into(),
        params: vec![Param {
            name: "arr".into(),
            ty: Type::Array(Box::new(Type::Primitive(PrimitiveType::I32)), 3),
        }],
        return_type: Some(Type::Primitive(PrimitiveType::I32)),
        body: Block {
            statements: vec![],
            result: Some(Box::new(Expression::Index {
                array: Box::new(Expression::Identifier("arr".into())),
                index: Box::new(Expression::Literal(Literal::Integer(1))),
            })),
        },
    };

    let resolver = TestResolver;
    let (iter, _locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // No additional locals beyond temps for index computation
    // (params are handled separately)
    
    // Should generate a switch-like structure for indexing
    // Check that we have local.get operations for each array element
    let local_gets: Vec<_> = ops
        .iter()
        .filter(|o| matches!(o, Operator::LocalGet { .. }))
        .collect();
    assert!(!local_gets.is_empty(), "Expected local.get operations for array access");
}

#[test]
fn test_array_index_write_splatted() {
    // Test that array index assignment writes to the correct splatted local
    let func = Function {
        name: "test".into(),
        params: vec![],
        return_type: Some(Type::Primitive(PrimitiveType::I32)),
        body: Block {
            statements: vec![
                Statement::Let {
                    name: "arr".into(),
                    mutable: true,
                    ty: Some(Type::Array(
                        Box::new(Type::Primitive(PrimitiveType::I32)),
                        3,
                    )),
                    value: Expression::Array(vec![
                        Expression::Literal(Literal::Integer(0)),
                        Expression::Literal(Literal::Integer(0)),
                        Expression::Literal(Literal::Integer(0)),
                    ]),
                },
                Statement::Assign {
                    target: AssignTarget::Index {
                        array: "arr".into(),
                        index: Box::new(Expression::Literal(Literal::Integer(1))),
                    },
                    value: Expression::Literal(Literal::Integer(42)),
                },
            ],
            result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
        },
    };

    let resolver = TestResolver;
    let (iter, locals) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have 3 locals for array + 2 temps (index and value)
    assert_eq!(locals.len(), 5);

    // Should have local.set operations for the assignment
    let local_sets: Vec<_> = ops
        .iter()
        .filter(|o| matches!(o, Operator::LocalSet { .. }))
        .collect();
    assert!(local_sets.len() >= 3, "Expected local.set operations for array assignment");
}

#[test]
fn test_tuple_splatted() {
    // Test that tuples are splatted into multiple locals
    let func = Function {
        name: "test".into(),
        params: vec![Param {
            name: "pair".into(),
            ty: Type::Tuple(vec![
                Type::Primitive(PrimitiveType::I32),
                Type::Primitive(PrimitiveType::I32),
            ]),
        }],
        return_type: Some(Type::Primitive(PrimitiveType::I32)),
        body: Block {
            statements: vec![],
            // Just return 0 for now - we're testing that locals are set up correctly
            result: Some(Box::new(Expression::Literal(Literal::Integer(0)))),
        },
    };

    let resolver = TestResolver;
    let (_, locals) = generate_function_operators(&func, &resolver).unwrap();

    // No additional locals beyond params, but params should be expanded
    // The param_local_count for a 2-element tuple should be 2
    assert_eq!(locals.len(), 0); // No additional locals, just params
}

#[test]
fn test_array_identifier_pushes_all_elements() {
    // Test that using an array identifier pushes all its splatted locals
    let func = Function {
        name: "test".into(),
        params: vec![
            Param {
                name: "arr".into(),
                ty: Type::Array(Box::new(Type::Primitive(PrimitiveType::I32)), 2),
            },
        ],
        return_type: Some(Type::Array(
            Box::new(Type::Primitive(PrimitiveType::I32)),
            2,
        )),
        body: Block {
            statements: vec![],
            result: Some(Box::new(Expression::Identifier("arr".into()))),
        },
    };

    let resolver = TestResolver;
    let (iter, _) = generate_function_operators(&func, &resolver).unwrap();
    let ops: Vec<_> = iter.collect();

    // Should have 2 local.get operations for the 2-element array
    let local_gets: Vec<_> = ops
        .iter()
        .filter(|o| matches!(o, Operator::LocalGet { .. }))
        .collect();
    assert_eq!(local_gets.len(), 2, "Expected 2 local.get for 2-element array");
    
    // Check they're getting locals 0 and 1
    assert!(matches!(ops[0], Operator::LocalGet { local_index: 0 }));
    assert!(matches!(ops[1], Operator::LocalGet { local_index: 1 }));
}
