use crate::parser::{Expression, Function, Statement, Type};

use super::scanner::Span;

pub fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Any => "Any".to_string(),
        Type::Named(path) => path.borrow().join("::"),
        Type::Function { params, ret, .. } => {
            let params = params
                .iter()
                .map(|x| type_to_string(&x.value))
                .collect::<Vec<_>>()
                .join(", ");
            let ret = type_to_string(&ret.value);
            format!("fn({}) -> {}", params, ret)
        }
        Type::Generic { name, fields } => {
            let fields = fields
                .iter()
                .map(|x| type_to_string(&x.value))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", name.value, fields)
        }
    }
}

pub fn print_statement(statement: &Span<Statement>, str: &str, depth: usize) {
    let padding = "  ".repeat(depth);

    match &statement.value {
        Statement::Declaration {
            name,
            is_mutable,
            ty: _,
            value,
        } => {
            let mutability = if is_mutable.value { "mut " } else { "" };
            println!("{padding}Declaration: {mutability}{} = ", name.value);
            print_math(value, str, depth);
        }
        Statement::Assignment { name, value } => {
            println!("{padding}Assignment: {} = ", name.value);
            print_math(value, str, depth);
        }
        Statement::Expression(expr) => {
            println!("{padding}Expression:");
            print_math(expr, str, depth);
        }

        Statement::Function(Function {
            path: Span { value: path, .. },
            params,
            body: block,
            return_type,
            function_kind: _,
        }) => {
            println!("{padding}Function: {path:?}");

            let padding = "  ".repeat(depth + 1);

            let no_params = if params.len() > 0 { "" } else { " <None>" };
            println!("{padding}Params:{no_params}");
            for param in params {
                let padding = "  ".repeat(depth + 2);
                println!(
                    "{padding}{}: {}",
                    param.value.name.value,
                    type_to_string(
                        &param
                            .value
                            .ty
                            .clone()
                            .unwrap_or_else(|| Span::default(Type::Any))
                            .value
                    )
                );
            }

            if let Some(ret) = return_type {
                println!("{padding}Returns: {}", type_to_string(&ret.value));
            }

            for statement in &block.value.statements {
                print_statement(&statement, str, depth + 1);
            }

            if let Some(ret) = &block.value.ret {
                println!("{padding}Block:");
                let padding = "  ".repeat(depth + 2);
                println!("{padding}Return:");
                print_math(&ret, str, depth + 3);
            } else {
                println!("{padding}Block: <None>");
            }
        }

        Statement::Struct { name, fields } => {
            println!("{padding}Struct: {}", name.value);

            let padding = "  ".repeat(depth + 1);

            let no_fields = if fields.len() > 0 { "" } else { " <None>" };
            println!("{padding}Fields:{no_fields}");
            for field in fields {
                let padding = "  ".repeat(depth + 2);
                println!(
                    "{padding}{}: {}",
                    field.value.name.value,
                    type_to_string(
                        &field
                            .value
                            .ty
                            .clone()
                            .unwrap_or_else(|| Span::default(Type::Any))
                            .value
                    )
                );
            }
        }

        #[allow(unreachable_patterns)]
        _ => {
            println!("{padding}Unknown statement");
        }
    }
}
pub fn print_math(expr: &Span<Expression>, str: &str, depth: usize) {
    let padding = "  ".repeat(depth);

    match &expr.value {
        //     Expression::Int(int) => {
        //         println!("{padding}Int: {int}");
        //     }
        //     Expression::Float(float) => {
        //         println!("{padding}Float: {float}");
        //     }
        //     Expression::Symbol(symbol) => {
        //         println!("{padding}Symbol: {symbol}");
        //     }
        // Expression::String(string) => {
        //         println!("{padding}String: {string:?}");
        //     }
        Expression::FunctionCall { func, args } => {
            println!("{padding}FunctionCall:");

            print_math(func, str, depth + 1);
            let no_args = if args.len() > 0 { "" } else { " <None>" };
            println!("{padding}  Args:{no_args}");
            for arg in args {
                print_math(arg, str, depth + 2);
            }
        }
        Expression::Binary { op, lhs, rhs } => {
            println!("{padding}{op:?}:");

            print_math(lhs, str, depth + 1);
            print_math(rhs, str, depth + 1);
        }
        Expression::Parentheses(expr) => {
            println!("{padding}Parentheses:");
            print_math(expr, str, depth + 1);
        }
        Expression::Block { statements, ret } => {
            println!("{padding}Block:");

            let no_statements = if statements.len() > 0 { "" } else { " <None>" };
            println!("{padding}  Statements:{no_statements}");
            for statement in statements {
                print_statement(&statement, str, depth + 2);
            }

            if let Some(ret) = ret {
                println!("{padding}  Return:");
                print_math(ret, str, depth + 1);
            }
        }
        Expression::Return(expr) => {
            println!("{padding}Return:");
            if let Some(expr) = expr {
                print_math(expr, str, depth + 1);
            }
        }

        Expression::If {
            condition,
            then,
            otherwise,
        } => {
            println!("{padding}If:");

            print_math(condition, str, depth + 1);
            print_math(then, str, depth + 1);

            if let Some(otherwise) = otherwise {
                println!("{padding}Else:");
                print_math(otherwise, str, depth + 1);
            }
        }

        Expression::Access { expr, field } => {
            println!("{padding}Access: {field}");

            print_math(expr, str, depth + 1);
        }

        #[allow(unreachable_patterns)]
        _ => {
            println!("{padding}Unknown expression");
        }
    }
}
