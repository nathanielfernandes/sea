use eyre::Result;
use std::fs::read_to_string;

use crate::parser::{scanner::Span, BinaryOp, Body, Expression, Function, Param, Statement};

pub struct Compiler {
    user_prefix: String,

    function_definitions: String,
    function_declarations: String,
}

// helpers
#[rustfmt::skip]
impl Compiler {
    const VALUE_TYPE: &'static str = "Value";
    const RESULT_TYPE: &'static str = "Result";
    const TRY: &'static str = "Try";
    const OK: &'static str = "Ok";
    const EXPECT_BOOL: &'static str = "EXPECT_BOOL";

    fn unit() -> String { String::from("Unit()") }
    fn bool(value: bool) -> String { format!("Bool({})", value) }
    fn int(value: i64) -> String { format!("Int({})", value) }
    fn float(value: f64) -> String { format!("Float({})", value) }
    fn string(value: &str) -> String { format!("String({:?})", value) }
    fn op(op: BinaryOp) -> &'static str { 
        use BinaryOp::*;
        match op {
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "div",
            Mod => "mod",
            Equals => "equals",
            NotEquals => "not_equals",
            Greater => "greater",
            Less => "less",
            GreaterEqual => "greater_equal",
            LessEqual => "less_equal",
            And => "and",
            Or => "or",
            Not => "not",
            BitwiseAnd => "bitwise_and",
            BitwiseOr => "bitwise_or",
            BitwiseXor => "bitwise_xor",
            BitwiseNot => "bitwise_not",
            LeftShift => "left_shift",
            RightShift => "right_shift",
        }
    }
    const BUILTIN_FUNCTIONS: &'static [&'static str] = &[
        "print",
        "println",
    ];
}

impl Compiler {
    pub fn new(user_prefix: String) -> Compiler {
        Compiler {
            user_prefix,

            function_definitions: String::new(),
            function_declarations: String::new(),
        }
    }

    fn symbol(&self, value: &str, skip: bool) -> String {
        if !skip && Self::BUILTIN_FUNCTIONS.contains(&value) {
            return value.to_string();
        }

        format!("{}__{}", self.user_prefix, value)
    }

    pub fn compile(mut self, statements: &[Span<Statement>]) -> Result<String> {
        const LIB: &str = "/* {{lib}} */";
        const FUNCTION_DEFS: &str = "/* {{function definitions}} */";
        const FUNCTIONS: &str = "/* {{functions}} */";
        const MAIN: &str = "/* {{main}} */";

        let template = read_to_string("src/static/template.c")?;

        let mut main_code = String::new();

        for statement in statements {
            main_code.push_str(&self.compile_statement(statement)?);
        }

        let lib = read_to_string("src/static/lib.c")?;

        let final_code = template
            .replace(LIB, &lib)
            .replace(FUNCTION_DEFS, &self.function_definitions)
            .replace(FUNCTIONS, &self.function_declarations)
            .replace(MAIN, &main_code);

        Ok(final_code)
    }

    fn compile_statement(
        &mut self,
        Span { start, end, value }: &Span<Statement>,
    ) -> Result<String> {
        let code = match value {
            Statement::Expression(expression) => self.compile_expression(expression)?,
            Statement::Declaration {
                name,
                is_mutable,
                ty,
                value,
            } => {
                let value = self.compile_expression(value)?;
                format!(
                    "{} {} = {}",
                    Self::VALUE_TYPE,
                    self.symbol(name, true),
                    value
                )
            }
            Statement::Assignment { name, value } => {
                let value = self.compile_expression(value)?;
                format!("{} = {}", self.symbol(name, true), value)
            }
            Statement::Function(function) => {
                let (def, delc) = &self.compile_function(function)?;
                self.function_declarations.push_str(delc);
                self.function_definitions.push_str(def);
                return Ok(String::new());
            }
            Statement::WhileLoop { condition, body } => {
                let condition = self.compile_expression(condition)?;
                let body = self.compile_expression(body)?;
                format!("while ({}({})) {}", Self::EXPECT_BOOL, condition, body)
            }
        };

        Ok(code + ";\n")
    }

    fn compile_expression(
        &mut self,
        Span { start, end, value }: &Span<Expression>,
    ) -> Result<String> {
        let expr = match value {
            Expression::Unit => Self::unit(),
            Expression::Bool(b) => Self::bool(*b),
            Expression::Int(i) => Self::int(*i),
            Expression::Float(f) => Self::float(*f),
            Expression::String(s) => Self::string(s),
            Expression::Symbol(s) => self.symbol(s, false),
            Expression::FunctionCall { func, args } => {
                let args = args
                    .iter()
                    .map(|arg: &Span<Expression>| self.compile_expression(arg))
                    .collect::<Result<Vec<String>>>()?
                    .join(", ");
                let func = self.compile_expression(func)?;
                format!("{}(({})({}))", Self::TRY, func, args)
            }
            Expression::Parentheses(e) => {
                let e = self.compile_expression(e)?;
                format!("({})", e)
            }
            Expression::Binary { op, lhs, rhs } => {
                let lhs = self.compile_expression(lhs)?;
                let rhs = self.compile_expression(rhs)?;
                format!("{}({}({}, {}))", Self::TRY, Self::op(*op), lhs, rhs)
            }
            Expression::Block { statements, ret } => {
                let statements = statements
                    .iter()
                    .map(|statement| self.compile_statement(statement))
                    .collect::<Result<Vec<String>>>()?
                    .join("");

                match ret {
                    Some(ret) => {
                        let expr = self.compile_expression(ret)?;
                        format!("({{\n{}\n{};}})", statements, expr)
                    }
                    None => format!("{{\n{}}}", statements),
                }
            }
            Expression::If {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.compile_expression(condition)?;
                let then = self.compile_expression(then)?;
                if let Some(otherwise) = otherwise {
                    let otherwise = self.compile_expression(otherwise)?;
                    format!(
                        "if ({}({})) {} else {}",
                        Self::EXPECT_BOOL,
                        condition,
                        then,
                        otherwise
                    )
                } else {
                    format!("if ({}({})) {}", Self::EXPECT_BOOL, condition, then)
                }
            }
            Expression::Return(expr) => {
                let expr = match expr {
                    Some(expr) => self.compile_expression(expr)?,
                    None => Self::unit(),
                };
                format!("return {}({})", Self::OK, expr)
            }
            Expression::Access { expr, field } => todo!(),
        };

        Ok(expr)
    }

    fn compile_function(
        &mut self,
        Function {
            function_kind,
            name,
            params,
            return_type,
            body,
        }: &Function,
    ) -> Result<(String, String)> {
        let params = params
            .iter()
            .map(|param| self.compile_param(param))
            .collect::<Vec<_>>()
            .join(", ");

        let body = self.compile_body(body)?;

        let delc = format!(
            "{ret} {name}({params}) {{\n{body}\n}}",
            ret = Self::RESULT_TYPE,
            name = self.symbol(&name.value, true),
            params = params,
            body = body
        );

        let def = format!(
            "{ret} {name}({params});",
            ret = Self::RESULT_TYPE,
            name = self.symbol(&name.value, true),
            params = params
        );

        Ok((def, delc))
    }

    fn compile_body(
        &mut self,
        Span {
            start,
            end,
            value: body,
        }: &Span<Body>,
    ) -> Result<String> {
        Ok(body
            .statements
            .iter()
            .map(|statement| self.compile_statement(statement))
            .collect::<Result<Vec<String>>>()?
            .join(""))
    }

    fn compile_param(&self, param: &Span<Param>) -> String {
        let name = &param.value.name.value;
        format!("{} {}", Self::VALUE_TYPE, self.symbol(name, true))
    }
}
