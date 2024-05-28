pub mod debug;
pub mod lexer;
pub mod scanner;

use std::usize;

use self::{
    lexer::Token,
    scanner::{Scanner, Span},
};

macro_rules! debug_println {
    ($($arg:tt)*) => {
        if false {
            println!($($arg)*);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equals,
    NotEquals,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    And,
    Or,
    Not,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LeftShift,
    RightShift,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),

    Symbol(String),
    FunctionCall {
        func: Box<Span<Expression>>,
        args: Vec<Span<Expression>>,
    },
    Parentheses(Box<Span<Expression>>),
    Binary {
        op: BinaryOp,
        lhs: Box<Span<Expression>>,
        rhs: Box<Span<Expression>>,
    },
    Block {
        statements: Vec<Span<Statement>>,
        ret: Option<Box<Span<Expression>>>,
    },
    If {
        condition: Box<Span<Expression>>,
        then: Box<Span<Expression>>,
        otherwise: Option<Box<Span<Expression>>>,
    },
    Return(Option<Box<Span<Expression>>>),
    Access {
        expr: Box<Span<Expression>>,
        field: String,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration {
        name: Span<String>,
        is_mutable: Span<bool>,
        ty: Option<Span<Type>>,
        value: Span<Expression>,
    },
    Assignment {
        name: Span<String>,
        value: Span<Expression>,
    },
    WhileLoop {
        condition: Span<Expression>,
        body: Box<Span<Expression>>,
    },
    Struct {
        name: Span<String>,
        fields: Vec<Span<Param>>,
    },
    Expression(Span<Expression>),
    Function(Function),
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionKind {
    Method { mutable: bool, consuming: bool },
    Static,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub function_kind: Span<FunctionKind>,
    pub name: Span<String>,
    pub params: Vec<Span<Param>>,
    pub return_type: Option<Span<Type>>,
    pub body: Span<Body>,
}

#[derive(Debug, Clone)]
pub struct Body {
    pub statements: Vec<Span<Statement>>,
    pub ret: Option<Span<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Span<String>,
    pub ty: Span<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Any,

    Name(String),
    //TODO not currently parsed
    Function {
        params: Vec<Span<Type>>,
        ret: Box<Span<Type>>,
    },
    Generic {
        name: Span<String>,
        fields: Vec<Span<Type>>,
    },
}

fn precedence(token: Token) -> usize {
    match token {
        Token::Mul | Token::Div | Token::Mod => 100,
        Token::Plus | Token::Minus => 50,
        _ => 0,
    }
}

pub struct Parser<'a> {
    input: &'a str,
    scanner: Scanner<Span<Token>, Span<ParserError>>,
    output: Vec<Span<Statement>>,
}

#[derive(Debug, Clone)]
pub enum ParserError {
    InvalidTypeField,
    InvalidParam,
    InvalidInteger,
    InvalidFloat,
    MissingToken {
        expected: &'static [Token],
    },
    UnexpectedToken {
        expected: &'static [Token],
        got: Token,
    },
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, tokens: Vec<Span<Token>>) -> Self {
        // let tokens = tokens
        //     .into_iter()
        //     .filter(|x| x.value != Token::WhiteSpace)
        //     .collect();

        let scanner = Scanner::new(tokens);
        let output = Vec::new();
        Self {
            input,
            scanner,
            output,
        }
    }

    pub fn push(&mut self, span: Span<Statement>) {
        self.output.push(span);
    }

    pub fn error(&mut self, error: Span<ParserError>) {
        self.scanner.error(error);
    }

    fn ty_fields(&mut self) -> Vec<Span<Type>> {
        let mut fields: Vec<Span<Type>> = Vec::new();

        while let Some(&token) = self.scanner.peek() {
            let result: Option<_> = try {
                match token.value {
                    Token::Greater => {
                        break;
                    }
                    Token::Comma => {
                        self.scanner.next();
                    }
                    _ => {
                        let ty = self.ty()?;
                        fields.push(ty);
                    }
                }
            };

            if result.is_none() {
                let error = token.span(ParserError::InvalidTypeField);
                self.error(error);
            }
        }

        fields
    }

    fn struct_statement(&mut self) -> Option<Span<Statement>> {
        // struct <symbol> { <symbol> : <type> , <symbol> : <type> , ... }
        let (struct_start, _) = self.match_token(Token::KwStruct)?;

        let (name, name_start, name_end) = self.symbol()?;
        let name = Span {
            start: name_start,
            end: name_end,
            value: name.to_string(),
        };

        self.match_token(Token::OpenCurlyBracket)?;

        let fields = self.comma_sep_params(Token::CloseCurlyBracket);

        let (_, end) = self.match_token(Token::CloseCurlyBracket)?;

        debug_println!("\x1b[32msuccessfully parsed struct\x1b[0m");

        Some(Span {
            start: struct_start,
            end,
            value: Statement::Struct { name, fields },
        })
    }

    fn ty(&mut self) -> Option<Span<Type>> {
        let (name, name_start, name_end) = self.symbol()?;
        let name = Span {
            start: name_start,
            end: name_end,
            value: name.to_string(),
        };

        let ty = match self.scanner.peek().map(|x| x.value) {
            Some(Token::Less) => {
                self.scanner.next();

                let fields = self.ty_fields();

                self.match_token(Token::Greater)?;

                Span {
                    start: name_start,
                    end: name_end,
                    value: Type::Generic { name, fields },
                }
            }
            _ => Span {
                start: name_start,
                end: name_end,
                value: Type::Name(name.value),
            },
        };

        Some(ty)
    }

    fn comma_sep_params(&mut self, closing: Token) -> Vec<Span<Param>> {
        let mut params: Vec<Span<Param>> = Vec::new();

        while let Some(&token) = self.scanner.peek() {
            let result: Option<_> = try {
                match token.value {
                    t if t == closing => {
                        break;
                    }
                    Token::Comma => {
                        self.scanner.next();
                    }
                    _ => {
                        let (name, name_start, name_end) = self.symbol()?;
                        let name = Span {
                            start: name_start,
                            end: name_end,
                            value: name.to_string(),
                        };

                        let ty: Option<Span<Type>> = try {
                            self.match_token(Token::Colon)?;
                            self.ty()?
                        };

                        let ty = ty.unwrap_or(Span {
                            start: name_end,
                            end: name_end,
                            value: Type::Any,
                        });

                        params.push(Span {
                            start: name_start,
                            end: ty.end,
                            value: Param { name, ty },
                        });
                    }
                }
            };

            let result = result;

            if result.is_none() {
                // println!("failed to parse param. (continuing...)");
                let error = token.span(ParserError::InvalidParam);
                self.error(error)
            }
        }

        params
    }

    fn return_expression(&mut self) -> Option<Span<Expression>> {
        let (return_start, return_end) = self.match_token(Token::KwReturn)?;

        let expr = self.transaction(|this| this.expression(0, false));

        let return_end = expr.as_ref().map(|x| x.end).unwrap_or(return_end);

        Some(Span {
            start: return_start,
            end: return_end,
            value: Expression::Return(expr.map(Box::new)),
        })
    }

    fn args(&mut self) -> Vec<Span<Expression>> {
        let mut args: Vec<Span<Expression>> = vec![];

        while let Some(&token) = self.scanner.peek() {
            match token.value {
                Token::CloseRoundBracket => {
                    break;
                }
                Token::Comma => {
                    self.scanner.next();
                }
                _ => {
                    if let Some(expr) = self.expression(0, true) {
                        args.push(expr);
                    } else {
                        break;
                    }
                }
            }
        }

        args
    }

    fn while_loop_statement(&mut self) -> Option<Span<Statement>> {
        // while <expression> { <block> }
        let (while_start, _) = self.match_token(Token::KwWhile)?;

        let condition = self.expression(0, false)?;

        let body = Box::new(self.block()?);

        debug_println!("\x1b[32msuccessfully parsed while loop\x1b[0m");

        Some(Span {
            start: while_start,
            end: body.end,
            value: Statement::WhileLoop { condition, body },
        })
    }

    fn let_declaration_statement(&mut self) -> Option<Span<Statement>> {
        // <let> [mut] <symbol> = <expression> ;

        let (let_start, _) = self.match_token(Token::KwLet)?;

        let mut is_mutable = self.mutable()?;

        let (symbol, symbol_start, symbol_end) = self.symbol()?;

        if !is_mutable.value {
            is_mutable.start = symbol_start;
            is_mutable.end = symbol_end;
        }

        let symbol = symbol.to_string();

        debug_println!("\x1b[36m>> symbol({symbol})\x1b[0m");

        let ty = self.transaction(|this| {
            this.match_token(Token::Colon)?;
            this.ty()
        });

        self.match_token(Token::Equal)?;

        let expr = self.expression(0, false)?;

        // let (_, semicolon_end) = self
        //     .match_token(Token::Semicolon)
        //     .unwrap_or((0, expr.end + 1));
        let mut semicolon_end = expr.end + 1;
        match self.match_token(Token::Semicolon) {
            Some((_, end)) => {
                semicolon_end = end;
            }
            None => {
                let error = Span {
                    start: semicolon_end,
                    end: semicolon_end + 1,
                    value: ParserError::MissingToken {
                        expected: &[Token::Semicolon],
                    },
                };
                self.error(error);
            }
        }

        debug_println!("\x1b[32msuccessfully parsed declaration\x1b[0m");
        debug_println!(
            "\x1b[32m>> {}\x1b[0m",
            &self.input[let_start..semicolon_end]
        );

        Some(Span {
            start: let_start,
            end: semicolon_end,
            value: Statement::Declaration {
                name: Span {
                    start: symbol_start,
                    end: symbol_end,
                    value: symbol,
                },
                ty,
                is_mutable,
                value: expr,
            },
        })
    }

    fn assignment_statement(&mut self) -> Option<Span<Statement>> {
        // <symbol> = <expression> ;
        let (symbol, symbol_start, symbol_end) = self.symbol()?;
        let symbol = symbol.to_string();

        debug_println!("\x1b[36m>> symbol({symbol})\x1b[0m");

        self.match_token(Token::Equal)?;

        let expr = self.expression(0, false)?;

        let mut semicolon_end = expr.end + 1;
        match self.match_token(Token::Semicolon) {
            Some((_, end)) => {
                semicolon_end = end;
            }
            None => {
                let error = Span {
                    start: semicolon_end,
                    end: semicolon_end + 1,
                    value: ParserError::MissingToken {
                        expected: &[Token::Semicolon],
                    },
                };
                self.error(error);
            }
        }

        debug_println!("\x1b[32msuccessfully parsed assignment\x1b[0m");
        debug_println!(
            "\x1b[32m>> {}\x1b[0m",
            &self.input[symbol_start..semicolon_end]
        );

        Some(Span {
            start: symbol_start,
            end: semicolon_end,
            value: Statement::Assignment {
                name: Span {
                    start: symbol_start,
                    end: symbol_end,
                    value: symbol,
                },
                value: expr,
            },
        })
    }

    fn expression_statement(&mut self) -> Option<Span<Statement>> {
        // <expression> ;
        let expr = self.expression(0, false)?;

        if let Expression::If { .. } = expr.value {
            // optional semicolon
            if let Some(Token::Semicolon) = self.scanner.peek().map(|x| x.value) {
                self.scanner.next();
            }

            return Some(Span {
                start: expr.start,
                end: expr.end,
                value: Statement::Expression(expr),
            });
        }

        let (_, semicolon_end) = self.match_token(Token::Semicolon)?;

        debug_println!("\x1b[32msuccessfully parsed expression statement\x1b[0m");
        debug_println!(
            "\x1b[32m>> {}\x1b[0m",
            &self.input[expr.start..semicolon_end]
        );

        Some(Span {
            start: expr.start,
            end: semicolon_end,
            value: Statement::Expression(expr),
        })
    }

    fn function_definition_statement(&mut self) -> Option<Span<Statement>> {
        // fn <symbol> ( <symbol> , <symbol> , ... ) { <block> }
        let (fn_start, _) = self.match_token(Token::KwFn)?;

        let (name, name_s, name_e) = self.symbol()?;
        let name = Span {
            start: name_s,
            end: name_e,
            value: name.to_string(),
        };

        self.match_token(Token::OpenRoundBracket)?;

        let function_kind = self
            .transaction(|this| {
                this.whitespace();
                let start = this.scanner.position();

                let consuming = this
                    .transaction(|this| this.match_token(Token::Ampersand))
                    .is_none();

                let mutable = this
                    .transaction(|this| this.mutable().map(|m| m.value))
                    .unwrap_or(false);

                let (_, end) = this.match_token(Token::KwSelf)?;

                Some(Span {
                    start,
                    end,
                    value: FunctionKind::Method { mutable, consuming },
                })
            })
            .unwrap_or(Span {
                start: 0,
                end: 0,
                value: FunctionKind::Static,
            });

        let params = self.comma_sep_params(Token::CloseRoundBracket);

        self.match_token(Token::CloseRoundBracket)?;

        let return_type = Some(
            self.transaction(|this| {
                this.match_token(Token::RightArrow)?;
                let ty = this.ty()?;
                Some(ty)
            })
            .unwrap_or(Span {
                start: name_e,
                end: name_e,
                value: Type::Any,
            }),
        );

        let body = self.body()?;

        debug_println!("\x1b[32msuccessfully parsed function definition\x1b[0m");

        Some(Span {
            start: fn_start,
            end: body.end,
            value: Statement::Function(Function {
                name,
                params,
                return_type,
                body,
                function_kind,
            }),
        })
    }

    fn if_expression(&mut self) -> Option<Span<Expression>> {
        // if <expression> { <block> } [else { <block> }]
        let (if_start, _) = self.match_token(Token::KwIf)?;

        let condition = self.expression(0, false)?;

        let then = self.block()?;

        let otherwise = self.transaction(|this| {
            this.match_token(Token::KwElse)?;

            // chained if
            if let Some(if_expr) = this.transaction(|this| this.if_expression()) {
                return Some(if_expr);
            }

            this.block()
        });

        debug_println!("\x1b[32msuccessfully parsed if expression\x1b[0m");

        Some(Span {
            start: if_start,
            end: otherwise.as_ref().map(|x| x.end).unwrap_or(then.end),
            value: Expression::If {
                condition: Box::new(condition),
                then: Box::new(then),
                otherwise: otherwise.map(Box::new),
            },
        })
    }

    fn statement(&mut self) -> Option<Span<Statement>> {
        debug_println!(
            "\x1b[30mcurrently at token position: {:?}\x1b[0m",
            self.scanner.peek()
        );
        debug_println!(
            "\x1b[33m{}\x1b[0m",
            &self.input[self.scanner.peek().unwrap().start..self.scanner.peek().unwrap().end]
        );

        self.whitespace();

        self.try_each([
            Self::struct_statement,
            Self::function_definition_statement,
            Self::while_loop_statement,
            Self::let_declaration_statement,
            Self::assignment_statement,
            Self::expression_statement,
        ])
    }

    fn transaction<T>(&mut self, mut f: impl FnMut(&mut Self) -> Option<T>) -> Option<T> {
        self.scanner.begin();
        let result = f(self);
        match result {
            Some(result) => {
                self.scanner.commit();
                Some(result)
            }
            None => {
                self.scanner.rollback();
                None
            }
        }
    }

    fn try_each<T, const SIZE: usize>(
        &mut self,
        funcs: [fn(&mut Self) -> Option<T>; SIZE],
    ) -> Option<T> {
        for f in funcs {
            self.scanner.begin();
            let result = f(self);
            match result {
                Some(result) => {
                    self.scanner.commit();
                    return Some(result);
                }
                None => {
                    self.scanner.rollback();
                }
            }
        }
        None
    }

    fn match_token(&mut self, token: Token) -> Option<(usize, usize)> {
        self.whitespace();
        if let Some(Span { value, start, end }) = self.scanner.peek().cloned() {
            if value == token {
                self.scanner.next();
                self.whitespace();
                return Some((start, end));
            }
        }
        None
    }

    fn whitespace(&mut self) {
        self.scanner.skip_all([Token::WhiteSpace, Token::Comment]);
    }

    fn mutable(&mut self) -> Option<Span<bool>> {
        if let Some((start, end)) = self.match_token(Token::KwMut) {
            return Some(Span {
                start,
                end,
                value: true,
            });
        }

        // let token = self.scanner.peek()?;
        // let error = token.span(ParserError::ExpectedToken {
        //     expected: &[Token::KwMut],
        //     got: token.value,
        // });
        // self.error(error);

        // return None; //TODO: we can still return Some(false) here if we want the parser to not stop trying to parse a declaration or something
        Some(Span {
            start: 0,
            end: 0,
            value: false,
        })
    }

    fn symbol(&mut self) -> Option<(&str, usize, usize)> {
        self.whitespace();
        if let Some(Span { value, start, end }) = self.scanner.peek().cloned() {
            if value == Token::Symbol {
                self.scanner.next();
                self.whitespace();
                return Some((&self.input[start..end], start, end));
            }
        }
        None
    }

    fn block(&mut self) -> Option<Span<Expression>> {
        let mut statements: Vec<Span<Statement>> = vec![];

        let (block_start, _) = self.match_token(Token::OpenCurlyBracket)?;

        while let Some(_) = self.scanner.peek() {
            let Some(statement) = self.statement() else {
                break;
            };

            statements.push(statement);
        }

        let ret = self.transaction(|this| this.expression(0, false));

        let (_, block_end) = self.match_token(Token::CloseCurlyBracket)?;

        Some(Span {
            start: block_start,
            end: block_end,
            value: Expression::Block {
                statements,
                ret: ret.map(Box::new),
            },
        })
    }

    fn body(&mut self) -> Option<Span<Body>> {
        let mut statements: Vec<Span<Statement>> = vec![];

        let (block_start, _) = self.match_token(Token::OpenCurlyBracket)?;

        while let Some(_) = self.scanner.peek() {
            if let None = self.transaction(|this| {
                let statement = this.statement()?;
                statements.push(statement);
                Some(())
            }) {
                break;
            }
            // let Some(statement) = self.statement() else {
            //     break;
            // };

            // statements.push(statement);
        }

        let ret = self.transaction(|this| this.expression(0, false));
        // println!("ret: {:?}", ret);

        // println!("peek: {:?}", self.scanner.peek());
        let (_, block_end) = self.match_token(Token::CloseCurlyBracket)?;

        // println!("block_end: {:?}", block_end);

        Some(Span {
            start: block_start,
            end: block_end,
            value: Body { statements, ret },
        })
    }

    fn atom(&mut self) -> Option<Span<Expression>> {
        self.whitespace();

        // try looking for an if expression
        if let Some(if_expr) = self.transaction(|this| this.if_expression()) {
            return Some(if_expr);
        }

        // try looking for a return expression
        if let Some(ret) = self.transaction(|this| this.return_expression()) {
            return Some(ret);
        }

        let token = self.scanner.peek().cloned()?;
        let expression = match token.value {
            Token::String => {
                self.scanner.next();
                let string = &self.input[token.start + 1..token.end - 1];
                token.span(Expression::String(string.to_string()))
            }

            Token::Int => {
                self.scanner.next();
                // println!("{}", &self.input[token.start..token.end]);
                let int: i64 = match self.input[token.start..token.end].parse() {
                    Ok(int) => int,
                    Err(_) => {
                        let error = token.span(ParserError::InvalidInteger);
                        self.error(error);
                        return None;
                    }
                };
                token.span(Expression::Int(int))
            }
            Token::Float => {
                self.scanner.next();
                let float: f64 = match self.input[token.start..token.end].parse() {
                    Ok(float) => float,
                    Err(_) => {
                        let error = token.span(ParserError::InvalidFloat);
                        self.error(error);
                        return None;
                    }
                };
                token.span(Expression::Float(float))
            }
            Token::Symbol => {
                self.scanner.next();
                let symbol = &self.input[token.start..token.end];

                // bool check
                let value = match symbol {
                    "true" => Expression::Bool(true),
                    "false" => Expression::Bool(false),
                    _ => Expression::Symbol(symbol.to_string()),
                };

                token.span(value)
            }
            Token::OpenCurlyBracket => self.block()?,
            Token::OpenRoundBracket => {
                self.scanner.next();
                // unit check
                if let Some(&token) = self.scanner.peek() {
                    if token.value == Token::CloseRoundBracket {
                        self.scanner.next();
                        return Some(token.span(Expression::Unit));
                    }
                }

                let expr = self.expression(0, true)?;
                self.match_token(Token::CloseRoundBracket)?;
                expr
            }
            Token::KwSelf => {
                self.scanner.next();
                Span {
                    start: token.start,
                    end: token.end,
                    value: Expression::Symbol("self".to_string()),
                }
            }
            _ => {
                // println!("\x1b[31mUnexpected token: {:?}\x1b[0m", token);
                let error = token.span(ParserError::UnexpectedToken {
                    expected: &[
                        Token::Int,
                        Token::Float,
                        Token::Symbol,
                        Token::OpenRoundBracket,
                        Token::OpenCurlyBracket,
                    ],
                    got: token.value,
                });
                self.error(error);
                return None;
            }
        };

        self.whitespace();

        Some(expression)
    }

    fn expression(
        &mut self,
        current_precedence: usize,
        expect_closing: bool,
    ) -> Option<Span<Expression>> {
        self.whitespace();

        let create_expression =
            |token: Span<Token>, lhs: Span<Expression>, rhs: Span<Expression>| Span {
                start: lhs.start,
                end: rhs.end,
                value: Expression::Binary {
                    op: match token.value {
                        Token::Plus => BinaryOp::Add,
                        Token::Minus => BinaryOp::Sub,
                        Token::Mul => BinaryOp::Mul,
                        Token::Div => BinaryOp::Div,
                        Token::Mod => BinaryOp::Mod,
                        Token::DoubleEqual => BinaryOp::Equals,
                        Token::NotEqual => BinaryOp::NotEquals,

                        Token::Chevron => BinaryOp::BitwiseXor,
                        Token::Ampersand => BinaryOp::BitwiseAnd,
                        Token::AmpersandAmpersand => BinaryOp::And,
                        Token::Bar => BinaryOp::BitwiseOr,
                        Token::BarBar => BinaryOp::Or,

                        Token::Greater => BinaryOp::Greater,
                        Token::Less => BinaryOp::Less,
                        Token::GreaterEqual => BinaryOp::GreaterEqual,
                        Token::LessEqual => BinaryOp::LessEqual,

                        Token::RightShift => BinaryOp::RightShift,
                        Token::LeftShift => BinaryOp::LeftShift,

                        _ => panic!("unexpected token (create_expression): {:?}", token),
                    },
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };

        let mut lhs = self.atom()?;

        while let Some(&token) = self.scanner.peek() {
            match token.value {
                x @ (Token::Plus
                | Token::Minus
                | Token::Mul
                | Token::Div
                | Token::Mod
                | Token::DoubleEqual
                | Token::NotEqual
                | Token::Chevron
                | Token::Ampersand
                | Token::AmpersandAmpersand
                | Token::Bar
                | Token::BarBar
                | Token::Greater
                | Token::Less
                | Token::GreaterEqual
                | Token::LessEqual
                | Token::RightShift
                | Token::LeftShift) => {
                    self.scanner.next();
                    let precedence = precedence(x);

                    let rhs = if precedence > current_precedence {
                        self.expression(precedence, expect_closing)?
                    } else {
                        self.atom()?
                    };

                    lhs = create_expression(token, lhs, rhs);
                }
                Token::OpenRoundBracket => {
                    self.scanner.next();
                    self.scanner.skip(Token::WhiteSpace);

                    let args = self.args();

                    let mut end = token.end + 1;
                    if let Some(token) = self.scanner.peek() {
                        end = token.end;
                        if token.value == Token::CloseRoundBracket {
                            self.scanner.next();
                        } else {
                            // println!(
                            //     "\x1b[31mExpected closing bracket but got: {:?}\x1b[0m",
                            //     token
                            // );
                            let error = token.span(ParserError::UnexpectedToken {
                                expected: &[Token::CloseRoundBracket],
                                got: token.value,
                            });
                            self.error(error);
                        }
                    }

                    let func = lhs;
                    lhs = Span {
                        start: func.start,
                        end,
                        value: Expression::FunctionCall {
                            func: Box::new(func),
                            args,
                        },
                    };
                }
                Token::Dot => {
                    self.scanner.next();
                    self.scanner.skip(Token::WhiteSpace);

                    let (field, _, field_end) = self.symbol()?;
                    let field = field.to_string();

                    lhs = Span {
                        start: lhs.start,
                        end: field_end,
                        value: Expression::Access {
                            expr: Box::new(lhs),
                            field,
                        },
                    };
                }
                Token::Semicolon => {
                    break;
                }
                _ => {
                    break;
                }
            }

            self.whitespace();
        }

        Some(lhs)
    }

    pub fn parse(mut self) -> (Vec<Span<Statement>>, Vec<Span<ParserError>>) {
        self.scanner.begin();
        self.whitespace();
        'while_loop: while let Some(_) = self.scanner.peek() {
            for f in [Self::statement] {
                self.scanner.begin();
                if let Some(statement) = f(&mut self) {
                    self.scanner.commit();
                    self.push(statement);
                    continue 'while_loop;
                } else {
                    self.scanner.rollback();
                }
            }

            // println!("\x1b[31munexpected token: {:?}\x1b[0m", self.scanner.peek());
            if let Some(token) = self.scanner.peek() {
                let error = token.span(ParserError::UnexpectedToken {
                    expected: &[],
                    got: token.value,
                });
                self.error(error);
            }

            self.scanner.next();
        }

        self.scanner.commit();
        (self.output, self.scanner.errors)
    }
}
