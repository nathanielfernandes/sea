// IR for tha ast

use crate::parser::{scanner::Span, BinaryOp, Expression, Literal, Statement, Type};

use super::pool::Pool;

pub type TempId = usize;
pub type BlockId = usize;

#[derive(Debug, Clone)]
pub enum IRType {
    Any,

    Unit,
    Bool,
    Int,
    Float,
    String,

    Struct(Span<String>),
}

#[derive(Debug, Clone)]
pub enum IRConstant {
    Unit,
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum IRValue {
    Constant(IRConstant),
    Variable(TempId), // identifier for temporary value
}

pub enum IRStatement {
    ValueAssignment {
        target: Span<TempId>,
        value: Span<IRValue>,
    },
    CallAssignment {
        target: Span<TempId>,
        function: Span<String>,
        arguments: Vec<Span<IRValue>>,
    },
}

impl IRStatement {
    pub fn target(&self) -> TempId {
        match self {
            IRStatement::ValueAssignment { target, .. } => target.value,
            IRStatement::CallAssignment { target, .. } => target.value,
        }
    }
}

pub struct IRBlock {
    pub id: BlockId,
    pub statements: Vec<Span<IRStatement>>,
    pub next_block: Option<BlockId>,
}

#[derive(Debug, Clone)]
pub struct IRVar {
    pub initialized: bool,

    pub id: Span<TempId>,
    pub ty: Span<IRType>,
    pub name: Option<String>,

    pub depth: usize,
}

pub struct IRFunction {
    pub name: Span<String>,
    pub locals: Vec<IRVar>,
    pub blocks: Vec<IRBlock>,
}

struct Level {
    enclosing: Option<Box<Level>>,

    function: IRFunction,

    scope_depth: usize,

    symbol_table: Pool<String, TempId>,
}

impl Level {
    fn resolve_enclosing(&self, name: &str) -> Option<&IRVar> {
        if let Some(enclosing) = &self.enclosing {
            if let Some(var) = enclosing.resolve_local(name) {
                return Some(var);
            }

            return enclosing.resolve_enclosing(name);
        }

        None
    }

    fn resolve_local(&self, name: &str) -> Option<&IRVar> {
        for local in self.function.locals.iter().rev() {
            if local.name.as_ref().map(|n| n == name).unwrap_or(false) {
                return Some(local);
            }
        }

        None
    }
}

pub struct IRCompiler {
    level: Level,

    functions: Vec<IRFunction>,
}

impl IRCompiler {
    pub fn new() -> IRCompiler {
        IRCompiler {
            level: Level {
                enclosing: None,
                function: IRFunction {
                    name: Span {
                        value: "main".to_string(),
                        start: 0,
                        end: 0,
                    },
                    locals: Vec::new(),
                    blocks: vec![IRBlock {
                        id: 0,
                        statements: Vec::new(),
                        next_block: None,
                    }],
                },
                scope_depth: 0,
                symbol_table: Pool::new(),
            },

            functions: Vec::new(),
        }
    }

    fn get_symbol(&self, id: TempId) -> &String {
        self.level.symbol_table.get(id).expect("Symbol not found")
    }

    fn symbol<S: Into<String>>(&mut self, symbol: S) -> TempId {
        self.level.symbol_table.add(symbol.into())
    }

    fn shadow_symbol<S: Into<String>>(&mut self, symbol: S) -> TempId {
        self.level.symbol_table.add_force(symbol.into())
    }

    fn temp(&mut self, ty: Span<IRType>) -> Span<TempId> {
        let name = ty.span(format!("@temp_{}", self.level.function.locals.len()));
        self.declare_local(&name, ty)
    }

    fn id<S: Into<String>>(&self, symbol: S) -> Option<TempId> {
        self.level.symbol_table.id(&symbol.into())
    }

    fn enter_level(&mut self, level: Level) {
        let level = std::mem::replace(&mut self.level, level);
        self.level.enclosing = Some(Box::new(level));
    }

    fn exit_level(&mut self) {
        let enclosing = self.level.enclosing.take().expect("No enclosing level");
        let level = std::mem::replace(&mut self.level, *enclosing);
        self.functions.push(level.function);
    }

    fn enter_scope(&mut self) {
        self.level.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        self.level.scope_depth -= 1;
    }

    fn get_variable(&self, name: &str) -> &IRVar {
        if let Some(var) = self.level.resolve_local(name) {
            return var;
        } else if let Some(enclosing) = self.level.resolve_enclosing(name) {
            return enclosing;
        }

        panic!("Variable not found: {}", name);
    }

    fn map_type(ty: &Option<Span<Type>>) -> Span<IRType> {
        match ty {
            Some(ty) => {
                let Span { start, end, value } = ty.clone();
                match value {
                    Type::Any => Span {
                        start,
                        end,
                        value: IRType::Any,
                    },
                    Type::Name(name) => Span {
                        start,
                        end,
                        value: match name.as_str() {
                            "Unit" => IRType::Unit,
                            "Bool" => IRType::Bool,
                            "Int" => IRType::Int,
                            "Float" => IRType::Float,
                            "String" => IRType::String,
                            _ => IRType::Struct(Span {
                                start,
                                end,
                                value: name,
                            }),
                        },
                    },
                    _ => todo!(),
                }
            }
            None => Span {
                start: 0,
                end: 0,
                value: IRType::Any,
            },
        }
    }

    fn declare_local(&mut self, name: &Span<String>, ty: Span<IRType>) -> Span<TempId> {
        for i in (0..self.level.function.locals.len()).rev() {
            let local = &self.level.function.locals[i];

            if local.depth < self.level.scope_depth {
                break;
            }

            if local
                .name
                .as_ref()
                .map(|n| n == &name.value)
                .unwrap_or(false)
            {
                // variable aldready declared
                let id = name.span(self.shadow_symbol(&name.value));
                self.level.function.locals.push(IRVar {
                    initialized: false,

                    id,
                    ty,
                    name: Some(name.value.clone()),
                    depth: self.level.scope_depth,
                });

                return id;
            }
        }

        // variable not declared
        let id = name.span(self.symbol(name.value.clone()));
        self.level.function.locals.push(IRVar {
            initialized: false,

            id,
            ty,
            name: Some(name.value.clone()),
            depth: self.level.scope_depth,
        });

        id
    }

    fn initialize_local(&mut self, id: TempId) {
        for local in self.level.function.locals.iter_mut().rev() {
            if local.id.value == id {
                local.initialized = true;
                return;
            }
        }

        panic!("Local not found: {}", id);
    }

    fn push_statement(&mut self, statement: IRStatement) {
        let target = statement.target();
        self.initialize_local(target);

        let block = &mut self.level.function.blocks.last_mut().expect("No block");
        block.statements.push(Span {
            start: 0,
            end: 0,
            value: statement,
        });
    }

    pub fn compile_statement(&mut self, Span { start, end, value }: &Span<Statement>) {
        match value {
            Statement::Declaration {
                name,
                is_mutable,
                ty,
                value,
            } => {
                let target = self.declare_local(name, Self::map_type(ty));
                self.compile_expression(value, Some(target));
            }

            _ => todo!(),
        }
    }

    fn compile_expression(
        &mut self,
        Span { start, end, value }: &Span<Expression>,
        target: Option<Span<TempId>>,
    ) -> Span<IRValue> {
        match value {
            Expression::Literal(value) => {
                //
                let v = match value {
                    // Literal::Int(value) => {
                    //     self.push_statement(IRStatement::ValueAssignment {
                    //         target,
                    //         value: Span {
                    //             start: *start,
                    //             end: *end,
                    //             value: IRValue::Int(*value),
                    //         },
                    //     });
                    // }
                    Literal::Int(value) => Span {
                        start: *start,
                        end: *end,
                        value: IRValue::Constant(IRConstant::Int(*value)),
                    },

                    _ => todo!(),
                };

                if let Some(target) = target {
                    self.push_statement(IRStatement::ValueAssignment {
                        target,
                        value: v.clone(),
                    });
                }

                v
            }
            Expression::Binary { op, lhs, rhs } => {
                let lhs_value = self.compile_expression(lhs, None);
                let rhs_value = self.compile_expression(rhs, None);

                let target = match target {
                    Some(target) => target,
                    None => self.temp(Span {
                        start: *start,
                        end: *end,
                        value: IRType::Any,
                    }),
                };

                let op = match op {
                    BinaryOp::Add => "add",
                    BinaryOp::Div => "div",
                    _ => todo!(),
                };

                self.push_statement(IRStatement::CallAssignment {
                    target,
                    function: Span {
                        start: *start,
                        end: *end,
                        value: op.to_string(),
                    },
                    arguments: vec![lhs_value, rhs_value],
                });

                target.span(IRValue::Variable(target.value))
            }
            Expression::Symbol(symbol) => {
                let var = self.get_variable(&symbol);
                if !var.initialized {
                    panic!("Variable not initialized: {}", symbol);
                }

                let value = Span {
                    start: *start,
                    end: *end,
                    value: IRValue::Variable(var.id.value),
                };

                if let Some(target) = target {
                    self.push_statement(IRStatement::ValueAssignment {
                        target,
                        value: value.clone(),
                    });
                }

                value
            }
            erm => todo!("{:?}", erm),
        }
    }

    fn format_ir_value(value: &IRValue) -> String {
        match value {
            IRValue::Constant(constant) => {
                let c = match constant {
                    IRConstant::Unit => "()".to_string(),
                    IRConstant::Int(value) => value.to_string(),
                    IRConstant::Float(value) => value.to_string(),
                    IRConstant::String(value) => format!("{:?}", value),
                };
                format!("const {}", c)
            }
            IRValue::Variable(id) => format!("_{}", id),
        }
    }

    fn print_function(function: &IRFunction) {
        println!("fn {}", function.name.value);
        for local in &function.locals {
            println!("let _{}: {:?};", local.id.value, local.ty.value);
        }

        println!();

        for block in &function.blocks {
            println!("b{}:", block.id);
            for statement in &block.statements {
                match &statement.value {
                    IRStatement::ValueAssignment { target, value } => {
                        println!(
                            "  _{} = {}",
                            target.value,
                            Self::format_ir_value(&value.value)
                        );
                    }
                    IRStatement::CallAssignment {
                        target,
                        function,
                        arguments,
                    } => {
                        let args = arguments
                            .iter()
                            .map(|arg| Self::format_ir_value(&arg.value))
                            .collect::<Vec<_>>()
                            .join(", ");
                        println!("  _{} = {}({})", target.value, function.value, args);
                    }

                    _ => todo!(),
                }
            }
        }
    }

    pub fn print(&self) {
        let func = &self.level.function;
        Self::print_function(&func);
    }
}
