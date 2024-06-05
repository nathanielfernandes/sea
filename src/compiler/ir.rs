// IR for tha ast

use crate::parser::{scanner::Span, BinaryOp, Expression, Function, Literal, Statement, Type};

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
    Function(Vec<Span<IRType>>, Box<Span<IRType>>),
}

#[derive(Debug, Clone)]
pub enum IRConstant {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Function(String),
}

#[derive(Debug, Clone)]
pub enum IRValue {
    Constant(IRConstant),
    Variable(TempId), // identifier for temporary value

    UpValue(TempId), // identifier for upvalue
    BuiltIn(String),
}

pub enum IRStatement {
    ValueAssignment {
        target: Span<TempId>,
        value: Span<IRValue>,
    },
    CallAssignment {
        target: Span<TempId>,
        function: Span<IRValue>,
        arguments: Vec<Span<IRValue>>,
    },
    BuiltinCallAssignment {
        target: Span<TempId>,
        function: Span<String>,
        arguments: Vec<Span<IRValue>>,
    },
    Return {
        value: Span<IRValue>,
    },
}

#[derive(Debug, Clone)]
pub enum IRError {
    UndefinedVariable(String),
    UninitializedVariable(String),
    RedclaredParam(String),
}

impl IRError {
    pub fn to_unintialized_variable(self) -> IRError {
        match self {
            IRError::UndefinedVariable(name) => IRError::UninitializedVariable(name),
            _ => self,
        }
    }
}

pub type Result<T> = std::result::Result<T, IRError>;

impl IRStatement {
    pub fn target(&self) -> Option<TempId> {
        let t = match self {
            IRStatement::ValueAssignment { target, .. } => target.value,
            IRStatement::CallAssignment { target, .. } => target.value,
            IRStatement::BuiltinCallAssignment { target, .. } => target.value,

            _ => return None,
        };

        Some(t)
    }
}

pub struct IRBlock {
    pub id: BlockId,
    pub statements: Vec<Span<IRStatement>>,
    pub next_block: Option<BlockId>,
}

#[derive(Debug, Clone)]
pub struct IRVar {
    pub id: Span<TempId>,
    pub ty: Span<IRType>,
    pub name: Option<String>,
    pub builtin: bool,

    dead: bool,
    initialized: bool,
    depth: usize,
}

pub struct IRFunction {
    pub name: Span<String>,
    pub params: Vec<IRVar>,
    pub locals: Vec<IRVar>,
    pub ret_ty: Span<IRType>,
    pub blocks: Vec<IRBlock>,
}

struct Level {
    enclosing: Option<Box<Level>>,

    function: IRFunction,

    scope_depth: usize,

    symbol_table: Pool<String, TempId>,
}

impl Level {
    fn resolve_enclosing(&self, name: &str, need_init: bool) -> Result<&IRVar> {
        if let Some(enclosing) = &self.enclosing {
            if let Ok(var) = enclosing.resolve_local(name, need_init) {
                return Ok(var);
            }

            return enclosing.resolve_enclosing(name, need_init);
        }

        Err(IRError::UndefinedVariable(name.to_string()))
    }

    fn resolve_local(&self, name: &str, need_init: bool) -> Result<&IRVar> {
        let mut err = IRError::UndefinedVariable(name.to_string());

        for local in self
            .function
            .params
            .iter()
            .chain(self.function.locals.iter())
            .rev()
        {
            if local.dead {
                continue;
            }

            if need_init && !local.initialized {
                err = err.to_unintialized_variable();
                continue;
            }

            if local.name.as_ref().map(|n| n == name).unwrap_or(false) {
                return Ok(local);
            }
        }

        Err(err)
    }
}

pub struct IRCompiler {
    level: Level,

    global_symbols: Pool<String, TempId>,
    functions: Vec<IRFunction>,
}

impl IRCompiler {
    pub fn new<S: ToString>(main: S) -> IRCompiler {
        IRCompiler {
            level: Level {
                enclosing: None,
                function: IRFunction {
                    name: Span {
                        value: main.to_string(),
                        start: 0,
                        end: 0,
                    },
                    ret_ty: Span {
                        value: IRType::Unit,
                        start: 0,
                        end: 0,
                    },
                    params: Vec::new(),
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

            global_symbols: Pool::new(),
            functions: Vec::new(),
        }
    }

    pub fn compile(mut self, statements: &[Span<Statement>]) -> Result<Vec<IRFunction>> {
        // find all functions
        for statement in statements {
            if let Statement::Function(Function { name, .. }) = &statement.value {
                self.global_symbols.add(name.value.clone());
            }
        }

        for statement in statements {
            self.compile_statement(statement, None)?;
        }

        let main = self.level.function;
        self.functions.push(main);

        Ok(self.functions)
    }

    // fn get_symbol(&self, id: TempId) -> &String {
    //     self.level.symbol_table.get(id).expect("Symbol not found")
    // }

    fn symbol<S: Into<String>>(&mut self, symbol: S) -> TempId {
        self.level.symbol_table.add(symbol.into())
    }

    // used for params, params should be unique
    fn only_new_symbol<S: Into<String>>(&mut self, symbol: S) -> Option<TempId> {
        self.level.symbol_table.add_checked(symbol.into())
    }

    fn shadow_symbol<S: Into<String>>(&mut self, symbol: S) -> TempId {
        self.level.symbol_table.add_force(symbol.into())
    }

    fn temp(&mut self, ty: Span<IRType>) -> Span<TempId> {
        let name = ty.span(format!("@temp_{}", self.level.function.locals.len()));
        self.declare_local(&name, ty)
    }

    // fn id<S: Into<String>>(&self, symbol: S) -> Option<TempId> {
    //     self.level.symbol_table.id(&symbol.into())
    // }

    fn enter_level(&mut self, level: Level) {
        let level = std::mem::replace(&mut self.level, level);
        self.level.enclosing = Some(Box::new(level));
    }

    fn exit_level(&mut self) {
        let enclosing = self.level.enclosing.take().expect("No enclosing level");
        let level = std::mem::replace(&mut self.level, *enclosing);
        self.functions.push(level.function);
    }

    fn enter_function(&mut self, name: Span<String>, ty: Span<IRType>, ret_ty: Span<IRType>) {
        // let id = self.declare_local(&name, ty);
        // self.initialize_local(id.value);

        let function = IRFunction {
            name,
            ret_ty,
            params: Vec::new(),
            locals: Vec::new(),
            blocks: vec![IRBlock {
                id: 0,
                statements: Vec::new(),
                next_block: None,
            }],
        };

        self.enter_level(Level {
            enclosing: None,
            function,
            scope_depth: self.level.scope_depth + 1,
            symbol_table: Pool::new(),
        });
    }

    fn exit_function(&mut self) {
        self.exit_level();
    }

    fn enter_scope(&mut self) {
        self.level.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        self.level.scope_depth -= 1;

        for local in self.level.function.locals.iter_mut().rev() {
            if local.depth > self.level.scope_depth {
                local.dead = true;
            }
        }
    }

    fn enter_block(&mut self) {
        if let Some(block) = self.level.function.blocks.last_mut() {
            if block.statements.is_empty() {
                return;
            }

            let id = block.id + 1;
            block.next_block = Some(id);

            self.level.function.blocks.push(IRBlock {
                id,
                statements: Vec::new(),
                next_block: None,
            });
            return;
        }

        panic!("No block");
    }

    fn enter_scoped_block(&mut self) {
        self.enter_scope();
        self.enter_block();
    }

    fn exit_scoped_block(&mut self) {
        self.exit_scope();
        self.enter_block();
    }

    // returns the variable and if it is a local
    // fn get_variable(&self, name: &String, need_init: bool) -> Result<(TempId, bool)> {
    //     match self.level.resolve_local(name, need_init) {
    //         Ok(var) => return Ok((var.id.value, true)),
    //         Err(_) => match self.level.resolve_enclosing(name, need_init) {
    //             Ok(var) => return Ok((var.id.value, false)),
    //             Err(err) => {
    //                 // check if it is a function
    //                 if let Some(id) = self.global_symbols.id(name) {
    //                     return Ok((id, false));
    //                 }

    //                 return Err(err);
    //             }
    //         },
    //     };
    // }
    fn get_value(&self, name: &String, need_init: bool) -> Result<IRValue> {
        match self.level.resolve_local(name, need_init) {
            Ok(var) => return Ok(IRValue::Variable(var.id.value)),
            Err(_) => match self.level.resolve_enclosing(name, need_init) {
                Ok(var) => return Ok(IRValue::UpValue(var.id.value)),
                Err(err) => {
                    // check if it is a function
                    if let Some(_) = self.global_symbols.id(name) {
                        return Ok(IRValue::Constant(IRConstant::Function(name.clone())));
                    }

                    return Err(err);
                }
            },
        };
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
        for local in self
            .level
            .function
            .params
            .iter()
            .chain(self.level.function.locals.iter())
            .rev()
        {
            if local.dead && local.depth < self.level.scope_depth {
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
                    dead: false,
                    initialized: false,
                    builtin: false,

                    id,
                    ty,
                    name: Some(name.value.clone()),
                    depth: self.level.scope_depth,
                });

                return id;
            }
        }

        // variable not declared
        let id = name.span(self.symbol(&name.value));
        self.level.function.locals.push(IRVar {
            dead: false,
            initialized: false,
            builtin: false,

            id,
            ty,
            name: Some(name.value.clone()),
            depth: self.level.scope_depth,
        });

        id
    }

    fn declare_param(&mut self, name: &Span<String>, ty: Span<IRType>) -> Result<Span<TempId>> {
        let id = self
            .only_new_symbol(&name.value)
            .ok_or(IRError::RedclaredParam(name.value.clone()))?;

        let id = name.span(id);
        self.level.function.params.push(IRVar {
            dead: false,
            initialized: true,
            builtin: false,

            id,
            ty,
            name: Some(name.value.clone()),
            depth: self.level.scope_depth,
        });

        Ok(id)
    }

    fn initialize_local(&mut self, id: TempId) {
        for local in self
            .level
            .function
            .params
            .iter_mut()
            .chain(self.level.function.locals.iter_mut())
            .rev()
        {
            if local.id.value == id {
                local.initialized = true;
                return;
            }
        }

        panic!("Local not found: {}", id);
    }

    fn push_statement(&mut self, statement: IRStatement) {
        if let Some(target) = statement.target() {
            self.initialize_local(target);
        }

        let block = &mut self.level.function.blocks.last_mut().expect("No block");
        block.statements.push(Span {
            start: 0,
            end: 0,
            value: statement,
        });
    }

    fn compile_statement(
        &mut self,
        Span { start, end, value }: &Span<Statement>,
        target: Option<Span<TempId>>,
    ) -> Result<()> {
        match value {
            Statement::Declaration {
                name,
                is_mutable,
                ty,
                value,
            } => {
                let target = self.declare_local(name, Self::map_type(ty));
                self.compile_expression(value, Some(target))?;

                Ok(())
            }
            Statement::Expression(expression) => {
                self.compile_expression(expression, target)?;
                Ok(())
            }
            Statement::Function(Function {
                function_kind,
                name,
                params,
                return_type,
                body,
            }) => {
                let param_tys = params
                    .iter()
                    .map(|param| Self::map_type(&Some(param.value.ty.clone())))
                    .collect::<Vec<_>>();
                let ret_ty = Self::map_type(&return_type);

                let ty = IRType::Function(param_tys, Box::new(ret_ty.clone()));

                self.enter_function(name.clone(), name.span(ty), ret_ty);

                for param in params {
                    self.declare_param(
                        &param.value.name,
                        Self::map_type(&Some(param.value.ty.clone())),
                    )?;
                }

                for statement in &body.value.statements {
                    self.compile_statement(statement, None)?;
                }

                if let Some(ret) = &body.value.ret {
                    self.compile_expression(ret, None)?;
                }

                self.exit_function();

                Ok(())
            }

            _ => todo!(),
        }
    }

    fn unit(start: usize, end: usize) -> Span<IRValue> {
        Span {
            start,
            end,
            value: IRValue::Constant(IRConstant::Unit),
        }
    }

    fn compile_expression(
        &mut self,
        Span { start, end, value }: &Span<Expression>,
        target: Option<Span<TempId>>,
    ) -> Result<Span<IRValue>> {
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

                Ok(v)
            }
            Expression::Binary { op, lhs, rhs } => {
                let lhs_value = self.compile_expression(lhs, None)?;
                let rhs_value = self.compile_expression(rhs, None)?;

                let target = match target {
                    Some(target) => target,
                    None => self.temp(Span {
                        start: *start,
                        end: *end,
                        value: IRType::Any,
                    }),
                };

                let op = get_op(*op);

                self.push_statement(IRStatement::BuiltinCallAssignment {
                    target,
                    function: Span {
                        start: *start,
                        end: *end,
                        value: op.to_string(),
                    },
                    arguments: vec![lhs_value, rhs_value],
                });

                Ok(target.span(IRValue::Variable(target.value)))
            }
            Expression::Symbol(symbol) => {
                let value = if ["print", "println"].contains(&symbol.as_str()) {
                    Span {
                        start: *start,
                        end: *end,
                        value: IRValue::BuiltIn(symbol.clone()),
                    }
                } else {
                    let value = self.get_value(&symbol, true)?;
                    // if !var.initialized {
                    //     panic!("Variable not initialized: {}", symbol);
                    // }

                    Span {
                        start: *start,
                        end: *end,
                        value,
                    }
                };

                if let Some(target) = target {
                    self.push_statement(IRStatement::ValueAssignment {
                        target,
                        value: value.clone(),
                    });
                }

                Ok(value)
            }
            Expression::FunctionCall { func, args } => {
                // let temp = self.temp(Span {
                //     start: func.start,
                //     end: func.end,
                //     value: IRType::Any,
                // });

                let func = self.compile_expression(func, None)?;

                let args = args
                    .iter()
                    .map(|arg| self.compile_expression(arg, None))
                    .collect::<Result<Vec<_>>>()?;

                let target = match target {
                    Some(target) => target,
                    None => self.temp(Span {
                        start: *start,
                        end: *end,
                        value: IRType::Any,
                    }),
                };

                self.push_statement(IRStatement::CallAssignment {
                    target,
                    function: func,
                    arguments: args,
                });

                Ok(target.span(IRValue::Variable(target.value)))
            }
            Expression::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.compile_expression(expr, None)?,
                    None => Self::unit(*start, *end),
                };

                self.push_statement(IRStatement::Return {
                    value: value.clone(),
                });

                Ok(value)
            }
            Expression::Block { statements, ret } => {
                self.enter_scoped_block();

                for statement in statements {
                    self.compile_statement(statement, None)?;
                }

                if let Some(ret) = ret {
                    let res = self.compile_expression(ret, target);
                    self.exit_scoped_block();
                    return res;
                }

                let ret = Self::unit(*start, *end);

                match target {
                    Some(target) => {
                        self.push_statement(IRStatement::ValueAssignment {
                            target,
                            value: ret.clone(),
                        });
                    }
                    None => {}
                }

                self.exit_scoped_block();
                Ok(ret)

                // if let Some(ret) = ret {
                //     let ret = self.compile_expression(ret, target)?;
                //     self.push_statement(IRStatement::ValueAssignment {
                //         target: target.expect("No target"),
                //         value: ret,
                //     });
                // }

                // self.exit_block();

                // Ok(Self::unit(*start, *end))
            }
            erm => todo!("erm: {:?}", erm),
        }
    }

    fn format_ir_value(value: &IRValue) -> String {
        match value {
            IRValue::Constant(constant) => {
                let c = match constant {
                    IRConstant::Unit => "()".to_string(),
                    IRConstant::Bool(value) => value.to_string(),
                    IRConstant::Int(value) => value.to_string(),
                    IRConstant::Float(value) => value.to_string(),
                    IRConstant::String(value) => format!("{:?}", value),
                    IRConstant::Function(value) => format!("<fn {}>", value),
                };
                format!("const {}", c)
            }
            IRValue::Variable(id) => format!("_{}", id),
            IRValue::UpValue(id) => format!("<upvalue: _{}>", id),
            IRValue::BuiltIn(name) => name.clone(),
        }
    }

    pub fn print_function(function: &IRFunction) {
        let sep = if function.params.len() > 5 { "\n" } else { " " };
        let spa = if function.params.len() > 5 { " " } else { "" };
        print!("fn {}({sep}", function.name.value);
        for param in &function.params {
            print!("_{}: {:?},{sep}{spa}", param.id.value, param.ty.value);
        }
        println!(") -> {:?} {{", function.ret_ty.value);
        for local in &function.locals {
            println!("  let _{}: {:?};", local.id.value, local.ty.value);
        }

        println!();

        for block in &function.blocks {
            println!("  bb{}: {{", block.id);
            let mut first = true;
            for statement in &block.statements {
                if first {
                    first = false;
                } else {
                    println!(";");
                }
                match &statement.value {
                    IRStatement::ValueAssignment { target, value } => {
                        print!(
                            "    _{} = {}",
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
                        print!(
                            "    _{} = {}({})",
                            target.value,
                            Self::format_ir_value(&function.value),
                            args
                        );
                    }
                    IRStatement::BuiltinCallAssignment {
                        target,
                        function,
                        arguments,
                    } => {
                        let args = arguments
                            .iter()
                            .map(|arg| Self::format_ir_value(&arg.value))
                            .collect::<Vec<_>>()
                            .join(", ");
                        print!("    _{} = {}({})", target.value, function.value, args);
                    }

                    IRStatement::Return { value } => {
                        print!("    return {}", Self::format_ir_value(&value.value));
                    }

                    _ => todo!(),
                }
            }
            match block.next_block {
                Some(next) => println!(" -> [goto bb{}];\n  }}\n", next),
                None => println!("    [return];\n  }}"),
            }
        }

        println!("}}");
    }

    pub fn print(&self) {
        let func = &self.level.function;
        Self::print_function(&func);
        println!();

        for func in &self.functions {
            Self::print_function(&func);
            println!();
        }
    }
}

fn get_op(op: BinaryOp) -> &'static str {
    use BinaryOp::*;
    match op {
        Add => "add",
        Sub => "sub",
        Mul => "mul",
        Div => "div",
        Mod => "mod",
        Equals => "eq",
        NotEquals => "neq",
        Greater => "gt",
        Less => "lt",
        GreaterEqual => "gte",
        LessEqual => "lte",
        And => "and",
        Or => "or",
        Not => "not",

        BitwiseAnd => "bitand",
        BitwiseOr => "bitor",

        BitwiseXor => "bitxor",
        BitwiseNot => "bitnot",
        LeftShift => "shl",
        RightShift => "shr",
    }
}
