pub mod dump;
pub mod passes;
pub mod pathmap;
pub mod pool;

use std::{cell::RefCell, hash::Hash};

use hashbrown::{HashMap, HashSet};
use indexmap::IndexMap;
use pathmap::{Path, PathMap};
use pool::Pool;
use smallvec::{smallvec, SmallVec};

use crate::parser::{scanner::Span, BinaryOp, Expression, Literal, Statement, Type};

pub type InternedString = usize;
pub type TypeId = usize;
pub type VariableId = usize;

pub type BlockId = usize;
pub type StatementId = usize;

pub struct Context {
    pub type_table: IndexMap<TypeId, IRType>,
    next_type_id: TypeId,

    pub strings: Pool<String, InternedString>,

    // (type_id, is_rigid?)
    pub types: PathMap<InternedString, (TypeId, bool)>,
    pub functions: PathMap<InternedString, IRFunction>,
    pub structs: PathMap<InternedString, IRStruct>,
    pub builtin_functions: PathMap<InternedString, TypeId>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            type_table: IndexMap::new(),
            next_type_id: 0,

            strings: {
                // id 0 will always be an empty string
                let mut strings = Pool::new();
                strings.add("@temp".to_string());
                strings
            },

            types: PathMap::new(),
            functions: PathMap::new(),
            structs: PathMap::new(),
            builtin_functions: PathMap::new(),
        }
    }

    pub fn path_from<S: ToString>(&mut self, path: &[S]) -> Path<InternedString> {
        Path::from_iter(path.iter().map(|part| self.intern_string(part.to_string())))
    }

    pub fn get_type(&mut self, path: Path<InternedString>) -> Option<(TypeId, bool)> {
        self.types.get(&path).copied()
    }

    pub fn new_struct<S: ToString>(&mut self, path: &[S], span: Span<()>, builtin: bool) -> TypeId {
        let path = self.path_from(path);
        let ty = self.new_type(IRType::Named(path.clone()));

        let strct = IRStruct {
            path: span.span(path.clone()),
            ty,

            fields: Vec::new(),
            methods: Vec::new(),

            builtin,
        };

        self.structs.insert(path, strct);

        ty
    }

    pub fn intern_string<S: ToString>(&mut self, string: S) -> InternedString {
        self.strings.add(string.to_string())
    }

    pub fn re_intern_string<S: ToString>(&mut self, string: S) -> InternedString {
        self.strings.add_force(string.to_string())
    }

    pub fn new_type(&mut self, ty: IRType) -> TypeId {
        let id = self.next_type_id;
        self.next_type_id += 1;
        self.type_table.insert(id, ty);
        id
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum IRType {
    Unreachable,

    // non rigid types can be updated later on to less or more specific types
    // during inference, rigid types will raise a type error if they are not the same
    Any,
    // RigidAny,
    Named(Path<InternedString>),
    // RigidNamed(Path<InternedString>),
    Function {
        args: SmallVec<[TypeId; 4]>,
        ret: TypeId,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum IRConstant {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(InternedString),
}

#[derive(Debug, Clone)]
pub enum IRValue {
    Constant(Span<IRConstant>),
    Local(VariableId),
    Upvalue(VariableId),
    Function(Path<InternedString>),
    BuiltinFunction(Path<InternedString>),
}

pub enum IRExpression {
    Value(IRValue),
    Call {
        function: IRValue,
        args: SmallVec<[IRValue; 4]>,
    },
}

pub struct IRVariableMetadata {
    pub name: Option<Span<InternedString>>, // variables may not have names
    pub ty: Span<TypeId>,                   // span points to where the type was declared
    pub temp: bool,                         // is this a temporary variable?
    pub used: HashSet<StatementId>,         // statements that use this variable
    pub assigned: HashSet<StatementId>,     // statements that assign to this variable
    pub dead: bool,                         // is this variable dead?
    pub depth: usize,                       // scope depth
}

pub struct IRVariable {
    pub id: VariableId,
    pub metadata: RefCell<IRVariableMetadata>,
}

impl IRVariable {
    pub fn metadata(&self) -> std::cell::Ref<IRVariableMetadata> {
        self.metadata.borrow()
    }
}

pub struct IRStatementMetadata {
    pub skip: bool, // Skip this statement during codegen
    pub span: Option<Span<()>>,
}
pub struct IRStatement {
    pub id: StatementId,
    pub target: VariableId,
    pub expression: IRExpression,
    pub metadata: RefCell<IRStatementMetadata>,
}

impl IRStatement {
    pub fn new(
        target: VariableId,
        expression: IRExpression,
        span: Option<Span<()>>,
    ) -> IRStatement {
        IRStatement {
            id: 0, // will be set by the compiler
            target,
            expression,
            metadata: RefCell::new(IRStatementMetadata { skip: false, span }),
        }
    }
}

pub struct Block {
    pub id: BlockId,
    pub statements: Vec<IRStatement>,
    pub control_flow: ControlFlow,
}

pub enum ControlFlow {
    Continue,
    Goto(BlockId),
    Branch {
        condition: VariableId,
        success: BlockId,
        otherwise: BlockId,
    },
    Return(VariableId),
}

pub struct IRFunction {
    pub path: Span<Path<InternedString>>,
    pub ty: TypeId,

    pub params: IndexMap<VariableId, IRVariable>,
    pub locals: IndexMap<VariableId, IRVariable>,
    pub blocks: IndexMap<BlockId, Block>,
}

pub struct Method {
    pub path: Span<Path<InternedString>>,
    pub ty: TypeId,
}

pub struct IRStruct {
    pub path: Span<Path<InternedString>>,
    pub ty: TypeId,

    pub fields: Vec<Span<TypeId>>,
    pub methods: Vec<Method>,

    pub builtin: bool,
}

pub struct Level {
    pub enclosing: Option<Box<Level>>,
    pub function: IRFunction,
    pub scope_depth: usize,

    // variable name -> variable
    pub symbols: Pool<InternedString, VariableId>,
    // type name -> type
    pub types: HashMap<InternedString, TypeId>,
}

#[derive(Debug, PartialEq, Eq)]
enum VarState {
    Undefined,
    Uninitialized,
    Ok(VariableId),
}

impl Level {
    fn resolve_enclosing(&self, name: InternedString, initialized: bool) -> VarState {
        if let Some(enclosing) = &self.enclosing {
            if let VarState::Ok(state) = enclosing.resolve_local(name, initialized) {
                return VarState::Ok(state);
            }

            return enclosing.resolve_enclosing(name, initialized);
        }

        VarState::Undefined
    }

    fn resolve_local(&self, name: InternedString, initialized: bool) -> VarState {
        let mut vs = VarState::Undefined;

        for local in self
            .function
            .params
            .values()
            .chain(self.function.locals.values())
            .rev()
        {
            let meta = local.metadata.borrow();

            if meta.dead {
                continue;
            }

            if initialized && meta.assigned.is_empty() {
                vs = VarState::Uninitialized;
                continue;
            }

            if meta.name.as_ref().map(|n| n.value) == Some(name) {
                return VarState::Ok(local.id);
            }
        }

        return vs;
    }

    pub fn get_type(&self, name: InternedString) -> Option<TypeId> {
        if let Some(ty) = self.types.get(&name) {
            return Some(*ty);
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.get_type(name);
        }

        None
    }
}

#[derive(Debug)]
pub enum IRCompileError {
    UnkownType(Span<String>),
    UndefinedVariable(Span<String>),
    UninitializedVariable(Span<String>),
}

type Result<T> = std::result::Result<T, IRCompileError>;

pub struct IRCompiler {
    level: Level,

    pub context: RefCell<Context>,
}

impl IRCompiler {
    const TEMP_ID: InternedString = 0;

    pub fn new() -> Self {
        let mut comp = IRCompiler {
            level: Level {
                enclosing: None,
                function: IRFunction {
                    path: Span::default(Path::new()),
                    ty: 0,

                    params: IndexMap::new(),
                    locals: IndexMap::new(),
                    blocks: {
                        let mut blocks = IndexMap::new();
                        blocks.insert(
                            0,
                            Block {
                                id: 0,
                                statements: Vec::new(),
                                control_flow: ControlFlow::Continue,
                            },
                        );
                        blocks
                    },
                },
                scope_depth: 0,
                symbols: Pool::new(),
                types: HashMap::new(),
            },
            context: RefCell::new(Context::new()),
        };

        // adding directly to context, makes it unaccessible
        // comp.ctx_mut().new_type(IRType::Any);
        comp.new_type("!Unreachable", IRType::Unreachable);

        let unit = comp.new_struct(&["core", "Unit"], Span::default(()), true);
        let bool = comp.new_struct(&["core", "Bool"], Span::default(()), true);
        let int = comp.new_struct(&["core", "Int"], Span::default(()), true);
        let float = comp.new_struct(&["core", "Float"], Span::default(()), true);
        let string = comp.new_struct(&["core", "String"], Span::default(()), true);

        let main = comp.ctx_mut().path_from(&["root", "main"]);
        let fn_type = comp.ctx_mut().new_type(IRType::Function {
            args: SmallVec::new(),
            ret: unit,
        });
        comp.level.function.path = Span::default(main);
        comp.level.function.ty = fn_type;

        comp.new_builtin(&["core", "add"], {
            let lhs = comp.Any();
            IRType::Function {
                args: smallvec![lhs, comp.Any()],
                ret: lhs,
            }
        });

        comp.new_builtin(&["core", "mul"], {
            let lhs = comp.Any();
            IRType::Function {
                args: smallvec![lhs, comp.Any()],
                ret: lhs,
            }
        });

        comp
    }

    fn new_builtin<S: ToString>(&mut self, path: &[S], ty: IRType) -> TypeId {
        let path = self.ctx_mut().path_from(path);
        let ty = self.ctx_mut().new_type(ty);
        self.ctx_mut().builtin_functions.insert(path.clone(), ty);
        ty
    }
    fn ctx(&self) -> std::cell::Ref<Context> {
        self.context.borrow()
    }

    fn ctx_mut(&self) -> std::cell::RefMut<Context> {
        self.context.borrow_mut()
    }

    fn new_type<S: ToString>(&mut self, name: S, ty: IRType) -> TypeId {
        let id = self.ctx_mut().new_type(ty);
        self.set_type(name, id);

        id
    }

    fn add_type(&self, ty: IRType) -> TypeId {
        let id = self.ctx_mut().new_type(ty);
        id
    }

    fn update_type(&self, ty: TypeId, new_ty: IRType) {
        self.ctx_mut().type_table.insert(ty, new_ty);
    }

    fn set_type<S: ToString>(&mut self, name: S, ty: TypeId) {
        let name = self.ctx_mut().intern_string(name);
        self.level.types.insert(name, ty);
    }

    // fn get_type<S: ToString>(&self, name: S) -> Option<TypeId> {
    //     let name = self.ctx_mut().intern_string(name);
    //     self.level.get_type(name)
    // }

    fn get_builtin(&self, path: &Path<InternedString>) -> Option<TypeId> {
        self.ctx().builtin_functions.get(path).copied()
    }

    fn new_struct<S: ToString>(&mut self, path: &[S], span: Span<()>, builtin: bool) -> TypeId {
        let name = path.last().expect("empty path").to_string();
        let ty = {
            let mut ctx = self.ctx_mut();
            let ty = ctx.new_struct(path, span, builtin);
            ty
        };

        self.set_type(name, ty);

        ty
    }

    #[allow(non_snake_case)]
    fn Unit(&self) -> TypeId {
        self.get_type("Unit").expect("Unit type not found")
    }

    #[allow(non_snake_case)]
    fn Bool(&self) -> TypeId {
        self.get_type("Bool").expect("Bool type not found")
    }

    #[allow(non_snake_case)]
    fn Int(&self) -> TypeId {
        self.get_type("Int").expect("Int type not found")
    }

    #[allow(non_snake_case)]
    fn Float(&self) -> TypeId {
        self.get_type("Float").expect("Float type not found")
    }

    #[allow(non_snake_case)]
    fn String(&self) -> TypeId {
        self.get_type("String").expect("String type not found")
    }

    // any is the unresolved type, so make a type for it everytime
    #[allow(non_snake_case)]
    fn Any(&self) -> TypeId {
        self.ctx_mut().new_type(IRType::Any)
    }

    #[allow(non_snake_case)]
    fn Unreachable(&self) -> TypeId {
        self.get_type("!Unreachable")
            .expect("Unreachable type not found")
    }

    fn new_symbol(&mut self, symbol: InternedString) -> VariableId {
        self.level.symbols.add(symbol)
    }

    fn shadow_symbol(&mut self, symbol: InternedString) -> VariableId {
        self.level.symbols.add_force(symbol)
    }

    fn get_value(&self, name: Span<InternedString>, need_init: bool) -> Result<IRValue> {
        match self.level.resolve_local(name.value, need_init) {
            VarState::Ok(var) => Ok(IRValue::Local(var)),
            VarState::Undefined | VarState::Uninitialized => {
                match self.level.resolve_enclosing(name.value, need_init) {
                    VarState::Ok(var) => Ok(IRValue::Upvalue(var)),
                    VarState::Undefined | VarState::Uninitialized => {
                        let real_name = self
                            .ctx()
                            .strings
                            .get(name.value)
                            .expect("invalid interned string")
                            .to_string();
                        Err(IRCompileError::UndefinedVariable(name.span(real_name)))
                    }
                }
            }
        }
    }

    fn declare_local(
        &mut self,
        name: Span<InternedString>,
        ty: Span<TypeId>,
    ) -> Result<Span<VariableId>> {
        let temp = name.value == Self::TEMP_ID;

        for local in self
            .level
            .function
            .params
            .values()
            .chain(self.level.function.locals.values())
            .rev()
        {
            let declared = {
                let meta = local.metadata.borrow();

                // if the variable is dead and in a lower scope, reuse it
                if meta.dead && meta.depth < self.level.scope_depth {
                    break;
                }
                meta.name.as_ref().map(|n| n.value) == Some(name.value)
            };

            if declared {
                // variable already declared, so shadow it
                let id = self.shadow_symbol(name.value);
                self.level.function.locals.insert(
                    id,
                    IRVariable {
                        id,
                        metadata: RefCell::new(IRVariableMetadata {
                            name: Some(name),
                            ty,
                            temp,
                            used: HashSet::new(),
                            assigned: HashSet::new(),
                            dead: false,
                            depth: self.level.scope_depth,
                        }),
                    },
                );

                return Ok(name.span(id));
            }
        }

        let id = self.new_symbol(name.value);
        self.level.function.locals.insert(
            id,
            IRVariable {
                id,
                metadata: RefCell::new(IRVariableMetadata {
                    name: Some(name),
                    ty,
                    temp,
                    used: HashSet::new(),
                    assigned: HashSet::new(),
                    dead: false,
                    depth: self.level.scope_depth,
                }),
            },
        );

        Ok(name.span(id))
    }

    fn temp(&mut self, ty: Span<TypeId>) -> Result<Span<VariableId>> {
        self.declare_local(ty.span(Self::TEMP_ID), ty)
    }

    fn get_local(&self, id: VariableId) -> &IRVariable {
        self.level.function.locals.get(&id).expect("no variable")
    }

    fn push_statement(&mut self, mut statement: IRStatement) {
        let var = self
            .level
            .function
            .locals
            .get_mut(&statement.target)
            .expect("no variable");

        var.metadata.borrow_mut().assigned.insert(statement.id);

        match &statement.expression {
            IRExpression::Value(IRValue::Local(id)) => {
                let var = self.level.function.locals.get_mut(id).expect("no variable");
                var.metadata.borrow_mut().used.insert(statement.id);
            }
            IRExpression::Value(_) => {}
            IRExpression::Call { .. } => {}
        }

        let statements = &mut self
            .level
            .function
            .blocks
            .last_mut()
            .expect("no blocks")
            .1
            .statements;

        let id = statements.len();

        statement.id = id;

        statements.push(statement);
    }

    pub fn compile(&mut self, statements: &[Span<Statement>]) -> Result<()> {
        for statement in statements {
            self.compile_statement(statement, None)?;
        }

        Ok(())
    }

    fn compile_type(&mut self, ty: &Option<Span<Type>>) -> Result<Span<TypeId>> {
        let Some(ty) = ty else {
            return Ok(Span::default(self.Any()));
        };

        match &ty.value {
            Type::Any => Ok(ty.span(self.Any())),
            Type::Named(path) => match self.get_type(name) {
                Some(t) => Ok(ty.span(t)),
                None => Err(IRCompileError::UnkownType(ty.span(name.to_string()))),
            },
            // Type::Generic { name, .. } => {}
            _ => todo!("compile_type {:?}", ty),
        }
    }

    fn compile_statement(
        &mut self,
        Span { start, end, value }: &Span<Statement>,
        target: Option<Span<VariableId>>,
    ) -> Result<()> {
        match value {
            Statement::Declaration {
                name,
                is_mutable,
                ty,
                value,
            } => {
                let ty = self.compile_type(ty)?;
                let name = name.span(self.ctx_mut().intern_string(&name.value));
                let target = self.declare_local(name, ty)?;

                self.compile_expression(value, Some(target))?;

                Ok(())
            }
            Statement::Assignment { name, value } => {
                let name = name.span(self.ctx_mut().intern_string(&name.value));
                let target = self.get_value(name, true)?;

                let target = match target {
                    IRValue::Local(id) => id,
                    IRValue::Upvalue(id) => id,
                    _ => panic!("invalid value"),
                };

                self.compile_expression(value, Some(name.span(target)))?;

                Ok(())
            }
            Statement::WhileLoop { condition, body } => todo!(),
            Statement::Struct { name, fields } => todo!(),
            Statement::Expression(_) => todo!(),
            Statement::Function(_) => todo!(),
        }
    }

    fn compile_expression(
        &mut self,
        Span { start, end, value }: &Span<Expression>,
        target: Option<Span<VariableId>>,
    ) -> Result<Option<IRValue>> {
        match value {
            Expression::Literal(lit) => {
                let constant = match lit {
                    Literal::Unit => IRConstant::Unit,
                    Literal::Bool(b) => IRConstant::Bool(*b),
                    Literal::Int(i) => IRConstant::Int(*i),
                    Literal::Float(f) => IRConstant::Float(*f),
                    Literal::String(s) => IRConstant::String(self.ctx_mut().intern_string(s)),
                };

                let value = IRValue::Constant(Span {
                    start: *start,
                    end: *end,
                    value: constant,
                });

                if let Some(target) = target {
                    let statement = IRStatement::new(
                        target.value,
                        IRExpression::Value(value),
                        Some(target.span(())),
                    );

                    self.push_statement(statement);

                    return Ok(None);
                }

                Ok(Some(value))
            }
            Expression::Symbol(symbol) => {
                let sym = self.ctx_mut().intern_string(symbol);
                let value = self.get_value(
                    Span {
                        start: *start,
                        end: *end,
                        value: sym,
                    },
                    true,
                )?;

                if let Some(target) = target {
                    let statement = IRStatement::new(
                        target.value,
                        IRExpression::Value(value),
                        Some(target.span(())),
                    );

                    self.push_statement(statement);

                    return Ok(None);
                }

                Ok(Some(value))
            }
            Expression::FunctionCall { func, args } => todo!(),
            Expression::Parentheses(_) => todo!(),
            Expression::Binary { op, lhs, rhs } => {
                // we can unwrap because we provided no targets
                let lhs = self.compile_expression(lhs, None)?.unwrap();
                let rhs = self.compile_expression(rhs, None)?.unwrap();
                let func = self.compile_binary_op(op);

                let target = match target {
                    Some(target) => target,
                    None => self.temp(Span {
                        start: *start,
                        end: *end,
                        value: self.Any(),
                    })?,
                };

                self.push_statement(IRStatement::new(
                    target.value,
                    IRExpression::Call {
                        function: func,
                        args: smallvec![lhs, rhs],
                    },
                    Some(target.span(())),
                ));

                Ok(Some(IRValue::Local(target.value)))
            }
            Expression::Block { statements, ret } => todo!(),
            Expression::If {
                condition,
                then,
                otherwise,
            } => todo!(),
            Expression::Return(_) => todo!(),
            Expression::Access { expr, field } => todo!(),
        }
    }

    fn compile_binary_op(&self, op: &BinaryOp) -> IRValue {
        match op {
            BinaryOp::Add => IRValue::BuiltinFunction(self.ctx_mut().path_from(&["core", "add"])),
            BinaryOp::Mul => IRValue::BuiltinFunction(self.ctx_mut().path_from(&["core", "mul"])),
            _ => todo!(),
        }
    }
}
