pub mod dump;
pub mod passes;
pub mod pathmap;
pub mod pool;

use std::{cell::RefCell, hash::Hash};

use hashbrown::HashSet;
use indexmap::IndexMap;
use pathmap::{Path, PathMap};
use pool::Pool;
use smallvec::{smallvec, SmallVec};

use crate::parser::{scanner::Span, BinaryOp, Expression, Function, Literal, Statement, Type};

pub type InternedString = usize;
pub type TypeId = usize;
pub type VariableId = usize;

pub type BlockId = usize;
pub type StatementId = usize;

pub struct Context {
    pub type_table: IndexMap<TypeId, IRType>,
    next_type_id: TypeId,

    strings: RefCell<Pool<String, InternedString>>,

    pub types: PathMap<InternedString, TypeId>,
    pub functions: PathMap<InternedString, IRFunction>,
    pub structs: PathMap<InternedString, IRStruct>,
    pub builtin_functions: PathMap<InternedString, TypeId>,
}

type Tpath = &'static [&'static str];
pub const ANY: Tpath = &["Any"];
pub const UNREACHABLE: Tpath = &["core", "!Unreachable"];
pub const UNIT: Tpath = &["Unit"];
pub const BOOL: Tpath = &["Bool"];
pub const INT: Tpath = &["Int"];
pub const FLOAT: Tpath = &["Float"];
pub const STRING: Tpath = &["String"];

impl Context {
    pub fn new() -> Self {
        Context {
            type_table: IndexMap::new(),
            next_type_id: 0,

            strings: {
                // id 0 will always be an empty string
                let mut strings = Pool::new();
                strings.add("@temp".to_string());
                RefCell::new(strings)
            },

            types: PathMap::new(),
            functions: PathMap::new(),
            structs: PathMap::new(),
            builtin_functions: PathMap::new(),
        }
    }

    pub fn strings(&self) -> std::cell::Ref<Pool<String, InternedString>> {
        self.strings.borrow()
    }

    pub fn strings_mut(&self) -> std::cell::RefMut<Pool<String, InternedString>> {
        self.strings.borrow_mut()
    }

    pub fn path_from<S: ToString>(&self, path: &[S]) -> Path<InternedString> {
        Path::from_iter(path.iter().map(|part| self.intern_string(part.to_string())))
    }

    pub fn get_type(&self, path: &Path<InternedString>) -> Option<TypeId> {
        self.types.get(path).copied()
    }

    pub fn get_type_from<S: ToString>(&self, path: &[S]) -> Option<TypeId> {
        self.get_type(&self.path_from(path))
    }

    pub fn new_struct<S: ToString>(&mut self, path: &[S], span: Span<()>, builtin: bool) -> TypeId {
        let path = self.path_from(path);
        let ty = self.new_rigid_type(path.clone(), IRTypeInner::Named(path.clone()));

        let strct = IRStruct {
            path: span.span(path.clone()),
            ty,

            fields: Vec::new(),
            methods: Vec::new(),

            builtin,
        };

        self.structs.insert(path.clone(), strct);

        ty
    }

    pub fn intern_string<S: ToString>(&self, string: S) -> InternedString {
        self.strings.borrow_mut().add(string.to_string())
    }

    pub fn re_intern_string<S: ToString>(&self, string: S) -> InternedString {
        self.strings.borrow_mut().add_force(string.to_string())
    }

    pub fn new_type(&mut self, ty: IRType) -> TypeId {
        let id = self.next_type_id;
        self.next_type_id += 1;
        self.type_table.insert(id, ty);
        id
    }

    pub fn new_rigid_type(&mut self, path: Path<InternedString>, ty: IRTypeInner) -> TypeId {
        let ty = self.new_type(IRType::Rigid(ty));

        self.types.insert(path, ty);

        ty
    }
    pub fn new_free_type(&mut self, ty: IRTypeInner) -> TypeId {
        self.new_type(IRType::Free(ty))
    }

    // expects the ty to exist
    // given a type, if it is rigid, return a new free one, if it is free just return it
    fn free_type(&mut self, ty_id: TypeId) -> TypeId {
        let ty = self.type_table.get(&ty_id).expect("Type not found");
        match ty {
            IRType::Rigid(ty) => self.new_free_type(ty.clone()),
            IRType::Free(_) => ty_id,
        }
    }

    #[allow(non_snake_case)]
    pub fn Unit(&self) -> TypeId {
        self.get_type_from(UNIT).expect("Unit type not found")
    }

    #[allow(non_snake_case)]
    pub fn Bool(&self) -> TypeId {
        self.get_type_from(BOOL).expect("Bool type not found")
    }

    #[allow(non_snake_case)]
    pub fn Int(&self) -> TypeId {
        self.get_type_from(INT).expect("Int type not found")
    }

    #[allow(non_snake_case)]
    pub fn Float(&self) -> TypeId {
        self.get_type_from(FLOAT).expect("Float type not found")
    }

    #[allow(non_snake_case)]
    pub fn String(&self) -> TypeId {
        self.get_type_from(STRING).expect("String type not found")
    }

    // any is the unresolved type, so make a type for it everytime
    #[allow(non_snake_case)]
    pub fn AnyRigid(&self) -> TypeId {
        self.get_type_from(ANY).expect("Any type not found")
    }

    #[allow(non_snake_case)]
    pub fn Any(&mut self) -> TypeId {
        self.free_type(self.AnyRigid())
    }

    #[allow(non_snake_case)]
    fn Unreachable(&self) -> TypeId {
        self.get_type_from(UNREACHABLE)
            .expect("Unreachable type not found")
    }
}

// non rigid types can be updated later on to less or more specific types
// during inference, rigid types will raise a type error if they are not the same

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum IRType {
    Rigid(IRTypeInner),
    Free(IRTypeInner),
}

impl IRType {
    pub fn is_rigid(&self) -> bool {
        matches!(self, IRType::Rigid(_))
    }

    pub fn inner(&self) -> &IRTypeInner {
        match self {
            IRType::Rigid(ty) => ty,
            IRType::Free(ty) => ty,
        }
    }

    pub fn to_inner(self) -> IRTypeInner {
        match self {
            IRType::Rigid(ty) => ty,
            IRType::Free(ty) => ty,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum IRTypeInner {
    Unreachable,

    Any,
    Named(Path<InternedString>),

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

impl IRValue {
    #[allow(non_snake_case)]
    fn Unit() -> Self {
        IRValue::Constant(Span::default(IRConstant::Unit))
    }
}

pub enum IRExpression {
    Value(IRValue),
    Call {
        function: IRValue,
        args: SmallVec<[IRValue; 4]>,

        // resolved function types after infrence
        resolved_function: RefCell<Option<TypeId>>,
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
    pub initialized: bool,                  // is this variable initialized?
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
    None,
    Goto(BlockId),
    Branch {
        condition: IRValue,
        success: BlockId,
        otherwise: BlockId,
    },
    Return(VariableId), // should always be 0
}

pub enum IRFunction {
    Building(TypeId),
    Finished(IRFunctionInner),
}

impl IRFunction {
    pub fn finished(&self) -> &IRFunctionInner {
        match self {
            IRFunction::Building(_) => panic!("function not finished"),
            IRFunction::Finished(func) => func,
        }
    }

    pub fn ty(&self) -> TypeId {
        match self {
            IRFunction::Building(ty) => *ty,
            IRFunction::Finished(func) => func.ty,
        }
    }
}

pub struct IRFunctionInner {
    pub path: Span<Path<InternedString>>,
    pub ty: TypeId,

    pub params: IndexMap<VariableId, IRVariable>,
    pub locals: IndexMap<VariableId, IRVariable>,
    pub blocks: IndexMap<BlockId, Block>,

    next_block_id: BlockId,
}

impl IRFunctionInner {
    pub fn new(path: Span<Path<InternedString>>, ty: TypeId) -> Self {
        IRFunctionInner {
            path,
            ty,
            params: IndexMap::new(),
            locals: IndexMap::new(),
            blocks: {
                let mut blocks = IndexMap::new();
                blocks.insert(
                    0,
                    Block {
                        id: 0,
                        statements: Vec::new(),
                        control_flow: ControlFlow::Return(0),
                    },
                );
                blocks
            },
            next_block_id: 1,
        }
    }

    pub fn enter_block(&mut self, force: bool) -> BlockId {
        let Some(block) = self.blocks.values_mut().last() else {
            panic!("no blocks");
        };

        if !force && block.statements.is_empty() {
            return block.id;
        }

        let id = self.next_block_id;
        block.control_flow = ControlFlow::Goto(id);

        self.next_block_id += 1;
        self.blocks.insert(
            id,
            Block {
                id,
                statements: Vec::new(),
                control_flow: ControlFlow::Return(0),
            },
        );

        id
    }

    pub fn get_local(&self, id: &VariableId) -> &IRVariable {
        match self.locals.get(id) {
            Some(local) => local,
            None => self.params.get(id).expect("no variable"),
        }
    }

    pub fn get_local_mut(&mut self, id: &VariableId) -> &mut IRVariable {
        match self.locals.get_mut(id) {
            Some(local) => local,
            None => self.params.get_mut(id).expect("no variable"),
        }
    }
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
    pub function: IRFunctionInner,
    pub scope_depth: usize,

    // variable name -> variable
    pub symbols: Pool<InternedString, VariableId>,
    // type name -> type
    // pub types: HashMap<InternedString, TypeId>,
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

            if initialized && !meta.initialized {
                vs = VarState::Uninitialized;
                continue;
            }

            if meta.name.as_ref().map(|n| n.value) == Some(name) {
                return VarState::Ok(local.id);
            }
        }

        return vs;
    }
}

#[derive(Debug)]
pub enum IRCompileError {
    UnkownType(Span<Vec<String>>),
    UndefinedVariable(Span<String>),
    UninitializedVariable(Span<String>),
    RedeclaredParameter(Span<String>),
}

type Result<T> = std::result::Result<T, IRCompileError>;

macro_rules! builtins {
    {
        [$comp:ident]
        $(
            fn $($path:ident)::+$(<$($generic:ident),+>)?($($arg_ty:ident),*) -> $ret_ty:ident;
        )*
    } => {
        #[allow(non_snake_case)]
        {
            $(
                $comp.new_builtin(&[$(stringify!($path)),+], {
                    let mut ctx = $comp.ctx();
                    $($(let $generic = ctx.Any();)+)?

                    IRTypeInner::Function {
                        args: SmallVec::from_slice(&[$(builtins!(@type ctx $arg_ty),)*]),
                        ret: builtins!(@type ctx $ret_ty),
                    }
                });
            )*
        }
    };

    {@type $ctx:ident Any} => { $ctx.AnyRigid() };
    {@type $ctx:ident Unit} => { $ctx.Unit() };
    {@type $ctx:ident Bool} => { $ctx.Bool() };
    {@type $ctx:ident Int} => { $ctx.Int() };
    {@type $ctx:ident Float} => { $ctx.Float() };
    {@type $ctx:ident String} => { $ctx.String() };
    {@type $ctx:ident $ty:ident} => { $ty };
}

pub struct IRCompiler {
    level: Level,

    pub context: RefCell<Context>,
    pub passes: Vec<fn(&mut IRFunctionInner, &mut Context)>,
}

impl IRCompiler {
    const TEMP_ID: InternedString = 0;
    const RETURN_ID: VariableId = 0;

    pub fn new() -> Self {
        let mut comp = IRCompiler {
            level: Level {
                enclosing: None,
                function: IRFunctionInner::new(Span::default(Path::new()), 0),
                scope_depth: 0,
                symbols: Pool::new(),
            },
            context: RefCell::new(Context::new()),
            passes: Vec::new(),
        };

        // adding directly to context, makes it unaccessible
        // comp.ctx_mut().new_type(IRType::Any);
        comp.new_rigid_type(&["core", "!Unreachable"], IRTypeInner::Unreachable);
        comp.new_rigid_type(&["Any"], IRTypeInner::Any);

        let unit = comp.ctx().new_struct(&["Unit"], Span::default(()), true);
        comp.ctx().new_struct(&["Bool"], Span::default(()), true);
        comp.ctx().new_struct(&["Int"], Span::default(()), true);
        comp.ctx().new_struct(&["Float"], Span::default(()), true);
        comp.ctx().new_struct(&["String"], Span::default(()), true);

        let main = comp.ctx().path_from(&["entry", "main"]);
        let fn_type = comp.ctx().new_rigid_type(
            main.clone(),
            IRTypeInner::Function {
                args: SmallVec::new(),
                ret: unit,
            },
        );
        comp.level.function.path = Span::default(main);
        comp.level.function.ty = fn_type;

        // create a temp variable for the return value
        comp.temp(Span::default(unit))
            .expect("failed to create temp");

        builtins! {
            [comp]

            fn core::add<T>(T, T) -> T;
            fn core::mul<T>(T, T) -> T;
            fn core::sub<T>(T, T) -> T;
            fn core::div<T>(T, T) -> T;

            fn core::eq(Any, Any) -> Bool;
            fn core::lt<T>(T, T) -> Bool;

            fn print(Any) -> Unit;
            fn println(Any) -> Unit;
        }

        comp
    }

    pub fn pass(&mut self, pass: fn(&mut IRFunctionInner, &mut Context)) {
        self.passes.push(pass);
    }

    fn new_builtin<S: ToString>(&mut self, path: &[S], ty: IRTypeInner) -> TypeId {
        let path = self.ctx_ref().path_from(path);
        let ty = self.ctx().new_rigid_type(path.clone(), ty);
        self.ctx().builtin_functions.insert(path, ty);
        ty
    }
    fn ctx_ref(&self) -> std::cell::Ref<Context> {
        self.context.borrow()
    }

    fn ctx(&self) -> std::cell::RefMut<Context> {
        self.context.borrow_mut()
    }

    fn intern(&self, string: &str) -> InternedString {
        self.ctx().intern_string(string)
    }

    fn new_rigid_type<S: ToString>(&mut self, path: &[S], ty: IRTypeInner) -> TypeId {
        let path = self.ctx().path_from(path);
        self.ctx().new_rigid_type(path, ty)
    }

    fn add_type(&self, ty: IRType) -> TypeId {
        let id = self.ctx().new_type(ty);
        id
    }

    fn update_type(&self, ty: TypeId, new_ty: IRType) {
        self.ctx().type_table.insert(ty, new_ty);
    }

    fn set_type(&mut self, path: Path<InternedString>, ty: TypeId) {
        self.ctx().types.insert(path, ty);
    }

    // fn get_type<S: ToString>(&self, name: S) -> Option<TypeId> {
    //     let name = self.ctx_mut().intern_string(name);
    //     self.level.get_type(name)
    // }

    fn get_builtin(&self, path: &Path<InternedString>) -> Option<TypeId> {
        self.ctx_ref().builtin_functions.get(path).copied()
    }

    // fn new_struct<S: ToString>(&mut self, path: &[S], span: Span<()>, builtin: bool) -> TypeId {
    //     let name = path.last().expect("empty path").to_string();
    //     let ty = {
    //         let mut ctx = self.ctx_mut();
    //         let ty = ctx.new_struct(path, span, builtin);
    //         ty
    //     };

    //     self.set_type(name, ty);

    //     ty
    // }

    #[allow(non_snake_case)]
    fn Unit(&self) -> TypeId {
        self.ctx_ref().Unit()
    }

    #[allow(non_snake_case)]
    fn Bool(&self) -> TypeId {
        self.ctx_ref().Bool()
    }

    #[allow(non_snake_case)]
    fn Int(&self) -> TypeId {
        self.ctx_ref().Int()
    }

    #[allow(non_snake_case)]
    fn Float(&self) -> TypeId {
        self.ctx_ref().Float()
    }

    #[allow(non_snake_case)]
    fn String(&self) -> TypeId {
        self.ctx_ref().String()
    }

    // any is the unresolved type, so make a type for it everytime
    #[allow(non_snake_case)]
    fn AnyRigid(&self) -> TypeId {
        self.ctx_ref().AnyRigid()
    }

    #[allow(non_snake_case)]
    fn Any(&mut self) -> TypeId {
        self.ctx().Any()
    }

    #[allow(non_snake_case)]
    fn Unreachable(&self) -> TypeId {
        self.ctx_ref().Unreachable()
    }

    // runs passes and pushes a function
    fn push_function(&self, mut function: IRFunctionInner) {
        let path = function.path.value.clone();

        let mut ctx = self.ctx();
        for pass in &self.passes {
            pass(&mut function, &mut ctx);
        }

        ctx.functions.insert(path, IRFunction::Finished(function));
    }

    fn enter_level(&mut self, level: Level) {
        let level = std::mem::replace(&mut self.level, level);
        self.level.enclosing = Some(Box::new(level));
    }

    fn exit_level(&mut self) {
        let enclosing: Box<Level> = self.level.enclosing.take().expect("no enclosing level");
        let level = std::mem::replace(&mut self.level, *enclosing);
        self.push_function(level.function);
    }

    fn exit_outmost_level(mut self) -> Result<Context> {
        {
            let path = self.level.function.path.value.clone();
            let mut ctx = self.context.borrow_mut();
            for pass in &self.passes {
                pass(&mut self.level.function, &mut ctx);
            }
            ctx.functions
                .insert(path, IRFunction::Finished(self.level.function));
        }

        Ok(self.context.into_inner())
    }

    fn return_value(&mut self) {
        let current_block = self.current_block();
        self.enter_block(true);

        self.patch_block(current_block, ControlFlow::Return(Self::RETURN_ID));

        self.exit_block();
    }

    fn enter_function(&mut self, raw_path: &Span<RefCell<Vec<String>>>, ty: IRTypeInner) {
        let ret_ty = match ty {
            IRTypeInner::Function { ret, .. } => ret,
            _ => panic!("not a function type"),
        };

        let path = self.ctx().path_from(&raw_path.value.borrow());

        let ty = self.ctx().new_rigid_type(path.clone(), ty);

        self.ctx()
            .functions
            .insert(path.clone(), IRFunction::Building(ty));

        let function = IRFunctionInner::new(raw_path.span(path), ty);

        self.enter_level(Level {
            enclosing: None,
            function,
            scope_depth: self.level.scope_depth + 1,
            symbols: Pool::new(),
        });

        // create a temp variable for the return value
        self.temp(raw_path.span(ret_ty))
            .expect("failed to create temp");
    }

    fn exit_function(&mut self) {
        self.exit_level();
    }

    fn enter_scope(&mut self) {
        self.level.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        self.level.scope_depth -= 1;

        for local in self.level.function.locals.values_mut() {
            let mut meta = local.metadata.borrow_mut();
            if meta.depth > self.level.scope_depth {
                meta.dead = true;
            }
        }
    }

    fn current_block(&self) -> BlockId {
        self.level
            .function
            .blocks
            .keys()
            .last()
            .copied()
            .expect("no blocks")
    }

    fn next_block(&self) -> BlockId {
        self.level.function.next_block_id
    }

    fn patch_block(&mut self, id: BlockId, control_flow: ControlFlow) {
        let block = self.level.function.blocks.get_mut(&id).expect("no block");
        block.control_flow = control_flow;
    }

    fn enter_block(&mut self, force: bool) -> BlockId {
        self.level.function.enter_block(force)
    }

    fn exit_block(&mut self) -> BlockId {
        self.level.function.enter_block(false)
    }

    fn enter_scoped_block(&mut self) -> BlockId {
        self.enter_scope();
        self.enter_block(false)
    }

    fn exit_scoped_block(&mut self) -> BlockId {
        self.exit_scope();
        self.exit_block()
    }

    fn new_symbol(&mut self, symbol: InternedString) -> VariableId {
        self.level.symbols.add(symbol)
    }

    fn exclusive_symbol(&mut self, symbol: InternedString) -> Option<VariableId> {
        self.level.symbols.add_checked(symbol)
    }

    fn shadow_symbol(&mut self, symbol: InternedString) -> VariableId {
        self.level.symbols.add_force(symbol)
    }

    fn get_function_value(&self, name: Span<InternedString>) -> Result<IRValue> {
        let path = Path::from(&[name.value]);

        match self.ctx_ref().functions.get(&path) {
            Some(_) => Ok(IRValue::Function(path)),
            None => match self.ctx_ref().builtin_functions.get(&path) {
                Some(_) => Ok(IRValue::BuiltinFunction(path)),
                None => {
                    let real_name = self
                        .ctx_ref()
                        .strings()
                        .get(name.value)
                        .expect("invalid interned string")
                        .to_string();
                    Err(IRCompileError::UndefinedVariable(name.span(real_name)))
                }
            },
        }
    }

    fn get_value(&self, name: Span<InternedString>, need_init: bool) -> Result<IRValue> {
        match self.level.resolve_local(name.value, need_init) {
            VarState::Ok(var) => Ok(IRValue::Local(var)),
            VarState::Undefined | VarState::Uninitialized => {
                match self.level.resolve_enclosing(name.value, need_init) {
                    VarState::Ok(var) => Ok(IRValue::Upvalue(var)),
                    VarState::Undefined | VarState::Uninitialized => self.get_function_value(name),
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
                            initialized: false,
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
                    initialized: false,
                }),
            },
        );

        Ok(name.span(id))
    }

    fn declare_param(
        &mut self,
        name: Span<InternedString>,
        ty: Span<TypeId>,
    ) -> Result<Span<VariableId>> {
        let id = self.exclusive_symbol(name.value).ok_or_else(|| {
            let real_name = self
                .ctx_ref()
                .strings()
                .get(name.value)
                .expect("invalid interned string")
                .to_string();
            IRCompileError::RedeclaredParameter(name.span(real_name))
        })?;

        self.level.function.params.insert(
            id,
            IRVariable {
                id,
                metadata: RefCell::new(IRVariableMetadata {
                    name: Some(name),
                    ty,
                    temp: false,
                    used: HashSet::new(),
                    assigned: HashSet::new(),
                    dead: false,
                    depth: self.level.scope_depth,
                    initialized: true,
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
        var.metadata.borrow_mut().initialized = true;

        match &statement.expression {
            IRExpression::Value(IRValue::Local(id)) => {
                let var = self.level.function.get_local_mut(id);
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

    pub fn compile(mut self, statements: &[Span<Statement>]) -> Result<Context> {
        for statement in statements {
            self.compile_statement(statement, None)?;
        }

        self.exit_outmost_level()
    }

    fn get_type<S: ToString>(&self, path: &[S]) -> Option<TypeId> {
        self.ctx_ref().get_type_from(path)
    }

    fn compile_type(&mut self, ty: &Option<Span<Type>>) -> Result<Span<TypeId>> {
        let Some(ty) = ty else {
            return Ok(Span::default(self.Any()));
        };

        match &ty.value {
            Type::Any => Ok(ty.span(self.AnyRigid())),
            Type::Named(path) => match self.get_type(&path.borrow()) {
                Some(t) => Ok(ty.span(t)),
                None => Err(IRCompileError::UnkownType(ty.span(path.borrow().clone()))),
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
                let name = name.span(self.intern(&name.value));
                let target = self.declare_local(name, ty)?;

                self.compile_expression(value, Some(target))?;

                Ok(())
            }
            Statement::Assignment { name, value } => {
                let name = name.span(self.intern(&name.value));
                let target = self.get_value(name, true)?;

                let target = match target {
                    IRValue::Local(id) => id,
                    IRValue::Upvalue(_) => todo!("tried to use captured variable"),
                    _ => panic!("invalid value"),
                };

                self.compile_expression(value, Some(name.span(target)))?;

                Ok(())
            }

            Statement::Expression(expr) => {
                self.compile_expression(expr, target)?;
                Ok(())
            }
            Statement::Function(Function {
                function_kind,
                path,
                params,
                return_type,
                body,
            }) => {
                let param_tys: Vec<Span<TypeId>> = params
                    .iter()
                    .map(|param| self.compile_type(&param.value.ty))
                    .collect::<Result<_>>()?;

                let return_ty = self.compile_type(return_type)?;

                let ty = IRTypeInner::Function {
                    args: param_tys.iter().map(|ty| ty.value).collect(),
                    ret: return_ty.value,
                };

                self.enter_function(path, ty);

                for (param, ty) in params.iter().zip(param_tys) {
                    let name = param.value.name.span(self.intern(&param.value.name.value));
                    self.declare_param(name, ty)?;
                }

                for statement in &body.value.statements {
                    self.compile_statement(statement, None)?;
                }

                if let Some(ret) = &body.value.ret {
                    let ret_target = ret.span(Self::RETURN_ID);
                    self.compile_expression(ret, Some(ret_target))?;
                }

                self.exit_function();

                Ok(())
            }
            Statement::WhileLoop { condition, body } => todo!(),
            Statement::Struct { name, fields } => todo!(),
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
                    Literal::String(s) => IRConstant::String(self.intern(s)),
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
                let sym = self.intern(symbol);
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
            Expression::FunctionCall { func, args } => {
                let func = self.compile_expression(func, None)?.unwrap();
                let mut arguments = SmallVec::with_capacity(args.len());
                for arg in args {
                    // we can unwrap because we provided no targets
                    let arg = self.compile_expression(arg, None)?.unwrap();
                    arguments.push(arg);
                }

                let target = match target {
                    Some(target) => target,
                    None => {
                        let any = self.Any();
                        self.temp(Span {
                            start: *start,
                            end: *end,
                            value: any,
                        })?
                    }
                };

                self.push_statement(IRStatement::new(
                    target.value,
                    IRExpression::Call {
                        function: func,
                        args: arguments,
                        resolved_function: RefCell::new(None),
                    },
                    Some(target.span(())),
                ));

                Ok(Some(IRValue::Local(target.value)))
            }
            Expression::Parentheses(expr) => self.compile_expression(expr, target),
            Expression::Binary { op, lhs, rhs } => {
                // we can unwrap because we provided no targets
                let lhs = self.compile_expression(lhs, None)?.unwrap();
                let rhs = self.compile_expression(rhs, None)?.unwrap();
                let func = self.compile_binary_op(op);

                let target = match target {
                    Some(target) => target,
                    None => {
                        let any = self.Any();
                        self.temp(Span {
                            start: *start,
                            end: *end,
                            value: any,
                        })?
                    }
                };

                self.push_statement(IRStatement::new(
                    target.value,
                    IRExpression::Call {
                        function: func,
                        args: smallvec![lhs, rhs],
                        resolved_function: RefCell::new(None),
                    },
                    Some(target.span(())),
                ));

                Ok(Some(IRValue::Local(target.value)))
            }
            Expression::Block { statements, ret } => {
                self.enter_scoped_block();

                for statement in statements {
                    self.compile_statement(statement, None)?;
                }

                if let Some(ret) = ret {
                    let ret = self.compile_expression(ret, target)?;
                    self.exit_scoped_block();
                    return Ok(ret);
                }

                let ret = IRValue::Constant(Span {
                    start: *start,
                    end: *end,
                    value: IRConstant::Unit,
                });

                let target = match target {
                    Some(target) => target,
                    None => self.temp(Span {
                        start: *start,
                        end: *end,
                        value: self.Unit(),
                    })?,
                };

                self.push_statement(IRStatement::new(
                    target.value,
                    IRExpression::Value(ret),
                    Some(target.span(())),
                ));

                self.exit_scoped_block();

                Ok(Some(IRValue::Local(target.value)))
            }
            Expression::If {
                condition,
                then,
                otherwise: other,
            } => {
                let curr_block = self.current_block();
                let condition = self.compile_expression(condition, None)?.unwrap();

                let target = match target {
                    Some(target) => target,
                    None => self.temp(Span {
                        start: *start,
                        end: *end,
                        value: self.Unit(),
                    })?,
                };

                let success = self.enter_block(true);
                self.compile_expression(then, Some(target))?;
                let sucess_end = self.exit_block();

                let mut otherwise = None;
                let end;
                if let Some(other) = other {
                    otherwise = Some(self.enter_block(true));
                    self.compile_expression(other, Some(target))?;
                    end = self.enter_block(false);
                } else {
                    end = self.enter_block(true);
                }

                self.patch_block(
                    curr_block,
                    ControlFlow::Branch {
                        condition,
                        success,
                        otherwise: otherwise.unwrap_or(end),
                    },
                );

                self.patch_block(sucess_end, ControlFlow::Goto(end));

                Ok(Some(IRValue::Local(target.value)))
            }
            Expression::Return(expr) => {
                let target = Span {
                    start: *start,
                    end: *end,
                    value: Self::RETURN_ID,
                };

                if let Some(expr) = expr {
                    self.compile_expression(expr, Some(target))?;
                } else {
                    let unit = IRValue::Unit();
                    self.push_statement(IRStatement::new(
                        Self::RETURN_ID,
                        IRExpression::Value(unit),
                        Some(Span {
                            start: *start,
                            end: *end,
                            value: (),
                        }),
                    ));
                }

                self.return_value();

                Ok(None)
            }
            Expression::Access { expr, field } => todo!(),
        }
    }

    fn compile_binary_op(&self, op: &BinaryOp) -> IRValue {
        let path = match op {
            BinaryOp::Add => &["core", "add"],
            BinaryOp::Mul => &["core", "mul"],
            BinaryOp::Sub => &["core", "sub"],
            BinaryOp::Div => &["core", "div"],

            BinaryOp::Equals => &["core", "eq"],
            BinaryOp::Less => &["core", "lt"],
            _ => todo!(),
        };
        IRValue::BuiltinFunction(self.ctx_ref().path_from(path))
    }
}
