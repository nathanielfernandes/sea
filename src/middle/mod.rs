use std::{cell::RefCell, hash::Hash};

use hashbrown::{HashMap, HashSet};

use crate::{compiler::pool::Pool, parser::scanner::Span};

pub type InternedString = usize;
pub type TypeId = usize;
pub type VariableId = usize;

pub type BlockId = usize;
pub type StatementId = usize;

#[derive(Debug, Clone, Hash)]
pub struct IRPath(Vec<InternedString>);

impl IRPath {
    pub fn name(&self) -> InternedString {
        *self.0.last().expect("empty path")
    }

    pub fn add(&mut self, part: InternedString) {
        self.0.push(part);
    }
}

pub struct Context {
    pub types: HashMap<TypeId, IRType>,
    next_type_id: TypeId,

    pub strings: Pool<String, InternedString>,
    pub functions: HashMap<IRPath, IRFunction>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            types: HashMap::new(),
            next_type_id: 0,

            strings: Pool::new(),
            functions: HashMap::new(),
        }
    }

    pub fn path_from<S: ToString>(&mut self, path: &[S]) -> IRPath {
        IRPath(
            path.iter()
                .map(|part| self.intern_string(part.to_string()))
                .collect(),
        )
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
        self.types.insert(id, ty);
        id
    }
}

#[derive(Debug, Clone, Hash)]
pub enum IRType {
    Any,
    Unreachable,

    Unit,
    Bool,
    Int,
    Float,
    String,

    Named(InternedString),
    Function { args: Vec<TypeId>, ret: TypeId },
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
}

pub enum IRExpression {
    Value(IRValue),
    Call {
        function: IRValue,
        args: Vec<IRValue>,
    },
}

pub struct IRVariableMetadata {
    pub path: Option<Span<IRPath>>,     // variables may not have names
    pub ty: Span<TypeId>,               // span points to where the type was declared
    pub temp: bool,                     // is this a temporary variable?
    pub used: HashSet<StatementId>,     // statements that use this variable
    pub assigned: HashSet<StatementId>, // statements that assign to this variable
}

pub struct IRVariable {
    pub id: VariableId,
    pub metadata: RefCell<IRVariableMetadata>,
}

impl IRVariable {
    pub fn new(id: VariableId, name: Option<Span<IRPath>>, ty: Span<TypeId>) -> IRVariable {
        IRVariable {
            id,
            metadata: RefCell::new(IRVariableMetadata {
                path: name,
                ty,
                temp: false,
                used: HashSet::new(),
                assigned: HashSet::new(),
            }),
        }
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
        id: StatementId,
        target: VariableId,
        expression: IRExpression,
        span: Option<Span<()>>,
    ) -> IRStatement {
        IRStatement {
            id,
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
    Goto(BlockId),
    Branch {
        condition: VariableId,
        success: BlockId,
        otherwise: BlockId,
    },
    Return(VariableId),
}

pub struct IRFunction {
    pub path: Span<IRPath>,
    pub ty: TypeId,

    pub params: Vec<VariableId>,
    pub locals: Vec<VariableId>,
    pub blocks: Vec<Block>,
}

pub struct Method {
    pub path: Span<IRPath>,
    pub ty: TypeId,
}

pub struct IRStruct {
    pub path: Span<IRPath>,
    pub ty: TypeId,

    pub fields: Vec<Span<TypeId>>,
    pub methods: Vec<Method>,
}

pub struct Level {
    pub enclosing: Option<Box<Level>>,
    pub function: IRFunction,
    pub scope_depth: usize,
    pub identifiers: Pool<InternedString, VariableId>,
}

pub struct IRCompiler {
    level: Level,

    pub context: Context,
}

impl IRCompiler {
    pub fn new() -> Self {
        let mut context = Context::new();

        context.new_type(IRType::Any);
        context.new_type(IRType::Unreachable);

        let unit = context.new_type(IRType::Unit);
        context.new_type(IRType::Bool);
        context.new_type(IRType::Int);
        context.new_type(IRType::Float);
        context.new_type(IRType::String);

        let fn_type = context.new_type(IRType::Function {
            args: Vec::new(),
            ret: unit,
        });

        let main = context.path_from(&["root", "main"]);

        IRCompiler {
            level: Level {
                enclosing: None,
                function: IRFunction {
                    path: Span::default(main),
                    ty: fn_type,

                    params: Vec::new(),
                    locals: Vec::new(),
                    blocks: Vec::new(),
                },
                scope_depth: 0,
                identifiers: Pool::new(),
            },
            context,
        }
    }
}
