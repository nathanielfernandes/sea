use eyre::eyre;
use eyre::{OptionExt, Result};
use std::fs::read_to_string;

use crate::middle::dump::Dump;
use crate::middle::pathmap::Path;
use crate::middle::{
    Block, Context, IRConstant, IRExpression, IRFunction, IRFunctionInner, IRStatement, IRType,
    IRTypeInner, IRValue, IRVariable, InternedString, TypeId, VariableId,
};

pub struct Compiler {
    ctx: Context,

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
   
    const BUILTIN_FUNCTIONS: &'static [&'static str] = &[
        "print",
        "println",
    ];
}

impl Compiler {
    pub fn new<T: ToString>(user_prefix: T, context: Context) -> Self {
        Self {
            ctx: context,
            user_prefix: user_prefix.to_string(),
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

    pub fn compile(mut self) -> Result<String> {
        const LIB: &str = "/* {{lib}} */";
        const FUNCTION_DEFS: &str = "/* {{function definitions}} */";
        const FUNCTIONS: &str = "/* {{functions}} */";
        const MAIN: &str = "/* {{main}} */";

        let template = read_to_string("src/static/template.c")?;

        let functions = self.ctx.functions.values();

        for function in functions {
            let (def, decl) = self.compile_function(function)?;
            self.function_definitions.push_str(&def);
            self.function_declarations.push_str(&decl);
        }

        let lib = read_to_string("src/static/lib.c")?;

        let final_code = template
            .replace(LIB, &lib)
            .replace(FUNCTION_DEFS, &self.function_definitions)
            .replace(FUNCTIONS, &self.function_declarations);

        Ok(final_code)
    }

    // fn compile_function_body(&mut self, function: &IRFunction) -> Result<String> {
    //     let mut body = String::new();

    //     let function = function.finished();

    //     for block in &function.blocks {
    //         body.push_str(&self.compile_block(block)?);
    //     }
    //     Ok(body)
    // }

    fn compile_function(&self, function: &IRFunction) -> Result<(String, String)> {
        let function = function.finished();

        let ty = self.get_type(function.ty)?;

        let IRTypeInner::Function { args, ret } = ty else {
            return Err(eyre!("Expected function type"));
        };

        let ret = self.compile_type(*ret)?;
        let name = self.compile_path(&function.path.value);
        let params = args
            .iter()
            .map(|&arg| self.compile_type(arg))
            .collect::<Result<Vec<_>>>()?
            .join(", ");

        let decl = format!("{} {}({});\n", ret, name, params);
        // let def = format!("{} {}({}) {{\n}}\n", ret, name, args);

        let mut locals = String::new();
        for local in function.locals.values() {
            let ty = self.compile_type(local.metadata().ty.value)?;
            locals.push_str(&format!("{} _{};\n", ty, local.id));
        }

        let mut body = String::new();
        for block in function.blocks.values() {
            body.push_str(&self.compile_block(function, block)?);
        }

        let args = function
            .params
            .values()
            .map(|param| {
                let ty = param.metadata().ty.value;
                let ty = self.compile_type(ty)?;
                Ok(format!("{} _{}", ty, param.id))
            })
            .collect::<Result<Vec<_>>>()?
            .join(", ");

        let def = format!("{} {}({}) {{{}\n{}\n}}\n", ret, name, args, locals, body);

        Ok((decl, def))
    }

    fn compile_block(&self, function: &IRFunctionInner, block: &Block) -> Result<String> {
        let mut code = String::new();

        if block.id == 0 {
            code.push_str(&format!("goto bb{};\n", block.id));
        }

        code.push_str(&format!("bb{}: {{\n", block.id));

        for statement in &block.statements {
            code.push_str(&self.compile_statement(function, statement)?);
        }

        match &block.control_flow {
            crate::middle::ControlFlow::None => panic!("Invalid control flow"),
            crate::middle::ControlFlow::Goto(id) => {
                code.push_str(&format!("goto bb{};\n", id));
            }
            crate::middle::ControlFlow::Branch {
                condition,
                success,
                otherwise,
            } => {
                let bool = self.ctx.Bool();
                let condition = self.compile_value(function, condition, bool)?;

                // success and otherwise are jump ids
                code.push_str(&format!("if ({}) {{\n", condition));
                code.push_str(&format!("goto bb{};\n", success));
                code.push_str("} else {\n");
                code.push_str(&format!("goto bb{};\n", otherwise));
                code.push_str("}\n");
            }
            crate::middle::ControlFlow::Return(var) => {
                code.push_str(&format!("return _{};\n", var));
            }
        }

        code.push_str("}\n");

        Ok(code)
    }

    fn compile_statement(
        &self,
        function: &IRFunctionInner,
        statement: &IRStatement,
    ) -> Result<String> {
        let target_ty = self.get_variable_type(function, statement.target);
        let value = self.compile_expression(function, &statement.expression, target_ty)?;
        Ok(format!("_{} = {};\n", statement.target, value))
    }

    fn get_variable_type(&self, function: &IRFunctionInner, var: VariableId) -> TypeId {
        let var = function.get_local(&var);
        var.metadata().ty.value
    }

    fn compile_expression(
        &self,
        function: &IRFunctionInner,
        expression: &IRExpression,
        target_ty: TypeId,
    ) -> Result<String> {
        match expression {
            IRExpression::Value(value) => self.compile_value(function, value, target_ty),
            IRExpression::Call {
                function: func,
                args,
                resolved_function,
            } => {
                let ty_id = if let Some(ty_id) = *resolved_function.borrow() {
                    ty_id
                } else {
                    let (IRValue::Function(path) | IRValue::BuiltinFunction(path)) = func else {
                        return Err(eyre!("Expected function"));
                    };
                    self.ctx.get_type(path).ok_or_eyre("Type not found")?
                };

                let IRTypeInner::Function {
                    args: arg_tys,
                    ret: ret_ty,
                } = self.get_type(ty_id)?
                else {
                    return Err(eyre!("Expected function type"));
                };

                let func = self.compile_value(function, func, ty_id)?;
                let p = self.get_type(ty_id)?.dump(&self.ctx);
                println!("name: {}, p: {}", func, p);

                let args = args
                    .iter()
                    .zip(arg_tys.iter())
                    .map(|(arg, ty)| self.compile_value(function, arg, *ty))
                    .collect::<Result<Vec<_>>>()?
                    .join(", ");

                let ret_ty = self.get_type(*ret_ty)?;
                // println!("ret_ty: {:?}", ret_ty);
                let target_ty = self.get_type(target_ty)?;

                if ret_ty == target_ty {
                    return Ok(format!("{}({})", func, args));
                }

                match target_ty {
                    IRTypeInner::Any => {
                        let src_ty_name = match ret_ty {
                            IRTypeInner::Named(path) => self.compile_path(path),
                            _ => panic!("Invalid type"),
                        };
                        Ok(format!("{}({}({}))", src_ty_name, func, args))
                    }
                    IRTypeInner::Named(path) => {
                        if let IRTypeInner::Any = ret_ty {
                            let path = self.compile_path(path);
                            return Ok(format!("({}({})).data.{}", func, args, path));
                        }
                        panic!("Invalid type");
                    }
                    _ => panic!("Invalid type"),
                }
            }
        }
    }

    fn get_string(&self, id: InternedString) -> Result<String> {
        self.ctx
            .strings()
            .get(id)
            .cloned()
            .ok_or_eyre("String not found")
    }

    fn compile_value(
        &self,
        function: &IRFunctionInner,
        value: &IRValue,
        target_ty: TypeId,
    ) -> Result<String> {
        let target_ty = self.get_type(target_ty)?;

        // let p = self.compile_path(&function.path.value);

        // println!(
        //     "function: {:?}, value: {:?}, target_ty: {:?}",
        //     p, value, target_ty
        // );

        match value {
            IRValue::Constant(constant) => {
                if let IRTypeInner::Any = target_ty {
                    return match constant.value {
                        IRConstant::Unit => Ok(Self::unit()),
                        IRConstant::Bool(value) => Ok(Self::bool(value)),
                        IRConstant::Int(value) => Ok(Self::int(value)),
                        IRConstant::Float(value) => Ok(Self::float(value)),
                        IRConstant::String(value) => Ok(Self::string(&self.get_string(value)?)),
                    };
                }

                if let IRTypeInner::Named(path) = target_ty {
                    let path = self.compile_path(path);
                    return match (constant.value, path.as_str()) {
                        (IRConstant::Unit, "Unit") => Ok(String::from("0")),
                        (IRConstant::Bool(value), "Bool") => Ok(value.to_string()),
                        (IRConstant::Int(value), "Int") => Ok(value.to_string()),
                        (IRConstant::Float(value), "Float") => Ok(value.to_string()),
                        (IRConstant::String(value), "String") => {
                            Ok(format!("to_string({:?})", value))
                        }
                        _ => panic!("constant type missmatch"),
                    };
                }

                panic!("Invalid type");
            }
            IRValue::Local(var) => {
                let var = function.get_local(var);
                let src_ty = var.metadata().ty.value;
                let src_ty = self.get_type(src_ty)?;

                if src_ty == target_ty {
                    return Ok(format!("_{}", var.id));
                }

                match target_ty {
                    IRTypeInner::Any => {
                        println!(
                            "target: {}, src: {}",
                            target_ty.dump(&self.ctx),
                            src_ty.dump(&self.ctx)
                        );

                        let src_ty_name = match src_ty {
                            IRTypeInner::Named(path) => self.compile_path(path),
                            _ => panic!("Invalid type"),
                        };

                        Ok(format!("{}(_{})", src_ty_name, var.id))
                    }
                    IRTypeInner::Named(path) => {
                        if let IRTypeInner::Any = src_ty {
                            let path = self.compile_path(path);
                            return Ok(format!("_{}.data.{}", var.id, path));
                        }

                        panic!("Invalid type");
                    }
                    _ => panic!("Invalid type"),
                }
            }
            IRValue::Upvalue(_) => todo!(),
            IRValue::Function(path) => Ok(self.compile_path(path)),
            IRValue::BuiltinFunction(path) => Ok(self.compile_path(path)),
        }
    }

    fn get_type(&self, ty: TypeId) -> Result<&IRTypeInner> {
        self.ctx
            .type_table
            .get(&ty)
            .ok_or_eyre("Type not found")
            .map(|x| x.inner())
    }

    fn compile_path(&self, path: &Path<InternedString>) -> String {
        let strings = self.ctx.strings();
        let path = path
            .0
            .iter()
            .map(|x| strings.get(*x).unwrap().clone())
            .collect::<Vec<_>>();
        path.join("__")
    }

    fn compile_type(&self, ty: usize) -> Result<String> {
        let ty = self
            .ctx
            .type_table
            .get(&ty)
            .ok_or_eyre("Type not found")?
            .inner();

        let res = match ty {
            IRTypeInner::Unreachable => "void".to_string(),
            IRTypeInner::Any => Self::VALUE_TYPE.to_string(),
            IRTypeInner::Named(path) => self.compile_path(path),
            IRTypeInner::Function { .. } => todo!(),
        };

        Ok(res)
    }

    // fn compile_type(&self, ty: &IRType) -> String {
    //     String::from(Self::VALUE_TYPE)
    // }

    // fn compile_param(&self, param: &IRVar) -> String {
    //     let id = param.id;
    //     // let name = match &param.name {
    //     //     Some(name) => name,
    //     //     None => &format!("_{}", id.value),
    //     // };

    //     let name = format!("_{}", id.value);

    //     let ty = self.compile_type(&param.ty.value);

    //     format!("{} {}", ty, name)
    // }

    // fn compile_block(&mut self, block: &IRBlock) -> Result<String> {
    //     let mut code = String::new();

    //     if block.id == 0 {
    //         code.push_str(&format!("goto bb{};\n", block.id));
    //     }

    //     code.push_str(&format!("bb{}: {{\n", block.id));

    //     for statement in &block.statements {
    //         code.push_str(&self.compile_statement(statement)?);
    //     }

    //     match block.next_block {
    //         Some(next) => {
    //             code.push_str(&format!("goto bb{};\n", next));
    //         }
    //         None => {
    //             code.push_str("return Ok(Unit());\n");
    //         }
    //     }

    //     code.push_str("}\n");

    //     Ok(code)
    // }

    // fn compile_value(&mut self, value: &Span<IRValue>) -> Result<String> {
    //     match &value.value {
    //         IRValue::Constant(constant) => match constant {
    //             ir::IRConstant::Unit => Ok(Self::unit()),
    //             ir::IRConstant::Bool(value) => Ok(Self::bool(*value)),
    //             ir::IRConstant::Int(value) => Ok(Self::int(*value)),
    //             ir::IRConstant::Float(value) => Ok(Self::float(*value)),
    //             ir::IRConstant::String(value) => Ok(Self::string(value)),
    //             ir::IRConstant::Function(name) => Ok(format!("_{}", name)),
    //         },
    //         IRValue::Variable(var) => Ok(format!("_{}", var)),
    //         IRValue::UpValue(_) => todo!(),
    //         IRValue::BuiltIn(name) => Ok(name.to_string()),
    //     }
    // }
    // fn compile_statement(
    //     &mut self,
    //     Span {
    //         start,
    //         end,
    //         value: statement,
    //     }: &Span<IRStatement>,
    // ) -> Result<String> {
    //     match statement {
    //         IRStatement::ValueAssignment { target, value } => {
    //             let name = format!("_{}", target.value);
    //             let value = self.compile_value(value)?;

    //             Ok(format!("{} = {};", name, value))
    //         }
    //         IRStatement::CallAssignment {
    //             target,
    //             function,
    //             arguments,
    //         } => {
    //             let name = format!("_{}", target.value);
    //             let args = arguments
    //                 .iter()
    //                 .map(|arg| self.compile_value(arg))
    //                 .collect::<Result<Vec<_>>>()?;

    //             let func = self.compile_value(function)?;

    //             Ok(format!(
    //                 "{} = {}({}({}));",
    //                 name,
    //                 Self::TRY,
    //                 func,
    //                 args.join(", ")
    //             ))
    //         }
    //         IRStatement::BuiltinCallAssignment {
    //             target,
    //             function,
    //             arguments,
    //         } => {
    //             let name = format!("_{}", target.value);
    //             let args = arguments
    //                 .iter()
    //                 .map(|arg| self.compile_value(arg))
    //                 .collect::<Result<Vec<_>>>()?;

    //             Ok(format!(
    //                 "{} = {}({}({}));",
    //                 name,
    //                 Self::TRY,
    //                 function.value,
    //                 args.join(", ")
    //             ))
    //         }
    //         IRStatement::Return { value } => {
    //             let value = self.compile_value(value)?;
    //             Ok(format!("return Ok({});", value))
    //         }
    //     }
    // }

    // fn compile_function_body(&mut self, function: &IRFunction) -> Result<String> {
    //     let mut body = String::new();
    //     for local in &function.locals {
    //         body.push_str(&self.compile_param(local));
    //         body.push_str(";\n");
    //     }

    //     for block in &function.blocks {
    //         body.push_str(&self.compile_block(block)?);
    //     }
    //     Ok(body)
    // }

    // fn compile_function(&mut self, function: &IRFunction) -> Result<(String, String)> {
    //     let ret = Self::RESULT_TYPE;
    //     let name = format!("_{}", function.name.value);
    //     let mut params = String::new();
    //     for (i, param) in function.params.iter().enumerate() {
    //         if i > 0 {
    //             params.push_str(", ");
    //         }
    //         params.push_str(&self.compile_param(param));
    //     }

    //     let body = self.compile_function_body(function)?;

    //     let def = format!("{} {}({}) {{\n{}\n}}\n", ret, name, params, body);
    //     let decl = format!("{} {}({});\n", ret, name, params);

    //     Ok((decl, def))
    // }
}
