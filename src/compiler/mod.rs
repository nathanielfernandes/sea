use std::fs::read_to_string;

use ir::{IRBlock, IRFunction, IRStatement, IRType, IRValue, IRVar};

use eyre::Result;

use crate::parser::scanner::Span;

pub mod ir;
pub mod pool;

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
   
    const BUILTIN_FUNCTIONS: &'static [&'static str] = &[
        "print",
        "println",
    ];
}

impl Compiler {
    pub fn new<T: ToString>(user_prefix: T) -> Self {
        Self {
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

    pub fn compile(mut self, functions: &[IRFunction]) -> Result<String> {
        const LIB: &str = "/* {{lib}} */";
        const FUNCTION_DEFS: &str = "/* {{function definitions}} */";
        const FUNCTIONS: &str = "/* {{functions}} */";
        const MAIN: &str = "/* {{main}} */";

        let template = read_to_string("src/static/template.c")?;

        let mut main_code = String::new();

        let mut functions = functions.into_iter().rev();

        if let Some(main) = functions.next() {
            main_code.push_str(&self.compile_function_body(main)?);
        }

        for function in functions {
            let (def, decl) = self.compile_function(function)?;
            self.function_definitions.push_str(&def);
            self.function_declarations.push_str(&decl);
        }

        let lib = read_to_string("src/static/lib.c")?;

        let final_code = template
            .replace(LIB, &lib)
            .replace(FUNCTION_DEFS, &self.function_definitions)
            .replace(FUNCTIONS, &self.function_declarations)
            .replace(MAIN, &main_code);

        Ok(final_code)
    }

    fn compile_type(&self, ty: &IRType) -> String {
        String::from(Self::VALUE_TYPE)
    }

    fn compile_param(&self, param: &IRVar) -> String {
        let id = param.id;
        // let name = match &param.name {
        //     Some(name) => name,
        //     None => &format!("_{}", id.value),
        // };

        let name = format!("_{}", id.value);

        let ty = self.compile_type(&param.ty.value);

        format!("{} {}", ty, name)
    }

    fn compile_block(&mut self, block: &IRBlock) -> Result<String> {
        let mut code = String::new();

        if block.id == 0 {
            code.push_str(&format!("goto bb{};\n", block.id));
        }

        code.push_str(&format!("bb{}: {{\n", block.id));

        for statement in &block.statements {
            code.push_str(&self.compile_statement(statement)?);
        }

        match block.next_block {
            Some(next) => {
                code.push_str(&format!("goto bb{};\n", next));
            }
            None => {
                code.push_str("return Ok(Unit());\n");
            }
        }

        code.push_str("}\n");

        Ok(code)
    }

    fn compile_value(&mut self, value: &Span<IRValue>) -> Result<String> {
        match &value.value {
            IRValue::Constant(constant) => match constant {
                ir::IRConstant::Unit => Ok(Self::unit()),
                ir::IRConstant::Bool(value) => Ok(Self::bool(*value)),
                ir::IRConstant::Int(value) => Ok(Self::int(*value)),
                ir::IRConstant::Float(value) => Ok(Self::float(*value)),
                ir::IRConstant::String(value) => Ok(Self::string(value)),
                ir::IRConstant::Function(name) => Ok(format!("_{}", name)),
            },
            IRValue::Variable(var) => Ok(format!("_{}", var)),
            IRValue::UpValue(_) => todo!(),
            IRValue::BuiltIn(name) => Ok(name.to_string()),
        }
    }
    fn compile_statement(
        &mut self,
        Span {
            start,
            end,
            value: statement,
        }: &Span<IRStatement>,
    ) -> Result<String> {
        match statement {
            IRStatement::ValueAssignment { target, value } => {
                let name = format!("_{}", target.value);
                let value = self.compile_value(value)?;

                Ok(format!("{} = {};", name, value))
            }
            IRStatement::CallAssignment {
                target,
                function,
                arguments,
            } => {
                let name = format!("_{}", target.value);
                let args = arguments
                    .iter()
                    .map(|arg| self.compile_value(arg))
                    .collect::<Result<Vec<_>>>()?;

                let func = self.compile_value(function)?;

                Ok(format!(
                    "{} = {}({}({}));",
                    name,
                    Self::TRY,
                    func,
                    args.join(", ")
                ))
            }
            IRStatement::BuiltinCallAssignment {
                target,
                function,
                arguments,
            } => {
                let name = format!("_{}", target.value);
                let args = arguments
                    .iter()
                    .map(|arg| self.compile_value(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(format!(
                    "{} = {}({}({}));",
                    name,
                    Self::TRY,
                    function.value,
                    args.join(", ")
                ))
            }
            IRStatement::Return { value } => {
                let value = self.compile_value(value)?;
                Ok(format!("return Ok({});", value))
            }
        }
    }

    fn compile_function_body(&mut self, function: &IRFunction) -> Result<String> {
        let mut body = String::new();
        for local in &function.locals {
            body.push_str(&self.compile_param(local));
            body.push_str(";\n");
        }

        for block in &function.blocks {
            body.push_str(&self.compile_block(block)?);
        }
        Ok(body)
    }

    fn compile_function(&mut self, function: &IRFunction) -> Result<(String, String)> {
        let ret = Self::RESULT_TYPE;
        let name = format!("_{}", function.name.value);
        let mut params = String::new();
        for (i, param) in function.params.iter().enumerate() {
            if i > 0 {
                params.push_str(", ");
            }
            params.push_str(&self.compile_param(param));
        }

        let body = self.compile_function_body(function)?;

        let def = format!("{} {}({}) {{\n{}\n}}\n", ret, name, params, body);
        let decl = format!("{} {}({});\n", ret, name, params);

        Ok((decl, def))
    }
}
