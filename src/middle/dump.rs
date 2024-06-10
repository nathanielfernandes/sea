use colored::{Colorize, CustomColor};

use super::*;

pub trait Dump {
    fn dump(&self, ctx: &Context) -> String;
}

#[rustfmt::skip]
const STRUCT: CustomColor = CustomColor { r: 229, g: 192, b: 123 };
#[rustfmt::skip]
const FUNCTION: CustomColor = CustomColor { r: 97, g: 175, b: 239 };
#[rustfmt::skip]
const KEYWORD: CustomColor = CustomColor { r: 198, g: 120, b: 221 };
#[rustfmt::skip]
const IDENTIFIER: CustomColor = CustomColor { r: 224, g: 108, b: 117 };
#[rustfmt::skip]
const STRING: CustomColor = CustomColor { r: 152, g: 195, b: 121 };
#[rustfmt::skip]
const NUMBER: CustomColor = CustomColor { r: 209, g: 154, b: 102 };
#[rustfmt::skip]
const BUILTIN: CustomColor = CustomColor { r: 80, g: 165, b: 179 };

impl Dump for Path<InternedString> {
    fn dump(&self, ctx: &Context) -> String {
        let mut out = String::new();

        for id in self.0.iter().take(self.0.len() - 1) {
            let name = ctx.strings.get(*id).expect("invalid interned string");
            out.push_str(&name.as_str().custom_color(STRUCT).to_string());
            out.push_str("::");
        }

        let last = *self.last().expect("empty path");
        let last = ctx.strings.get(last).expect("invalid interned string");

        if let Some(s) = ctx.structs.get(self) {
            let c = if s.builtin { BUILTIN } else { STRUCT };
            return out + &last.as_str().custom_color(c).to_string();
        }

        if let Some(_) = ctx.functions.get(self) {
            return out + &last.as_str().custom_color(FUNCTION).to_string();
        }

        if let Some(_) = ctx.builtin_functions.get(self) {
            return out + &last.as_str().custom_color(BUILTIN).to_string();
        }

        // special check if main
        if last.as_str() == "main" {
            return out + &last.as_str().custom_color(BUILTIN).to_string();
        }

        out + &last.as_str()
    }
}

impl Dump for IRType {
    fn dump(&self, ctx: &Context) -> String {
        match self {
            IRType::Any => "Any".custom_color(BUILTIN).to_string(),
            IRType::Unreachable => "Unreachable".dimmed().to_string(),
            IRType::Named(path) => path.dump(ctx),
            IRType::Function { args, ret } => {
                let args = args
                    .iter()
                    .map(|id| {
                        let ty = ctx.type_table.get(id).unwrap();
                        ty.dump(ctx)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = ctx.type_table.get(ret).unwrap().dump(ctx);
                format!("{}({}) -> {}", "fn".custom_color(KEYWORD), args, ret)
            }
        }
    }
}

impl Dump for IRConstant {
    fn dump(&self, ctx: &Context) -> String {
        let v = match self {
            IRConstant::Unit => "()".to_string(),
            IRConstant::Bool(b) => b.to_string().custom_color(NUMBER).to_string(),
            IRConstant::Int(i) => i.to_string().custom_color(NUMBER).to_string(),
            IRConstant::Float(f) => f.to_string().custom_color(NUMBER).to_string(),
            IRConstant::String(s) => {
                let s = ctx.strings.get(*s).expect("invalid interned string");
                format!("{:?}", s).custom_color(STRING).to_string()
            }
        };

        format!("{} {}", "const".custom_color(KEYWORD), v)
    }
}

impl IRValue {
    pub fn dump(&self, ctx: &Context) -> String {
        match self {
            IRValue::Constant(c) => c.value.dump(ctx),
            IRValue::Local(v) => format!("_{}", v).custom_color(IDENTIFIER).to_string(),
            IRValue::Upvalue(_) => todo!(),
            IRValue::Function(_) => todo!(),
            IRValue::BuiltinFunction(path) => path.dump(ctx),
        }
    }
}

impl IRStatement {
    fn dump(&self, ctx: &Context) -> String {
        if self.metadata.borrow().skip {
            return String::new();
        }

        let target = format!("_{}", self.target).custom_color(IDENTIFIER);

        match &self.expression {
            IRExpression::Value(value) => {
                let value = value.dump(ctx);
                format!("      {} = {};\n", target, value)
            }
            IRExpression::Call { function, args } => {
                let function = function.dump(ctx);
                let args = args
                    .iter()
                    .map(|arg| arg.dump(ctx))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("      {} = {}({});\n", target, function, args)
            }
        }
    }
}

impl Block {
    fn dump(&self, ctx: &Context) -> String {
        let mut out = String::new();

        out.push_str(
            &format!("    bb{}", self.id)
                .custom_color(NUMBER)
                .to_string(),
        );
        out.push_str(": {\n");

        for statement in &self.statements {
            out.push_str(&statement.dump(ctx));
        }

        out.push_str("    }\n");

        out
    }
}

impl IRFunction {
    fn dump(&self, ctx: &Context) -> String {
        let sep = if self.params.len() > 5 { ", " } else { ",\n" };

        let format_var = |var: &IRVariable, pre: &str, suf: &str| {
            let name = format!("_{}", var.id).custom_color(IDENTIFIER);
            let meta = var.metadata.borrow();
            let ty = ctx.type_table.get(&meta.ty.value).unwrap();
            let ty = ty.dump(ctx);
            format!("{}{}: {}{}", pre, name, ty, suf)
        };

        let params = self
            .params
            .values()
            .map(|var| format_var(var, "", ""))
            .collect::<Vec<_>>()
            .join(sep);

        let pre = "    let ".custom_color(KEYWORD).to_string();
        let locals = self
            .locals
            .values()
            .map(|var| format_var(var, &pre, ";"))
            .collect::<Vec<_>>()
            .join("\n");

        let mut blocks = String::new();
        for block in self.blocks.values() {
            blocks.push_str(&block.dump(&ctx));
        }

        format!(
            "{f} {name}({params}) {{\n{locals}\n\n{blocks}}}",
            f = "fn".custom_color(KEYWORD),
            name = self.path.value.dump(ctx),
            params = params,
            locals = locals
        )
    }
}

impl IRCompiler {
    pub fn dump_types(&self) {
        let ctx = self.ctx();
        println!("Types:");
        for (id, ty) in &ctx.type_table {
            println!(
                "{} => {}",
                id.to_string().custom_color(NUMBER),
                ty.dump(&ctx)
            );
        }
    }

    pub fn dump(&self) {
        self.dump_types();

        let ctx = self.ctx();

        println!("\nFunctions:");

        for function in [&self.level.function]
            .into_iter()
            .chain(ctx.functions.values())
        {
            println!("{}", function.dump(&ctx));
        }
    }
}
