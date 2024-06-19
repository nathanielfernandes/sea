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
            let name = ctx
                .strings()
                .get(*id)
                .expect("invalid interned string")
                .clone();
            out.push_str(&name.as_str().custom_color(STRUCT).to_string());
            out.push_str("::");
        }

        let last = *self.last().expect("empty path");
        let last = ctx
            .strings()
            .get(last)
            .expect("invalid interned string")
            .clone();

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

        // // special check if main
        // if last.as_str() == "main" {
        //     return out + &last.as_str().custom_color(BUILTIN).to_string();
        // }

        out + &last.as_str()
    }
}

impl Dump for IRTypeInner {
    fn dump(&self, ctx: &Context) -> String {
        match self {
            IRTypeInner::Any => "Any".custom_color(BUILTIN).to_string(),
            IRTypeInner::Unreachable => "Unreachable".dimmed().to_string(),
            IRTypeInner::Named(path) => path.dump(ctx),
            IRTypeInner::Function { args, ret: ret_id } => {
                // if any of the arg types are the same as any other type,
                // it is a generic

                const LETTERS: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
                let mut letters = LETTERS.chars();

                let mut seen = HashSet::new();
                let mut generics = IndexMap::new();

                for id in args.iter() {
                    let ty = ctx.type_table.get(id).unwrap().inner();
                    if let IRTypeInner::Any = ty {
                        if seen.contains(id) {
                            let letter = letters.next().unwrap();
                            generics.insert(*id, letter);
                        } else {
                            seen.insert(*id);
                        }
                    }
                }

                let args = args
                    .iter()
                    .map(|id| match generics.get(id) {
                        Some(letter) => letter.to_string().custom_color(STRUCT).to_string(),
                        None => {
                            let ty = ctx.type_table.get(id).unwrap();
                            ty.dump(ctx)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let ret = match generics.get(ret_id) {
                    Some(letter) => letter.to_string().custom_color(STRUCT).to_string(),
                    None => ctx.type_table.get(ret_id).unwrap().dump(ctx),
                };

                let gens = generics
                    .values()
                    .map(|letter| letter.to_string().custom_color(STRUCT).to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                let gens = if gens.is_empty() {
                    String::new()
                } else {
                    format!("<{}>", gens)
                };

                // let ret = format!("{}>{}", ret_id, ret);
                format!(
                    "{}{}({}) -> {}",
                    "fn".custom_color(KEYWORD),
                    gens,
                    args,
                    ret
                )
            }
        }
    }
}

impl Dump for IRType {
    fn dump(&self, ctx: &Context) -> String {
        match self {
            IRType::Rigid(inner) => format!("🔒{}", inner.dump(ctx)),
            IRType::Free(inner) => inner.dump(ctx),
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
                let s = ctx
                    .strings()
                    .get(*s)
                    .expect("invalid interned string")
                    .clone();
                format!("{:?}", s).custom_color(STRING).to_string()
            }
        };

        format!("{} {}", "const".custom_color(KEYWORD), v)
    }
}
impl Dump for IRValue {
    fn dump(&self, ctx: &Context) -> String {
        match self {
            IRValue::Constant(c) => c.value.dump(ctx),
            IRValue::Local(v) => format!("_{}", v).custom_color(IDENTIFIER).to_string(),
            IRValue::Upvalue(_) => todo!(),
            IRValue::Function(path) => path.dump(ctx),
            IRValue::BuiltinFunction(path) => path.dump(ctx),
        }
    }
}

impl Dump for IRStatement {
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
            IRExpression::Call { function, args, .. } => {
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

impl Dump for ControlFlow {
    fn dump(&self, ctx: &Context) -> String {
        let cf = match self {
            ControlFlow::None => "none".custom_color(KEYWORD).to_string(),
            ControlFlow::Goto(id) => {
                let bb = format!("bb{}", id).custom_color(NUMBER);
                let goto = "goto".custom_color(KEYWORD);
                format!("{} -> {}", goto, bb)
            }
            ControlFlow::Branch {
                condition,
                success,
                otherwise,
            } => {
                let cond = condition.dump(ctx);
                let success = format!("bb{}", success).custom_color(NUMBER);
                let otherwise = format!("bb{}", otherwise).custom_color(NUMBER);

                format!(
                    "{switch}({cond}) -> {success}, {otherwise}",
                    switch = "switch".custom_color(KEYWORD),
                    cond = cond,
                    success = success,
                    otherwise = otherwise
                )
            }
            ControlFlow::Return(var) => {
                let ret = "return".custom_color(KEYWORD);
                let var = if var > &0 {
                    format!(" _{}", var).custom_color(IDENTIFIER).to_string()
                } else {
                    String::new()
                };

                format!("{}{}", ret, var)
            }
        };

        format!("      [{}];\n", cf)
    }
}

impl Dump for Block {
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

        if !self.statements.is_empty() {
            out.push_str("\n");
        }

        out.push_str(&self.control_flow.dump(ctx));

        out.push_str("    }\n");

        out
    }
}

impl Dump for IRFunctionInner {
    fn dump(&self, ctx: &Context) -> String {
        let sep = if self.params.len() > 5 { ",\n" } else { ", " };

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

        let mut blocks = Vec::new();
        for block in self.blocks.values() {
            blocks.push(block.dump(&ctx));
        }

        let func_ty = ctx.type_table.get(&self.ty).unwrap();
        let ret = match func_ty.inner() {
            IRTypeInner::Function { ret, .. } => ctx.type_table.get(ret).unwrap().dump(ctx),
            _ => panic!("expected function type"),
        };

        format!(
            "{f} {name}({params}) -> {ret} {{\n{locals}\n\n{blocks}}}",
            f = "fn".custom_color(KEYWORD),
            name = self.path.value.dump(ctx),
            params = params,
            ret = ret,
            locals = locals,
            blocks = blocks.join("\n")
        )
    }
}

impl Context {
    pub fn dump(&self) -> String {
        let mut out = String::new();

        // out.push_str("Types:\n");
        // for (id, ty) in &self.type_table {
        //     out.push_str(&format!(
        //         "  {} => {}\n",
        //         id.to_string().custom_color(NUMBER),
        //         ty.dump(self)
        //     ));
        // }

        out.push_str(
            &("          Functions          "
                .on_custom_color(FUNCTION)
                .to_string()
                + "\n"),
        );
        for function in self.functions.values() {
            out.push_str(&function.finished().dump(self));
            out.push_str("\n\n");
        }

        out
    }
}

// impl IRCompiler {
//     pub fn dump_types(&self) {
//         let ctx = self.ctx_ref();
//         println!("Types:");
//         for (id, ty) in &ctx.type_table {
//             println!(
//                 "{} => {}",
//                 id.to_string().custom_color(NUMBER),
//                 ty.dump(&ctx)
//             );
//         }
//     }

//     pub fn dump(&self) {
//         self.dump_types();

//         let ctx = self.ctx_ref();

//         println!("\nFunctions:");

//         // for function in [&self.level.function]
//         //     .into_iter()
//         //     .chain(ctx.functions.values())
//         // {
//         //     println!("{}", function.dump(&ctx));
//         // }

//         for function in ctx.functions.values() {
//             println!("{}", function.dump(&ctx));
//         }
//     }
// }
