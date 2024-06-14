use smallvec::SmallVec;

use crate::middle::{
    dump::Dump, Context, IRConstant, IRExpression, IRFunction, IRStatement, IRType, IRTypeInner,
    IRValue, TypeId,
};

pub fn inference(func: &mut IRFunction, ctx: &mut Context) {
    let mut pass = InferencePass {
        func,
        ctx,
        errors: Vec::new(),
    };

    for block in func.blocks.values() {
        for statement in block.statements.iter() {
            pass.check(statement);
        }
    }

    if !pass.errors.is_empty() {
        for error in pass.errors.iter() {
            eprintln!("{}", error);
        }
    }
}

struct InferencePass<'a> {
    func: &'a IRFunction,
    ctx: &'a mut Context,
    errors: Vec<String>,
}

impl<'a> InferencePass<'a> {
    fn check(&mut self, statement: &IRStatement) {
        let expr_ty = self.infer_expr(&statement.expression);
        let target_ty = self.func.get_local(&statement.target).metadata().ty.value;

        let expr = self.grab_type(expr_ty);
        let target = self.grab_type(target_ty);

        // println!("{} = {}", target.dump(self.ctx), expr.dump(self.ctx));

        if !self.unify(target_ty, expr_ty) {
            if target.is_rigid() {
                self.errors.push(format!(
                    "Type mismatch: {} != {}",
                    expr.dump(self.ctx),
                    target.dump(self.ctx)
                ));
            }

            self.update_type_rigid(target_ty, IRTypeInner::Any);
            // let expr = self.grab_type(expr_ty);
            // let target = self.grab_type(target_ty);

            // let ctx = self.ctx();
            // panic!(
            //     "Type mismatch: {} != {}",
            //     expr.dump(&ctx),
            //     target.dump(&ctx)
            // );
        }
    }

    fn grab_type(&self, id: TypeId) -> IRType {
        self.ctx
            .type_table
            .get(&id)
            .expect("Type not found")
            .clone()
    }

    fn update_type_rigid(&mut self, id: TypeId, ty: IRTypeInner) {
        // if the type we are updating is rigid, it is an error

        let tyy = self.grab_type(id);
        if tyy.is_rigid() {
            // self.errors.push(format!(
            //     "Type mismatch: {} != {}",
            //     tyy.dump(self.ctx),
            //     ty.dump(self.ctx),
            // ));
        } else {
            self.ctx.type_table.insert(id, IRType::Rigid(ty));
        }
    }

    fn update_type(&mut self, id: TypeId, ty: IRTypeInner) {
        let tyy = self.grab_type(id);
        if tyy.is_rigid() {
            // self.errors.push(format!(
            //     "Type mismatch: {} != {}",
            //     tyy.dump(self.ctx),
            //     ty.dump(self.ctx),
            // ));
        } else {
            self.ctx.type_table.insert(id, IRType::Free(ty));
        }
    }

    fn free_type(&mut self, id: TypeId) -> TypeId {
        let ty = self.grab_type(id);
        match ty {
            IRType::Free(ty) => self.ctx.new_free_type(ty),
            IRType::Rigid(_) => id,
        }
    }

    fn lock_type(&mut self, id: TypeId) {
        let ty = self.grab_type(id);
        match ty {
            IRType::Free(ty) => self.ctx.type_table.insert(id, IRType::Rigid(ty)),
            IRType::Rigid(_) => None,
        };
    }

    fn free_function(&mut self, id: TypeId) -> TypeId {
        let ty = self.grab_type(id);
        match ty {
            IRType::Rigid(ty) => match ty {
                IRTypeInner::Function { args, ret } => {
                    let new_ret = self.free_type(ret);
                    let args = args
                        .iter()
                        .map(|ty| {
                            if *ty == ret {
                                new_ret
                            } else {
                                self.free_type(*ty)
                            }
                        })
                        .collect::<SmallVec<_>>();
                    self.ctx
                        .new_free_type(IRTypeInner::Function { args, ret: new_ret })
                }

                _ => self.ctx.new_free_type(ty),
            },
            IRType::Free(_) => id,
        }
    }

    fn unify(&mut self, a_id: TypeId, b_id: TypeId) -> bool {
        use IRTypeInner::*;

        let a = self.grab_type(a_id);
        let b = self.grab_type(b_id);

        match (&a, &b) {
            (IRType::Rigid(Any), _) => {
                self.update_type_rigid(b_id, Any);
                true
            }

            (_, IRType::Rigid(Any)) => {
                self.update_type_rigid(a_id, Any);
                true
            }

            _ => {
                let a = a.inner();
                let b = b.inner();

                match (a, b) {
                    (Any, _) => {
                        self.update_type(a_id, b.clone());
                        true
                    }
                    (_, Any) => {
                        self.update_type(b_id, a.clone());
                        true
                    }

                    (
                        Function {
                            args: args1,
                            ret: ret1,
                        },
                        Function {
                            args: args2,
                            ret: ret2,
                        },
                    ) => {
                        if args1.len() != args2.len() {
                            return false;
                        }

                        for (a, b) in args1.iter().zip(args2.iter()) {
                            if !self.unify(*a, *b) {
                                return false;
                            }
                        }

                        self.unify(*ret1, *ret2)
                    }

                    (Unreachable, _) => false,
                    (_, Unreachable) => false,

                    _ => a == b,
                }
            }
        }

        // let a = self.grab_type(a_id);
        // let ar = a.is_rigid();
        // let b = self.grab_type(b_id);
        // let br = b.is_rigid();

        // println!("{} = {}", a.dump(self.ctx), b.dump(self.ctx),);

        // let a = a.to_inner();
        // let b = b.to_inner();

        // match (&a, &b) {
        //     (IRTypeInner::Any, _) => {
        //         if !ar {
        //             self.update_type(a_id, b);
        //         } else {
        //             self.update_type_rigid(b_id, a);
        //         }
        //         true
        //     }
        //     (_, IRTypeInner::Any) => {
        //         if !br {
        //             self.update_type(b_id, a);
        //         } else {
        //             self.update_type_rigid(a_id, b);
        //         }

        //         true
        //     }

        //     (
        //         IRTypeInner::Function {
        //             args: args1,
        //             ret: ret1,
        //         },
        //         IRTypeInner::Function {
        //             args: args2,
        //             ret: ret2,
        //         },
        //     ) => {
        //         if args1.len() != args2.len() {
        //             return false;
        //         }

        //         let mut args_unified = false;
        //         for (a, b) in args1.iter().zip(args2.iter()) {
        //             args_unified |= self.unify(*a, *b);
        //         }

        //         let ret_unified = self.unify(*ret1, *ret2);

        //         args_unified && ret_unified
        //     }

        //     (IRTypeInner::Unreachable, _) => false,
        //     (_, IRTypeInner::Unreachable) => false,
        //     _ => a == b,
        // }
    }

    fn infer_expr(&mut self, expr: &IRExpression) -> TypeId {
        match expr {
            IRExpression::Value(value) => self.infer_value(value),
            IRExpression::Call { function, args } => {
                let function_ty = self.infer_value(function);
                let function_ty = self.free_function(function_ty);

                let params = args
                    .iter()
                    .map(|arg| self.infer_value(arg))
                    .collect::<SmallVec<_>>();

                let ret_ty = self.ctx.AnyFree(); // will be unified
                let func_call = IRTypeInner::Function {
                    args: params,
                    ret: ret_ty, // will be unified
                };
                let func_call_ty = self.ctx.new_free_type(func_call);

                // if it fails to unify, the type will be Any, lock it
                if !self.unify(func_call_ty, function_ty) {
                    self.lock_type(ret_ty);
                }

                ret_ty
            }
        }
    }

    fn infer_value(&mut self, value: &IRValue) -> TypeId {
        match value {
            IRValue::Constant(constant) => self.infer_constant(&constant.value),
            IRValue::Local(var) => self.func.get_local(var).metadata().ty.value,
            IRValue::Function(_) => todo!(),
            IRValue::BuiltinFunction(path) => *self
                .ctx
                .builtin_functions
                .get(path)
                .expect("Builtin function not found"),
            IRValue::Upvalue(_) => todo!(),
        }
    }

    fn infer_constant(&mut self, constant: &IRConstant) -> TypeId {
        let ty = match constant {
            IRConstant::Unit => self.ctx.Unit(),
            IRConstant::Bool(_) => self.ctx.Bool(),
            IRConstant::Int(_) => self.ctx.Int(),
            IRConstant::Float(_) => self.ctx.Float(),
            IRConstant::String(_) => self.ctx.String(),
        };

        self.free_function(ty)
    }
}

// impl InferencePass {
//     pub fn run(ctx: &mut Context) {
//         let mut pass = InferencePass { errors: Vec::new() };

//         if !pass.errors.is_empty() {
//             for error in pass.errors.iter() {
//                 eprintln!("{}", error);
//             }
//             panic!("Inference failed");
//         }
//     }

//     fn inference_pass(&mut self, function: &IRFunction, ctx: &mut Context) {
//         for block in function.blocks.values() {
//             for statement in block.statements.iter() {
//                 self.check(function, statement, ctx);
//             }
//         }
//     }

//     fn grab_type(&self, id: TypeId, ctx: &mut Context) -> IRType {
//         ctx.type_table.get(&id).expect("Type not found").clone()
//     }

//     fn update_type(&self, id: TypeId, ty: IRTypeInner, ctx: &mut Context) {
//         ctx.type_table.insert(id, IRType::Rigid(ty));
//     }

//     fn free_type(&self, id: TypeId, ctx: &mut Context) -> TypeId {
//         let ty = self.grab_type(id, ctx);
//         match ty {
//             IRType::Rigid(ty) => match ty {
//                 IRTypeInner::Function { args, ret } => {
//                     let new_ret = self.free_type(ret, ctx);
//                     let args = args
//                         .iter()
//                         .map(|ty| {
//                             if *ty == ret {
//                                 new_ret
//                             } else {
//                                 self.free_type(*ty, ctx)
//                             }
//                         })
//                         .collect::<SmallVec<_>>();
//                     ctx.new_free_type(IRTypeInner::Function { args, ret: new_ret })
//                 }

//                 _ => ctx.new_free_type(ty),
//             },
//             IRType::Free(_) => id,
//         }
//     }

//     fn check(&self, func: &IRFunction, statement: &IRStatement, ctx: &mut Context) {
//         let expr_ty = self.infer_expr(func, &statement.expression, ctx);
//         let target_ty = func.get_local(&statement.target).metadata().ty.value;

//         if !self.unify(expr_ty, target_ty, ctx) {
//             self.update_type(target_ty, IRTypeInner::Any, ctx);
//             // let expr = self.grab_type(expr_ty);
//             // let target = self.grab_type(target_ty);

//             // let ctx = self.ctx();
//             // panic!(
//             //     "Type mismatch: {} != {}",
//             //     expr.dump(&ctx),
//             //     target.dump(&ctx)
//             // );
//         }
//     }

//     fn unify(&self, a_id: TypeId, b_id: TypeId, ctx: &mut Context) -> bool {
//         let a = self.grab_type(a_id, ctx).to_inner();
//         let b = self.grab_type(b_id, ctx).to_inner();

//         match (&a, &b) {
//             (IRTypeInner::Any, _) => {
//                 self.update_type(a_id, b, ctx);
//                 true
//             }
//             (_, IRTypeInner::Any) => {
//                 self.update_type(b_id, a, ctx);
//                 true
//             }

//             (
//                 IRTypeInner::Function {
//                     args: args1,
//                     ret: ret1,
//                 },
//                 IRTypeInner::Function {
//                     args: args2,
//                     ret: ret2,
//                 },
//             ) => {
//                 if args1.len() != args2.len() {
//                     return false;
//                 }

//                 let mut args_unified = false;
//                 for (a, b) in args1.iter().zip(args2.iter()) {
//                     args_unified |= self.unify(*a, *b, ctx);
//                 }

//                 let ret_unified = self.unify(*ret1, *ret2, ctx);

//                 args_unified && ret_unified
//             }

//             (IRTypeInner::Unreachable, _) => false,
//             (_, IRTypeInner::Unreachable) => false,
//             _ => a == b,
//         }
//     }

//     fn infer_expr(&self, func: &IRFunction, expr: &IRExpression, ctx: &mut Context) -> TypeId {
//         match expr {
//             IRExpression::Value(value) => self.infer_value(func, value, ctx),
//             IRExpression::Call { function, args } => {
//                 let function_ty = self.infer_value(func, function, ctx);
//                 let function_ty = self.free_type(function_ty, ctx);

//                 let params = args
//                     .iter()
//                     .map(|arg| self.infer_value(func, arg, ctx))
//                     .collect::<SmallVec<_>>();

//                 let ret_ty = ctx.Any(); // will be unified
//                 let func_call = IRTypeInner::Function {
//                     args: params,
//                     ret: ret_ty, // will be unified
//                 };
//                 let func_call_ty = ctx.new_free_type(func_call);

//                 self.unify(function_ty, func_call_ty, ctx);

//                 ret_ty
//             }
//         }
//     }

//     fn infer_value(&self, func: &IRFunction, value: &IRValue, ctx: &mut Context) -> TypeId {
//         match value {
//             IRValue::Constant(constant) => self.infer_constant(&constant.value, ctx),
//             IRValue::Local(var) => func.get_local(var).metadata().ty.value,
//             IRValue::Function(_) => todo!(),
//             IRValue::BuiltinFunction(path) => *ctx
//                 .builtin_functions
//                 .get(path)
//                 .expect("Builtin function not found"),
//             IRValue::Upvalue(_) => todo!(),
//         }
//     }

//     fn infer_constant(&self, constant: &IRConstant, ctx: &mut Context) -> TypeId {
//         match constant {
//             IRConstant::Unit => ctx.Unit(),
//             IRConstant::Bool(_) => ctx.Bool(),
//             IRConstant::Int(_) => ctx.Int(),
//             IRConstant::Float(_) => ctx.Float(),
//             IRConstant::String(_) => ctx.String(),
//         }
//     }
// }

// impl IRFunction {
//     pub fn inference_pass(&self, ctx: &mut Context) {}

//     fn grab_type(&self, id: TypeId, ctx: &Context) -> IRType {
//         ctx.type_table.get(&id).expect("Type not found").clone()
//     }

//     fn update_type(&self, id: TypeId, ty: IRTypeInner, ctx: &mut Context) {
//         ctx.type_table.insert(id, ty);
//     }

//     fn refresh_type(&self, id: TypeId, ctx: &mut Context) -> TypeId {
//         let ty = self.grab_type(id, ctx);
//         match ty {
//             IRTypeInner::Function { args, ret } => {
//                 let new_ret = self.refresh_type(ret, ctx);
//                 let args = args
//                     .iter()
//                     .map(|ty| {
//                         if *ty == ret {
//                             new_ret
//                         } else {
//                             self.refresh_type(*ty, ctx)
//                         }
//                     })
//                     .collect::<SmallVec<_>>();
//                 ctx.new_type(IRTypeInner::Function { args, ret: new_ret })
//             }

//             _ => ctx.new_type(ty),
//         }
//     }

//     fn unify(&self, a_id: TypeId, b_id: TypeId, ctx: &mut Context) -> bool {
//         let a = self.grab_type(a_id, ctx);
//         let b = self.grab_type(b_id, ctx);

//         match (&a, &b) {
//             (IRTypeInner::Any, _) => {
//                 self.update_type(a_id, b, ctx);
//                 true
//             }
//             (_, IRTypeInner::Any) => {
//                 self.update_type(b_id, a, ctx);
//                 true
//             }

//             (
//                 IRTypeInner::Function {
//                     args: args1,
//                     ret: ret1,
//                 },
//                 IRTypeInner::Function {
//                     args: args2,
//                     ret: ret2,
//                 },
//             ) => {
//                 if args1.len() != args2.len() {
//                     return false;
//                 }

//                 let mut args_unified = false;
//                 for (a, b) in args1.iter().zip(args2.iter()) {
//                     args_unified |= self.unify(*a, *b, ctx);
//                 }

//                 let ret_unified = self.unify(*ret1, *ret2, ctx);

//                 args_unified && ret_unified
//             }

//             (IRTypeInner::Unreachable, _) => false,
//             (_, IRTypeInner::Unreachable) => false,
//             _ => a == b,
//         }
//     }

//     // fn infer_expr(&self, expr: &IRExpression, ctx: &mut Context) -> TypeId {
//     //     match expr {
//     //         IRExpression::Value(value) => self.infer_value(value),
//     //         IRExpression::Call { function, args } => {
//     //             let function_ty = self.infer_value(function);
//     //             let function_ty = self.refresh_type(function_ty, ctx);

//     //             let params = args
//     //                 .iter()
//     //                 .map(|arg| self.infer_value(arg))
//     //                 .collect::<Vec<_>>();

//     //             let ret_ty = self.Any(); // will be unified
//     //             let func_call = IRType::Function {
//     //                 args: params,
//     //                 ret: ret_ty, // will be unified
//     //             };
//     //             let func_call_ty = ctx.new_type(func_call);

//     //             self.unify(function_ty, func_call_ty);

//     //             ret_ty
//     //         }
//     //     }
//     // }

//     // fn infer_value(&self, value: &IRValue) -> TypeId {
//     //     match value {
//     //         IRValue::Constant(constant) => self.infer_constant(&constant.value),
//     //         IRValue::Local(var) => self.get_local(*var).metadata().ty.value,
//     //         IRValue::Function(_) => todo!(),
//     //         IRValue::BuiltinFunction(path) => {
//     //             self.get_builtin(path).expect("Builtin function not found")
//     //         }
//     //         IRValue::Upvalue(_) => todo!(),
//     //     }
//     // }

//     // fn infer_constant(&self, constant: &IRConstant) -> TypeId {
//     //     match constant {
//     //         IRConstant::Unit => self.Unit(),
//     //         IRConstant::Bool(_) => self.Bool(),
//     //         IRConstant::Int(_) => self.Int(),
//     //         IRConstant::Float(_) => self.Float(),
//     //         IRConstant::String(_) => self.String(),
//     //     }
//     // }
// }

// impl IRCompiler {
//     pub fn run_inference(&self) {
//         for block in self.level.function.blocks.values() {
//             for statement in block.statements.iter() {
//                 self.check(statement);
//             }
//         }

//         // for function in self.ctx().functions.values() {
//         //     for block in function.blocks.values() {
//         //         for statement in block.statements.iter() {
//         //             self.check(statement);
//         //         }
//         //     }
//         // }
//     }

//     fn grab_type(&self, id: TypeId) -> IRTypeInner {
//         self.ctx_ref()
//             .type_table
//             .get(&id)
//             .expect("Type not found")
//             .clone()
//     }

//     fn refresh_type(&self, id: TypeId) -> TypeId {
//         let ty = self.grab_type(id);
//         match ty {
//             IRTypeInner::Function { args, ret } => {
//                 let new_ret = self.refresh_type(ret);
//                 let args = args
//                     .iter()
//                     .map(|ty| {
//                         if *ty == ret {
//                             new_ret
//                         } else {
//                             self.refresh_type(*ty)
//                         }
//                     })
//                     .collect::<SmallVec<_>>();
//                 self.add_type(IRTypeInner::Function { args, ret: new_ret })
//             }

//             _ => self.add_type(ty),
//         }
//     }

//     fn check(&self, statement: &IRStatement) {
//         let expr_ty = self.infer_expr(&statement.expression);
//         let target_ty = self.get_local(statement.target).metadata().ty.value;

//         if !self.unify(expr_ty, target_ty) {
//             self.update_type(target_ty, IRTypeInner::Any);
//             // let expr = self.grab_type(expr_ty);
//             // let target = self.grab_type(target_ty);

//             // let ctx = self.ctx();
//             // panic!(
//             //     "Type mismatch: {} != {}",
//             //     expr.dump(&ctx),
//             //     target.dump(&ctx)
//             // );
//         }
//     }

//     fn unify(&self, a_id: TypeId, b_id: TypeId) -> bool {
//         // if a_id == b_id {
//         //     return true;
//         // }

//         let a = self.grab_type(a_id);
//         let b = self.grab_type(b_id);

//         match (&a, &b) {
//             (IRTypeInner::Any, _) => {
//                 self.update_type(a_id, b);
//                 true
//             }
//             (_, IRTypeInner::Any) => {
//                 self.update_type(b_id, a);
//                 true
//             }

//             (
//                 IRTypeInner::Function {
//                     args: args1,
//                     ret: ret1,
//                 },
//                 IRTypeInner::Function {
//                     args: args2,
//                     ret: ret2,
//                 },
//             ) => {
//                 if args1.len() != args2.len() {
//                     return false;
//                 }

//                 let mut args_unified = false;
//                 for (a, b) in args1.iter().zip(args2.iter()) {
//                     args_unified |= self.unify(*a, *b);
//                 }

//                 let ret_unified = self.unify(*ret1, *ret2);

//                 args_unified && ret_unified
//             }

//             (IRTypeInner::Unreachable, _) => false,
//             (_, IRTypeInner::Unreachable) => false,
//             _ => a == b,
//         }
//     }

//     fn infer_expr(&self, expr: &IRExpression) -> TypeId {
//         match expr {
//             IRExpression::Value(value) => self.infer_value(value),
//             IRExpression::Call { function, args } => {
//                 let function_ty = self.infer_value(function);
//                 let function_ty = self.refresh_type(function_ty);

//                 let params = args
//                     .iter()
//                     .map(|arg| self.infer_value(arg))
//                     .collect::<SmallVec<_>>();

//                 let ret_ty = self.Any(); // will be unified
//                 let func_call = IRTypeInner::Function {
//                     args: params,
//                     ret: ret_ty, // will be unified
//                 };
//                 let func_call_ty = self.add_type(func_call);

//                 self.unify(function_ty, func_call_ty);

//                 ret_ty
//             }
//         }
//     }

//     fn infer_value(&self, value: &IRValue) -> TypeId {
//         match value {
//             IRValue::Constant(constant) => self.infer_constant(&constant.value),
//             IRValue::Local(var) => self.get_local(*var).metadata().ty.value,
//             IRValue::Function(_) => todo!(),
//             IRValue::BuiltinFunction(path) => {
//                 self.get_builtin(path).expect("Builtin function not found")
//             }
//             IRValue::Upvalue(_) => todo!(),
//         }
//     }

//     fn infer_constant(&self, constant: &IRConstant) -> TypeId {
//         match constant {
//             IRConstant::Unit => self.Unit(),
//             IRConstant::Bool(_) => self.Bool(),
//             IRConstant::Int(_) => self.Int(),
//             IRConstant::Float(_) => self.Float(),
//             IRConstant::String(_) => self.String(),
//         }
//     }
// }
