use smallvec::SmallVec;

use crate::{
    compiler::ir::IRFunction,
    middle::{Context, IRCompiler, IRConstant, IRExpression, IRStatement, IRType, IRValue, TypeId},
};

impl IRFunction {
    pub fn inference_pass(&self, ctx: &mut Context) {}

    fn grab_type(&self, id: TypeId, ctx: &Context) -> IRType {
        ctx.type_table.get(&id).expect("Type not found").clone()
    }

    fn update_type(&self, id: TypeId, ty: IRType, ctx: &mut Context) {
        ctx.type_table.insert(id, ty);
    }

    fn refresh_type(&self, id: TypeId, ctx: &mut Context) -> TypeId {
        let ty = self.grab_type(id, ctx);
        match ty {
            IRType::Function { args, ret } => {
                let new_ret = self.refresh_type(ret, ctx);
                let args = args
                    .iter()
                    .map(|ty| {
                        if *ty == ret {
                            new_ret
                        } else {
                            self.refresh_type(*ty, ctx)
                        }
                    })
                    .collect::<SmallVec<_>>();
                ctx.new_type(IRType::Function { args, ret: new_ret })
            }

            _ => ctx.new_type(ty),
        }
    }

    fn unify(&self, a_id: TypeId, b_id: TypeId, ctx: &mut Context) -> bool {
        let a = self.grab_type(a_id, ctx);
        let b = self.grab_type(b_id, ctx);

        match (&a, &b) {
            (IRType::Any, _) => {
                self.update_type(a_id, b, ctx);
                true
            }
            (_, IRType::Any) => {
                self.update_type(b_id, a, ctx);
                true
            }

            (
                IRType::Function {
                    args: args1,
                    ret: ret1,
                },
                IRType::Function {
                    args: args2,
                    ret: ret2,
                },
            ) => {
                if args1.len() != args2.len() {
                    return false;
                }

                let mut args_unified = false;
                for (a, b) in args1.iter().zip(args2.iter()) {
                    args_unified |= self.unify(*a, *b, ctx);
                }

                let ret_unified = self.unify(*ret1, *ret2, ctx);

                args_unified && ret_unified
            }

            (IRType::Unreachable, _) => false,
            (_, IRType::Unreachable) => false,
            _ => a == b,
        }
    }

    // fn infer_expr(&self, expr: &IRExpression, ctx: &mut Context) -> TypeId {
    //     match expr {
    //         IRExpression::Value(value) => self.infer_value(value),
    //         IRExpression::Call { function, args } => {
    //             let function_ty = self.infer_value(function);
    //             let function_ty = self.refresh_type(function_ty, ctx);

    //             let params = args
    //                 .iter()
    //                 .map(|arg| self.infer_value(arg))
    //                 .collect::<Vec<_>>();

    //             let ret_ty = self.Any(); // will be unified
    //             let func_call = IRType::Function {
    //                 args: params,
    //                 ret: ret_ty, // will be unified
    //             };
    //             let func_call_ty = ctx.new_type(func_call);

    //             self.unify(function_ty, func_call_ty);

    //             ret_ty
    //         }
    //     }
    // }

    // fn infer_value(&self, value: &IRValue) -> TypeId {
    //     match value {
    //         IRValue::Constant(constant) => self.infer_constant(&constant.value),
    //         IRValue::Local(var) => self.get_local(*var).metadata().ty.value,
    //         IRValue::Function(_) => todo!(),
    //         IRValue::BuiltinFunction(path) => {
    //             self.get_builtin(path).expect("Builtin function not found")
    //         }
    //         IRValue::Upvalue(_) => todo!(),
    //     }
    // }

    // fn infer_constant(&self, constant: &IRConstant) -> TypeId {
    //     match constant {
    //         IRConstant::Unit => self.Unit(),
    //         IRConstant::Bool(_) => self.Bool(),
    //         IRConstant::Int(_) => self.Int(),
    //         IRConstant::Float(_) => self.Float(),
    //         IRConstant::String(_) => self.String(),
    //     }
    // }
}

impl IRCompiler {
    pub fn run_inference(&self) {
        for block in self.level.function.blocks.values() {
            for statement in block.statements.iter() {
                self.check(statement);
            }
        }

        // for function in self.ctx().functions.values() {
        //     for block in function.blocks.values() {
        //         for statement in block.statements.iter() {
        //             self.check(statement);
        //         }
        //     }
        // }
    }

    fn grab_type(&self, id: TypeId) -> IRType {
        self.ctx()
            .type_table
            .get(&id)
            .expect("Type not found")
            .clone()
    }

    fn refresh_type(&self, id: TypeId) -> TypeId {
        let ty = self.grab_type(id);
        match ty {
            IRType::Function { args, ret } => {
                let new_ret = self.refresh_type(ret);
                let args = args
                    .iter()
                    .map(|ty| {
                        if *ty == ret {
                            new_ret
                        } else {
                            self.refresh_type(*ty)
                        }
                    })
                    .collect::<SmallVec<_>>();
                self.add_type(IRType::Function { args, ret: new_ret })
            }

            _ => self.add_type(ty),
        }
    }

    fn check(&self, statement: &IRStatement) {
        let expr_ty = self.infer_expr(&statement.expression);
        let target_ty = self.get_local(statement.target).metadata().ty.value;

        if !self.unify(expr_ty, target_ty) {
            self.update_type(target_ty, IRType::Any);
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

    fn unify(&self, a_id: TypeId, b_id: TypeId) -> bool {
        // if a_id == b_id {
        //     return true;
        // }

        let a = self.grab_type(a_id);
        let b = self.grab_type(b_id);

        match (&a, &b) {
            (IRType::Any, _) => {
                self.update_type(a_id, b);
                true
            }
            (_, IRType::Any) => {
                self.update_type(b_id, a);
                true
            }

            (
                IRType::Function {
                    args: args1,
                    ret: ret1,
                },
                IRType::Function {
                    args: args2,
                    ret: ret2,
                },
            ) => {
                if args1.len() != args2.len() {
                    return false;
                }

                let mut args_unified = false;
                for (a, b) in args1.iter().zip(args2.iter()) {
                    args_unified |= self.unify(*a, *b);
                }

                let ret_unified = self.unify(*ret1, *ret2);

                args_unified && ret_unified
            }

            (IRType::Unreachable, _) => false,
            (_, IRType::Unreachable) => false,
            _ => a == b,
        }
    }

    fn infer_expr(&self, expr: &IRExpression) -> TypeId {
        match expr {
            IRExpression::Value(value) => self.infer_value(value),
            IRExpression::Call { function, args } => {
                let function_ty = self.infer_value(function);
                let function_ty = self.refresh_type(function_ty);

                let params = args
                    .iter()
                    .map(|arg| self.infer_value(arg))
                    .collect::<SmallVec<_>>();

                let ret_ty = self.Any(); // will be unified
                let func_call = IRType::Function {
                    args: params,
                    ret: ret_ty, // will be unified
                };
                let func_call_ty = self.add_type(func_call);

                self.unify(function_ty, func_call_ty);

                ret_ty
            }
        }
    }

    fn infer_value(&self, value: &IRValue) -> TypeId {
        match value {
            IRValue::Constant(constant) => self.infer_constant(&constant.value),
            IRValue::Local(var) => self.get_local(*var).metadata().ty.value,
            IRValue::Function(_) => todo!(),
            IRValue::BuiltinFunction(path) => {
                self.get_builtin(path).expect("Builtin function not found")
            }
            IRValue::Upvalue(_) => todo!(),
        }
    }

    fn infer_constant(&self, constant: &IRConstant) -> TypeId {
        match constant {
            IRConstant::Unit => self.Unit(),
            IRConstant::Bool(_) => self.Bool(),
            IRConstant::Int(_) => self.Int(),
            IRConstant::Float(_) => self.Float(),
            IRConstant::String(_) => self.String(),
        }
    }
}
