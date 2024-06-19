use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::middle::{
    dump::Dump, Context, IRConstant, IRExpression, IRFunctionInner, IRStatement, IRType,
    IRTypeInner, IRValue, TypeId,
};

pub fn inference(func: &mut IRFunctionInner, ctx: &mut Context) {
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
    func: &'a IRFunctionInner,
    ctx: &'a mut Context,
    errors: Vec<String>,
}

impl<'a> InferencePass<'a> {
    fn check(&mut self, statement: &IRStatement) {
        let expr_ty = self.infer_expr(&statement.expression);
        let target_ty = self.func.get_local(&statement.target).metadata().ty.value;

        if !self.unify(target_ty, expr_ty) {
            let target = self.grab_type(target_ty);

            // if the target is rigid, it is an error
            if target.is_rigid() {
                let expr = self.grab_type(expr_ty);

                self.errors.push(format!(
                    "Type mismatch: {} != {}",
                    expr.dump(self.ctx),
                    target.dump(self.ctx)
                ));
            }

            self.update_type_rigid(target_ty, IRTypeInner::Any);
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
                    let mut generics = HashMap::new();
                    let new_ret = self.free_type(ret);

                    generics.insert(ret, new_ret);

                    let args = args
                        .iter()
                        .map(|ty| match generics.get(ty) {
                            Some(new_ty) => *new_ty,
                            None => {
                                let new_ty = self.free_type(*ty);
                                generics.insert(*ty, new_ty);
                                new_ty
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
    }

    fn infer_expr(&mut self, expr: &IRExpression) -> TypeId {
        match expr {
            IRExpression::Value(value) => self.infer_value(value),
            IRExpression::Call {
                function,
                args,
                resolved_function,
            } => {
                let function_ty = self.infer_value(function);
                let function_ty = self.free_function(function_ty);

                let params = args
                    .iter()
                    .map(|arg| {
                        let ty = self.infer_value(arg);
                        // we free the type because we don't want to update it based on fn call
                        // self.free_type(ty)
                        // removed this free because it fucks up monomorphization,
                        // as a result, if there is ever a rigid Any, it eats the generics
                        // small price to pay for sanity
                        ty
                    })
                    .collect::<SmallVec<_>>();

                let ret_ty = self.ctx.Any(); // will be unified
                let func_call = IRTypeInner::Function {
                    args: params,
                    ret: ret_ty, // will be unified
                };

                let func_call_ty = self.ctx.new_free_type(func_call);

                // if it fails to unify, the type will be Any, lock it
                if !self.unify(func_call_ty, function_ty) {
                    let f = self.grab_type(function_ty);
                    let c = self.grab_type(func_call_ty);

                    self.errors.push(format!(
                        "Type mismatch: {} != {}",
                        c.dump(self.ctx),
                        f.dump(self.ctx),
                    ));

                    self.lock_type(ret_ty);
                }

                *resolved_function.borrow_mut() = Some(func_call_ty);

                ret_ty
            }
        }
    }

    fn infer_value(&mut self, value: &IRValue) -> TypeId {
        match value {
            IRValue::Constant(constant) => self.infer_constant(&constant.value),
            IRValue::Local(var) => self.func.get_local(var).metadata().ty.value,
            IRValue::Function(path) => self
                .ctx
                .functions
                .get(path)
                .expect("Function not found")
                .ty(),
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
