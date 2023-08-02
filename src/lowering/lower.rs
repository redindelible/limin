use std::collections::HashMap;
use crate::emit::{builder::{LIRBuilder, BlockBuilder}, lir};
use crate::lowering::mir;

pub fn lower(mir: mir::MIR) -> lir::LIR {
    Lowering::new().lower_mir(mir)
}


struct Lowering {
    struct_mapping: HashMap<mir::StructKey, lir::StructID>,
    function_mapping: HashMap<mir::FunctionKey, lir::FunctionID>,

    parameters_mapping: HashMap<(mir::FunctionKey, usize), lir::LocalID>,
    locals_mapping: HashMap<mir::LocalKey, lir::LocalID>,
}

impl Lowering {
    fn new() -> Lowering {
        Lowering {
            struct_mapping: HashMap::new(),
            function_mapping: HashMap::new(),

            parameters_mapping: HashMap::new(),
            locals_mapping: HashMap::new()
        }
    }

    fn lower_mir(mut self, mir: mir::MIR) -> lir::LIR {
        let mut builder = LIRBuilder::new();

        for (struct_key, _) in &mir.struct_prototypes {
            let id = builder.declare_struct();
            self.struct_mapping.insert(struct_key, id);
        }

        for (struct_key, struct_proto) in &mir.struct_prototypes {
            let struct_body = &mir.struct_bodies[struct_key];
            let id = self.struct_mapping[&struct_key];
            builder.define_struct(id, struct_proto.name.clone(), |builder| {
                for (field_name, field_type) in &struct_body.fields {
                    if let Some(lowered) = self.lower_type(&mir, field_type) {
                        builder.field(field_name.clone(), lowered);
                    }
                }
            });
        }

        for (function_key, _) in &mir.function_prototypes {
            let id = builder.declare_function();
            self.function_mapping.insert(function_key, id);
        }

        for (function_key, function_proto) in &mir.function_prototypes {
            let function_body = &mir.function_bodies[function_key];
            let id = self.function_mapping[&function_key];
            builder.define_function(id, function_proto.name.clone(), |mut builder| {
                if mir.is_never(&function_proto.ret) {
                    builder.return_void();
                } else if let Some(lowered) = self.lower_type(&mir, &function_proto.ret) {
                    builder.return_type(lowered);
                } else {
                    builder.return_void();
                }
                builder.parameter("$closure".into(), lir::Type::AnyRef);
                for (i, (param_name, param_type)) in function_proto.params.iter().enumerate() {
                    if let Some(lowered) = self.lower_type(&mir, param_type) {
                        let local = builder.parameter(param_name.clone(), lowered);
                        self.parameters_mapping.insert((function_key, i), local);
                    }
                }
                builder.build(|mut builder| {
                    let block_key = function_body.body;

                    self.lower_block(&mut builder, block_key, &mir);
                    let block = &mir.blocks[block_key];
                    if mir.is_never(&block.ret_type) {
                        // hopefully something already returned
                    } else if mir.is_zero_sized(&block.ret_type) {
                        builder.return_void();
                    } else {
                        builder.return_value(self.lower_type(&mir, &block.ret_type).unwrap());
                    }
                    builder.yield_diverges()
                })
            });
        }

        builder.main_function(self.function_mapping[&mir.main_fn]);

        builder.finish()
    }

    fn lower_block(&mut self, builder: &mut BlockBuilder, block_key: mir::BlockKey, mir: &mir::MIR) {
        let block = &mir.blocks[block_key];
        for stmt in &block.stmts {
            match stmt {
                mir::Stmt::Expr(expr) => {
                    let ty = self.lower_expr(builder, expr, mir);
                    if let Some(ty) = ty {
                        builder.pop(ty);
                    }
                }
                mir::Stmt::Decl(local, ty, value) => {
                    self.lower_expr(builder, value, mir);
                    if let Some(ty) = self.lower_type(mir, ty) {
                        let id = builder.declare_variable(ty);
                        self.locals_mapping.insert(*local, id);
                    }
                }
                mir::Stmt::Ret(value) => {
                    let ty = self.lower_expr(builder, value, mir);
                    if let Some(ty) = ty {
                        builder.return_value(ty);
                    } else {
                        builder.return_void();
                    }
                }
            }
        }
        self.lower_expr(builder, &block.ret, mir);
    }

    fn lower_expr(&mut self, builder: &mut BlockBuilder, expr: &mir::Expr, mir: &mir::MIR) -> Option<lir::Type> {
        match expr {
            mir::Expr::Unit => None,
            mir::Expr::Never => {
                builder.unreachable();
                None
            }
            mir::Expr::Integer(value) => {
                builder.load_i32(*value as i32);
                Some(lir::Type::Int32)
            }
            mir::Expr::Boolean(value) => {
                builder.load_bool(*value);
                Some(lir::Type::Boolean)
            }
            mir::Expr::Parameter(func, index) => {
                Some(builder.load_variable(self.parameters_mapping[&(*func, *index)]))
            }
            mir::Expr::LoadLocal(local) => {
                Some(builder.load_variable(self.locals_mapping[local]))
            }
            mir::Expr::LoadFunction(func) => {
                let ty = self.lower_type(mir, &mir.function_prototypes[*func].sig()).unwrap();
                let lir::Type::Tuple(tys) = &ty else { panic!() };
                let fty = tys[0].clone();
                let id = self.function_mapping[func];
                builder.load_function(id, fty.clone());
                builder.load_null();
                builder.create_tuple(vec![fty, lir::Type::AnyRef]);
                Some(ty)
            }
            mir::Expr::New(struct_key, fields) => {
                if mir.is_zero_sized(&mir::Type::Struct(*struct_key)) {
                    for (_, field_expr) in fields {
                        self.lower_expr(builder, field_expr, mir);
                    }
                    None
                } else {
                    let mut field_tys = Vec::new();
                    for (field_name, field_expr) in fields {
                        if let Some(ty) = self.lower_expr(builder, field_expr, mir) {
                            field_tys.push((field_name.clone(), ty));
                        }
                    }
                    let id = self.struct_mapping[struct_key];
                    builder.statepoint();
                    builder.create_struct(id, field_tys);
                    Some(lir::Type::StructRef(id))
                }
            }
            mir::Expr::GetAttr(struct_key, obj, field) => {
                let field_ty = &mir.struct_bodies[*struct_key].fields[field];
                if let Some(obj_ty) = self.lower_expr(builder, obj, mir) {
                    if let Some(field_ty) = self.lower_type(mir, field_ty) {
                        builder.get_field(self.struct_mapping[struct_key], (field.clone(), field_ty.clone()));
                        Some(field_ty)
                    } else {
                        builder.pop(obj_ty);
                        None
                    }
                } else {
                    None
                }
            }
            mir::Expr::Call(func, arguments) => {
                self.lower_expr(builder, func, mir).unwrap();
                let mir::Type::Function(params, ret) = mir.type_of(func) else { panic!() };
                if mir.is_never(&ret) || mir.is_zero_sized(&ret) {
                    let (params, _) = self.lower_fn_type(mir, &params, &ret);
                    builder.splat(vec![lir::Type::Function(params.clone(), None)]);
                    for arg in arguments {
                        self.lower_expr(builder, arg, mir);
                    }
                    builder.statepoint();
                    builder.call_void(&params);
                    None
                } else {
                    let (params, ret) = self.lower_fn_type(mir, &params, &ret);
                    let ret = ret.unwrap();
                    builder.splat(vec![
                        lir::Type::Function(params.clone(), Some(Box::new(ret.clone()))),
                        lir::Type::AnyRef
                    ]);
                    for arg in arguments {
                        self.lower_expr(builder, arg, mir);
                    }
                    builder.statepoint();
                    builder.call(&params, ret.clone());
                    Some(ret)
                }
            }
            mir::Expr::Block(block_key) => {
                let block = &mir.blocks[*block_key];
                if mir.is_never(&block.ret_type) {
                    let child = builder.build(|mut builder| {
                        self.lower_block(&mut builder, *block_key, mir);
                        builder.yield_diverges()
                    });
                    builder.block_diverge(child);
                    builder.unreachable();
                    None
                } else if mir.is_zero_sized(&block.ret_type) {
                    let child = builder.build(|mut builder| {
                        self.lower_block(&mut builder, *block_key, mir);
                        builder.yield_none()
                    });
                    builder.block_void(child);
                    None
                } else {
                    let ty = self.lower_type(mir, &block.ret_type).unwrap();
                    let child = builder.build(|mut builder| {
                        self.lower_block(&mut builder, *block_key, mir);
                        builder.yield_value(ty.clone())
                    });
                    builder.block_value(child);
                    Some(ty)
                }
            },
            mir::Expr::IfElse { cond, then_do, else_do, yield_type } => {
                self.lower_expr(builder, cond, mir).unwrap();
                if mir.is_never(yield_type) {
                    let then_do = builder.build(|mut builder| {
                        self.lower_expr(&mut builder, then_do, mir);
                        builder.yield_diverges()
                    });
                    let else_do = builder.build(|mut builder| {
                        self.lower_expr(&mut builder, else_do, mir);
                        builder.yield_diverges()
                    });
                    builder.if_else_diverge(then_do, else_do);
                    None
                } else if let Some(ty) = self.lower_type(mir, yield_type) {
                    let then_do = builder.build(|mut builder| {
                        self.lower_expr(&mut builder, then_do, mir);
                        if then_do.always_diverges(mir) {
                            builder.yield_diverges().into()
                        } else {
                            builder.yield_value(ty.clone()).into()
                        }
                    });
                    let else_do = builder.build(|mut builder| {
                        self.lower_expr(&mut builder, else_do, mir);
                        if else_do.always_diverges(mir) {
                            builder.yield_diverges().into()
                        } else {
                            builder.yield_value(ty.clone()).into()
                        }
                    });
                    builder.if_else_value(ty.clone(), then_do, else_do);
                    Some(ty)
                } else {
                    let then_do = builder.build(|mut builder| {
                        self.lower_expr(&mut builder, then_do, mir);
                        if then_do.always_diverges(mir) {
                            builder.yield_diverges().into()
                        } else {
                            builder.yield_none().into()
                        }
                    });
                    let else_do = builder.build(|mut builder| {
                        self.lower_expr(&mut builder, else_do, mir);
                        if else_do.always_diverges(mir) {
                            builder.yield_diverges().into()
                        } else {
                            builder.yield_none().into()
                        }
                    });
                    builder.if_else_void(then_do, else_do);
                    None
                }
            }
        }
    }

    fn lower_type(&self, mir: &mir::MIR, ty: &mir::Type) -> Option<lir::Type> {
        match ty {
            mir::Type::Unit => None,
            mir::Type::Never => panic!(),
            mir::Type::Boolean => Some(lir::Type::Boolean),
            mir::Type::Integer(bits) => {
                match *bits {
                    32 => Some(lir::Type::Int32),
                    _ => todo!()
                }
            },
            mir::Type::Struct(key) => {
                if mir.is_zero_sized(ty) {
                    None
                } else if mir.is_never(ty) {
                    panic!()
                } else {
                    Some(lir::Type::StructRef(self.struct_mapping[key]))
                }
            },
            mir::Type::Function(params, ret) => {
                let (lowered_params, lowered_ret) = self.lower_fn_type(mir, params, ret);
                Some(lir::Type::Tuple(vec![
                    lir::Type::Function(lowered_params, lowered_ret.map(Box::new)),
                    lir::Type::AnyRef
                ]))
            }
        }
    }

    fn lower_fn_type(&self, mir: &mir::MIR, params: &[mir::Type], ret: &mir::Type) -> (Vec<lir::Type>, Option<lir::Type>) {
        let mut lowered_params = vec![lir::Type::AnyRef];
        lowered_params.extend(params.iter().filter_map(|ty| self.lower_type(mir, ty)));
        let lowered_ret = self.lower_type(mir, ret);
        (lowered_params, lowered_ret)
    }
}