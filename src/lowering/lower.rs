use std::collections::HashMap;
use crate::emit::{builder::{LIRBuilder, BlockBuilder}, lir};
use crate::emit::lir::{LocalID, StructID};
use crate::lowering::mir;

pub fn lower(mir: mir::MIR) -> lir::LIR {
    Lowering::new().lower_mir(mir)
}


struct Lowering {
    struct_mapping: HashMap<mir::StructKey, lir::StructID>,
    function_mapping: HashMap<mir::FunctionKey, lir::FunctionID>,
}

impl Lowering {
    fn new() -> Lowering {
        Lowering {
            struct_mapping: HashMap::new(),
            function_mapping: HashMap::new()
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
                let mut fn_lowering = FunctionLowering::new(&self, &function_proto.name);

                if mir.is_never(&function_proto.ret) {
                    builder.return_void();
                } else if let Some(lowered) = self.lower_type(&mir, &function_proto.ret) {
                    builder.return_type(lowered);
                } else {
                    builder.return_void();
                }
                builder.parameter("$closure".into(), lir::Type::AnyRef);

                let mut parameter_locals: HashMap<mir::LocalKey, LocalID> = HashMap::new();
                for (i, (param_name, param_type)) in function_proto.params.iter().enumerate() {
                    if let Some(lowered) = self.lower_type(&mir, param_type) {
                        let local = builder.parameter(param_name.clone(), lowered);
                        parameter_locals.insert(function_body.params[i], local);
                    }
                }

                builder.build(|mut builder| {
                    let block_key = function_body.body;
                    fn_lowering.lower_block(&mut builder, block_key, &mir, |fn_lowering, builder| {
                        for (i, (_, param_type)) in function_proto.params.iter().enumerate() {
                            if let Some(lowered) = self.lower_type(&mir, param_type) {
                                let key = function_body.params[i];
                                let info = &mir.locals[key];
                                let local = parameter_locals[&key];

                                if info.is_closed {
                                    builder.load_variable(fn_lowering.block_captures[&info.block].obj);
                                    builder.load_variable(local);
                                    builder.set_field(fn_lowering.block_captures[&info.block].capture_ty, (fn_lowering.block_captures[&info.block].captures[&key].clone(), lowered));
                                } else {
                                    fn_lowering.locals_mapping.insert(function_body.params[i], local);
                                }
                            }
                        }
                    });
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


struct FunctionLowering<'a> {
    lowering: &'a Lowering,

    fn_name: String,
    capture_count: usize,
    closure_count: usize,
    block_captures: HashMap<mir::BlockKey, CaptureInfo>,
    locals_mapping: HashMap<mir::LocalKey, lir::LocalID>,
}

struct CaptureInfo {
    obj: lir::LocalID,
    capture_ty: StructID,
    captures: HashMap<mir::LocalKey, String>
}

impl<'a> FunctionLowering<'a> {
    fn new(lowering: &'a Lowering, name: impl Into<String>) -> FunctionLowering<'a> {
        Self {
            lowering,

            fn_name: name.into(),
            capture_count: 0,
            closure_count: 0,
            block_captures: HashMap::new(),
            locals_mapping: HashMap::new()
        }
    }

    fn lower_block(&mut self, builder: &mut BlockBuilder, block_key: mir::BlockKey, mir: &mir::MIR, after_captures: impl FnOnce(&mut Self, &mut BlockBuilder)) {
        if mir.blocks[block_key].locals.iter().any(|local| mir.locals[*local].is_closed) {
            let capture_ty = builder.lir_builder.declare_struct();
            let name = format!("{}_capture_{}", &self.fn_name, self.capture_count);
            self.capture_count += 1;

            let mut captures: HashMap<mir::LocalKey, String> = HashMap::new();

            builder.lir_builder.define_struct(capture_ty, name, |mut builder| {
                let block = &mir.blocks[block_key];
                for (i, local) in block.locals.iter().enumerate() {
                    let local_info = &mir.locals[*local];
                    if local_info.is_closed {
                        if let Some(ty) = self.lower_type(mir, &local_info.typ) {
                            let name = format!("{i}");
                            builder.field(name.clone(), ty);
                            captures.insert(*local, name);
                        }
                    }
                }
            });

            builder.create_zero_init_struct(capture_ty);
            let capture = builder.declare_variable(lir::Type::StructRef(capture_ty));

            self.block_captures.insert(block_key, CaptureInfo { obj: capture, capture_ty, captures });
        };

        after_captures(self, builder);

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
                    if let Some(ty) = self.lower_type(mir, ty) {
                        if mir.locals[*local].is_closed {
                            builder.load_variable(self.block_captures[&mir.locals[*local].block].obj);
                            self.lower_expr(builder, value, mir);
                            builder.set_field(self.block_captures[&mir.locals[*local].block].capture_ty, (self.block_captures[&mir.locals[*local].block].captures[local].clone(), ty.clone()));
                        } else {
                            self.lower_expr(builder, value, mir);
                            let id = builder.declare_variable(ty);
                            self.locals_mapping.insert(*local, id);
                        }
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
            mir::Expr::LoadLocal(local) => {
                if mir.locals[*local].is_closed {
                    if let Some(ty) = self.lower_type(mir, &mir.locals[*local].typ) {
                        let capture_info = &self.block_captures[&mir.locals[*local].block];
                        builder.load_variable(capture_info.obj);
                        builder.get_field(capture_info.capture_ty, (capture_info.captures[local].clone(), ty.clone()));
                        Some(ty)
                    } else {
                        None
                    }
                } else {
                    Some(builder.load_variable(self.locals_mapping[local]))
                }
            }
            mir::Expr::LoadFunction(func) => {
                let ty = self.lower_type(mir, &mir.function_prototypes[*func].sig()).unwrap();
                let lir::Type::Tuple(tys) = &ty else { panic!() };
                let fty = tys[0].clone();
                let id = self.lowering.function_mapping[func];
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
                    let id = self.lowering.struct_mapping[struct_key];
                    builder.statepoint();
                    builder.create_struct(id, field_tys);
                    Some(lir::Type::StructRef(id))
                }
            }
            mir::Expr::GetAttr(struct_key, obj, field) => {
                let field_ty = &mir.struct_bodies[*struct_key].fields[field];
                if let Some(obj_ty) = self.lower_expr(builder, obj, mir) {
                    if let Some(field_ty) = self.lower_type(mir, field_ty) {
                        builder.get_field(self.lowering.struct_mapping[struct_key], (field.clone(), field_ty.clone()));
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
                    let (params, _) = self.lowering.lower_fn_type(mir, &params, &ret);
                    builder.splat(vec![lir::Type::Function(params.clone(), None)]);
                    for arg in arguments {
                        self.lower_expr(builder, arg, mir);
                    }
                    builder.statepoint();
                    builder.call_void(&params);
                    None
                } else {
                    let (params, ret) = self.lowering.lower_fn_type(mir, &params, &ret);
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
                        self.lower_block(&mut builder, *block_key, mir, |_, _| ());
                        builder.yield_diverges()
                    });
                    builder.block_diverge(child);
                    builder.unreachable();
                    None
                } else if mir.is_zero_sized(&block.ret_type) {
                    let child = builder.build(|mut builder| {
                        self.lower_block(&mut builder, *block_key, mir, |_, _| ());
                        builder.yield_none()
                    });
                    builder.block_void(child);
                    None
                } else {
                    let ty = self.lower_type(mir, &block.ret_type).unwrap();
                    let child = builder.build(|mut builder| {
                        self.lower_block(&mut builder, *block_key, mir, |_, _| ());
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
            },
            mir::Expr::Closure { parameters, ret_type, body, closed_blocks } => {
                let closure_ty = builder.lir_builder.declare_struct();
                let name = format!("{}_closure_{}_captures", &self.fn_name, self.closure_count);
                builder.lir_builder.define_struct(closure_ty, name, |mut builder| {
                    for (i, closed) in closed_blocks.iter().enumerate() {
                        builder.field(format!("{i}"), lir::Type::StructRef(self.block_captures[closed].capture_ty));
                    }
                });

                let closure_fn = builder.lir_builder.declare_function();
                let name = format!("{}_closure_{}", &self.fn_name, self.closure_count);
                let sig = builder.lir_builder.define_function(closure_fn, name.clone(), |mut builder| {
                    let mut closure_lowering = FunctionLowering::new(self.lowering, name);

                    if let Some(ret_ty) = self.lower_type(mir, ret_type) {
                        builder.return_type(ret_ty);
                    } else {
                        builder.return_void()
                    }

                    let closure = builder.parameter("$closure".into(), lir::Type::AnyRef);

                    let mut parameter_locals: HashMap<mir::LocalKey, lir::LocalID> = HashMap::new();
                    for param in parameters.iter() {
                        if let Some(lowered) = self.lower_type(&mir, &param.typ) {
                            let local = builder.parameter(param.name.clone(), lowered);
                            parameter_locals.insert(param.key, local);
                        }
                    }

                    builder.build(|mut builder| {
                        for (i, closed) in closed_blocks.iter().enumerate() {
                            let capture_info = &self.block_captures[closed];
                            builder.load_variable(closure);
                            builder.downcast_ref(lir::Type::StructRef(closure_ty));
                            builder.get_field(closure_ty, (format!("{i}"), lir::Type::StructRef(capture_info.capture_ty)));
                            let local = builder.declare_variable(lir::Type::StructRef(capture_info.capture_ty));
                            closure_lowering.block_captures.insert(*closed, CaptureInfo {
                                obj: local,
                                capture_ty: capture_info.capture_ty,
                                captures: capture_info.captures.clone()
                            });
                        }

                        closure_lowering.lower_block(&mut builder, *body, mir, |closure_lowering, builder| {
                            for (i, param) in parameters.iter().enumerate() {
                                if let Some(lowered) = self.lower_type(&mir, &param.typ) {
                                    let info = &mir.locals[param.key];
                                    let local = parameter_locals[&param.key];

                                    if info.is_closed {
                                        builder.load_variable(closure_lowering.block_captures[&info.block].obj);
                                        builder.load_variable(local);
                                        builder.set_field(closure_lowering.block_captures[&info.block].capture_ty, (closure_lowering.block_captures[&info.block].captures[&param.key].clone(), lowered));
                                    } else {
                                        closure_lowering.locals_mapping.insert(param.key, local);
                                    }
                                }
                            }
                        });
                        let block = &mir.blocks[*body];
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

                self.closure_count += 1;

                builder.load_function(closure_fn, sig.clone());

                let mut field_names = Vec::new();
                for (i, closed) in closed_blocks.iter().enumerate() {
                    let capture_info = &self.block_captures[closed];
                    builder.load_variable(capture_info.obj);
                    field_names.push((format!("{i}"), lir::Type::StructRef(capture_info.capture_ty)));
                }
                builder.create_struct(closure_ty, field_names);
                builder.upcast_ref(lir::Type::StructRef(closure_ty));

                builder.create_tuple(vec![
                    sig.clone(),
                    lir::Type::AnyRef
                ]);

                Some(lir::Type::Tuple(vec![
                    sig,
                    lir::Type::AnyRef
                ]))
            }
        }
    }

    fn lower_type(&self, mir: &mir::MIR, ty: &mir::Type) -> Option<lir::Type> {
        self.lowering.lower_type(mir, ty)
    }
}