use std::collections::HashMap;
use crate::emit::{builder::{LIRBuilder, BlockBuilder}, lir};
use crate::lowering::mir;

pub fn lower(mir: mir::MIR) -> lir::LIR {
    Lowering::new().lower_mir(mir)
}


enum Lowered<T> {
    Empty,
    Unit,
    Some(T)
}

impl<T> Lowered<T> {
    fn to_option(self) -> Option<T> {
        match self {
            Lowered::Unit => None,
            Lowered::Empty => None,
            Lowered::Some(ty) => Some(ty)
        }
    }

    fn unwrap(self) -> T {
        match self {
            Lowered::Some(t) => t,
            _ => panic!()
        }
    }
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
                for (field_name, field_type) in struct_body.all_fields(&mir) {
                    if let Lowered::Some(field_type) = self.lower_type(&mir, field_type) {
                        builder.field(field_name.clone(), field_type);
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

                if let Lowered::Some(lowered) = self.lower_type(&mir, &function_proto.fn_type.ret) {
                    builder.return_type(lowered);
                } else {
                    builder.return_void();
                }
                builder.parameter("$closure".into(), lir::Type::AnyGc);

                let mut parameter_locals: HashMap<mir::LocalKey, lir::LocalID> = HashMap::new();
                for (i, param_type) in function_proto.fn_type.params.iter().enumerate() {
                    if let Lowered::Some(lowered) = self.lower_type(&mir, param_type) {
                        let local = builder.parameter(function_body.params[i].0.clone(), lowered);
                        parameter_locals.insert(function_body.params[i].1, local);
                    }
                }

                builder.build(|mut builder| {
                    let block_key = function_body.body;
                    let block_yield = fn_lowering.lower_block(&mut builder, block_key, &mir, |fn_lowering, builder| {
                        for (i, param_type) in function_proto.fn_type.params.iter().enumerate() {
                            if let Lowered::Some(lowered) = self.lower_type(&mir, param_type) {
                                let key = function_body.params[i].1;
                                let info = &mir.locals[key];
                                let local = parameter_locals[&key];

                                if info.is_closed {
                                    builder.load_variable(fn_lowering.block_captures[&info.block].obj);
                                    builder.load_variable(local);
                                    builder.set_gc_field(fn_lowering.block_captures[&info.block].capture_ty, (fn_lowering.block_captures[&info.block].captures[&key].clone(), lowered));
                                } else {
                                    fn_lowering.locals_mapping.insert(function_body.params[i].1, local);
                                }
                            }
                        }
                    });
                    match block_yield {
                        Lowered::Some(ty) => builder.return_value(ty),
                        Lowered::Unit => builder.return_void(),
                        Lowered::Empty => ()
                    }
                    builder.yield_diverges()
                })
            });
        }

        builder.main_function(self.function_mapping[&mir.main_fn]);

        builder.finish()
    }

    fn lower_type(&self, mir: &mir::MIR, ty: &mir::Type) -> Lowered<lir::Type> {
        match ty {
            mir::Type::Unit => Lowered::Unit,
            mir::Type::Never => Lowered::Empty,
            mir::Type::Boolean => Lowered::Some(lir::Type::Boolean),
            mir::Type::Integer(bits) => {
                match *bits {
                    32 => Lowered::Some(lir::Type::Int32),
                    _ => todo!()
                }
            },
            mir::Type::Struct(key) => {
                if mir.is_zero_sized(ty) {
                    Lowered::Unit
                } else if mir.is_empty(ty) {
                    Lowered::Empty
                } else {
                    Lowered::Some(lir::Type::Struct(self.struct_mapping[key]))
                }
            },
            mir::Type::Gc(inner) => {
                Lowered::Some(match self.lower_type(mir, inner) {
                    Lowered::Empty | Lowered::Unit => lir::Type::AnyGc,
                    Lowered::Some(inner) => lir::Type::Gc(Box::new(inner))
                })
            }
            mir::Type::Ref(inner) => { todo!() }
            mir::Type::Function(mir::FunctionType { params, ret }) => {
                let mut lowered_params = vec![lir::Type::AnyGc];
                lowered_params.extend(params.iter().filter_map(|ty|
                    match self.lower_type(mir, ty) {
                        Lowered::Unit => None,
                        Lowered::Empty => unreachable!(),
                        Lowered::Some(ty) => Some(ty)
                    }
                ));
                let lowered_ret = self.lower_type(mir, ret).to_option();
                Lowered::Some(lir::Type::Tuple(vec![
                    lir::Type::Function(lowered_params, lowered_ret.map(Box::new)),
                    lir::Type::AnyGc
                ]))
            }
        }
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
    capture_ty: lir::StructID,
    captures: HashMap<mir::LocalKey, String>
}


#[derive(Debug)]
pub enum AnyBlock {
    Value(lir::Type, lir::BlockValue),
    Void(lir::BlockVoid),
    Diverge(lir::BlockDiverge)
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

    #[must_use]
    fn lower_block(&mut self, builder: &mut BlockBuilder, block_key: mir::BlockKey, mir: &mir::MIR, after_captures: impl FnOnce(&mut Self, &mut BlockBuilder)) -> Lowered<lir::Type> {
        if mir.blocks[block_key].locals.iter().any(|local| mir.locals[*local].is_closed) {
            let capture_ty = builder.lir_builder.declare_struct();
            let name = format!("{}_capture_{}", &self.fn_name, self.capture_count);
            self.capture_count += 1;

            let mut captures: HashMap<mir::LocalKey, String> = HashMap::new();

            builder.lir_builder.define_struct(capture_ty, name, |builder| {
                let block = &mir.blocks[block_key];
                for (i, local) in block.locals.iter().enumerate() {
                    let local_info = &mir.locals[*local];
                    if local_info.is_closed {
                        if let Lowered::Some(ty) = self.lower_type(mir, &local_info.typ) {
                            let name = format!("{i}");
                            builder.field(name.clone(), ty);
                            captures.insert(*local, name);
                        }
                    }
                }
            });

            builder.create_zero_init_gc_struct(capture_ty);
            let capture = builder.declare_variable(lir::Type::gc_struct(capture_ty));

            self.block_captures.insert(block_key, CaptureInfo { obj: capture, capture_ty, captures });
        };

        after_captures(self, builder);

        let block = &mir.blocks[block_key];

        for stmt in &block.stmts {
            match stmt {
                mir::Stmt::Expr(expr) => {
                    match self.lower_expr(builder, expr, mir) {
                        Lowered::Unit => (),
                        Lowered::Empty => return Lowered::Empty,
                        Lowered::Some(ty) => builder.pop(ty)
                    }
                }
                mir::Stmt::Decl(local, _, value) => {
                    match self.lower_expr(builder, value, mir) {
                        Lowered::Unit => (),
                        Lowered::Empty => return Lowered::Empty,
                        Lowered::Some(ty) => {
                            if mir.locals[*local].is_closed {
                                builder.load_variable(self.block_captures[&mir.locals[*local].block].obj);
                                builder.set_gc_field(self.block_captures[&mir.locals[*local].block].capture_ty, (self.block_captures[&mir.locals[*local].block].captures[local].clone(), ty.clone()));
                            } else {
                                let id = builder.declare_variable(ty);
                                self.locals_mapping.insert(*local, id);
                            }
                        }
                    }
                }
                mir::Stmt::Ret(value) => {
                    match self.lower_expr(builder, value, mir) {
                        Lowered::Unit => builder.return_void(),
                        Lowered::Empty => return Lowered::Empty,
                        Lowered::Some(ty) => builder.return_value(ty)
                    }
                }
            }
        }
        self.lower_expr(builder, &block.ret, mir)
    }

    #[must_use]
    fn lower_expr(&mut self, builder: &mut BlockBuilder, expr: &mir::Expr, mir: &mir::MIR) -> Lowered<lir::Type> {
        match expr {
            mir::Expr::Unit => Lowered::Unit,
            mir::Expr::Never => {
                builder.unreachable();
                Lowered::Empty
            }
            mir::Expr::Integer(value) => {
                builder.load_i32(*value as i32);
                Lowered::Some(lir::Type::Int32)
            }
            mir::Expr::Boolean(value) => {
                builder.load_bool(*value);
                Lowered::Some(lir::Type::Boolean)
            }
            mir::Expr::LoadLocal(local) => {
                if mir.locals[*local].is_closed {
                    match self.lower_type(mir, &mir.locals[*local].typ) {
                        Lowered::Unit => Lowered::Unit,
                        Lowered::Empty => return Lowered::Empty,
                        Lowered::Some(ty) => {
                            let capture_info = &self.block_captures[&mir.locals[*local].block];
                            builder.load_variable(capture_info.obj);
                            builder.get_gc_field(capture_info.capture_ty, (capture_info.captures[local].clone(), ty.clone()));
                            Lowered::Some(ty)
                        }
                    }
                } else {
                    Lowered::Some(builder.load_variable(self.locals_mapping[local]))
                }
            }
            mir::Expr::LoadFunction(func) => {
                let ty = self.lower_type(mir, &mir.function_prototypes[*func].sig()).unwrap();
                let lir::Type::Tuple(tys) = &ty else { panic!() };
                let fty = tys[0].clone();
                let id = self.lowering.function_mapping[func];
                builder.load_function(id, fty.clone());
                builder.load_null();
                builder.create_tuple(vec![fty, lir::Type::AnyGc]);
                Lowered::Some(ty)
            }
            mir::Expr::DerefGc(value) => {
                match self.lower_expr(builder, value, mir) {
                    Lowered::Unit => Lowered::Unit,
                    Lowered::Empty => panic!(),
                    Lowered::Some(value) => {
                        match value {
                            lir::Type::Gc(contained) => {
                                builder.deref_gc(contained.as_ref().clone());
                                Lowered::Some(*contained)
                            }
                            lir::Type::AnyGc => {
                                builder.pop(lir::Type::AnyGc);
                                Lowered::Unit
                            }
                            _ => panic!()
                        }
                    }
                }
            }
            mir::Expr::CreateStruct(struct_key, fields) => {
                if mir.is_zero_sized(&mir::Type::Struct(*struct_key)) {
                    for (_, field_expr) in fields {
                        match self.lower_expr(builder, field_expr, mir) {
                            Lowered::Unit => (),
                            Lowered::Empty => return Lowered::Empty,
                            Lowered::Some(_) => unreachable!()
                        }
                    }
                    Lowered::Unit
                } else {
                    let mut field_tys = Vec::new();
                    for (field_name, field_expr) in fields {
                        match self.lower_expr(builder, field_expr, mir) {
                            Lowered::Unit => (),
                            Lowered::Empty => return Lowered::Empty,
                            Lowered::Some(ty) => field_tys.push((field_name.clone(), ty))
                        }
                    }
                    let id = self.lowering.struct_mapping[struct_key];
                    builder.statepoint();
                    builder.create_struct(id, field_tys);
                    Lowered::Some(lir::Type::Struct(id))
                }
            }
            mir::Expr::New(value) => {
                match self.lower_expr(builder, value, mir) {
                    Lowered::Unit => {
                        builder.load_null();
                        Lowered::Some(lir::Type::AnyGc)
                    },
                    Lowered::Empty => panic!(),
                    Lowered::Some(value) => {
                        builder.create_new(value.clone());
                        Lowered::Some(lir::Type::Gc(Box::new(value)))
                    }
                }
            }
            mir::Expr::GetAttr(struct_key, obj, field) => {
                match self.lower_expr(builder, obj, mir) {
                    Lowered::Unit => Lowered::Unit,
                    Lowered::Empty => return Lowered::Empty,
                    Lowered::Some(obj_ty) => {
                        let field_ty = mir.struct_bodies[*struct_key].all_fields(mir)[field];
                        match self.lower_type(mir, field_ty) {
                            Lowered::Unit => {
                                builder.pop(obj_ty);
                                Lowered::Unit
                            },
                            Lowered::Empty => unreachable!(),
                            Lowered::Some(field_ty) => {
                                builder.get_field(self.lowering.struct_mapping[struct_key], (field.clone(), field_ty.clone()));
                                Lowered::Some(field_ty)
                            }
                        }
                    }
                }
            }
            mir::Expr::Call(fn_type, callee, arguments) => {
                match self.lower_expr(builder, callee, mir) {
                    Lowered::Unit => panic!(),
                    Lowered::Empty => return Lowered::Empty,
                    Lowered::Some(_) => ()
                }

                let mir::FunctionType { params, ret } = fn_type;
                let mut param_types = vec![lir::Type::AnyGc];
                for param in params {
                    match self.lower_type(mir, param) {
                        Lowered::Unit => (),
                        Lowered::Empty => panic!(),
                        Lowered::Some(ty) => param_types.push(ty)
                    }
                }

                match self.lower_type(mir, ret) {
                    result @ (Lowered::Unit | Lowered::Empty) => {
                        builder.splat(vec![lir::Type::Function(param_types.clone(), None), lir::Type::AnyGc]);
                        for arg in arguments {
                            match self.lower_expr(builder, arg, mir) {
                                Lowered::Empty => {
                                    return Lowered::Empty
                                },
                                Lowered::Unit | Lowered::Some(_) => ()
                            }
                        }
                        builder.statepoint();
                        builder.call_void(&param_types);
                        result
                    }
                    Lowered::Some(ret_type) => {
                        builder.splat(vec![
                            lir::Type::Function(param_types.clone(), Some(Box::new(ret_type.clone()))),
                            lir::Type::AnyGc
                        ]);
                        for arg in arguments {
                            match self.lower_expr(builder, arg, mir) {
                                Lowered::Empty => {
                                    return Lowered::Empty
                                },
                                Lowered::Unit | Lowered::Some(_) => ()
                            }
                        }
                        builder.statepoint();
                        builder.call(&param_types, ret_type.clone());
                        Lowered::Some(ret_type.clone())
                    }
                }
            }
            mir::Expr::Block(block_key) => {
                let child = builder.build(|mut builder| {
                    match self.lower_block(&mut builder, *block_key, mir, |_, _| ()) {
                        Lowered::Unit => AnyBlock::Void(builder.yield_none()),
                        Lowered::Empty => AnyBlock::Diverge(builder.yield_diverges()),
                        Lowered::Some(ty) => AnyBlock::Value(ty.clone(), builder.yield_value(ty))
                    }
                });

                match child {
                    AnyBlock::Void(block) => {
                        builder.block_void(block);
                        Lowered::Unit
                    },
                    AnyBlock::Diverge(block) => {
                        builder.block_diverge(block);
                        Lowered::Empty
                    },
                    AnyBlock::Value(ty, block) => {
                        builder.block_value(block);
                        Lowered::Some(ty)
                    }
                }
            },
            mir::Expr::IfElse { cond, then_do, else_do, yield_type } => {
                self.lower_expr(builder, cond, mir).unwrap();
                match self.lower_type(mir, yield_type) {
                    Lowered::Unit => {
                        let then_do: lir::BlockVoidOrDiverge = builder.build(|mut builder| {
                            match self.lower_expr(&mut builder, then_do, mir) {
                                Lowered::Unit => builder.yield_none().into(),
                                Lowered::Empty => builder.yield_diverges().into(),
                                Lowered::Some(_) => unreachable!()
                            }
                        });
                        let else_do: lir::BlockVoidOrDiverge = builder.build(|mut builder| {
                            match self.lower_expr(&mut builder, else_do, mir) {
                                Lowered::Unit => builder.yield_none().into(),
                                Lowered::Empty => builder.yield_diverges().into(),
                                Lowered::Some(_) => unreachable!()
                            }
                        });
                        if then_do.diverges() && else_do.diverges() {
                            builder.if_else_diverge(then_do.into_diverges().unwrap(), else_do.into_diverges().unwrap());
                            Lowered::Empty
                        } else {
                            builder.if_else_void(then_do, else_do);
                            Lowered::Unit
                        }
                    }
                    Lowered::Empty => {
                        let then_do = builder.build(|mut builder| {
                            match self.lower_expr(&mut builder, then_do, mir) {
                                Lowered::Unit => unreachable!(),
                                Lowered::Empty => builder.yield_diverges().into(),
                                Lowered::Some(_) => unreachable!()
                            }
                        });
                        let else_do = builder.build(|mut builder| {
                            match self.lower_expr(&mut builder, else_do, mir) {
                                Lowered::Unit => unreachable!(),
                                Lowered::Empty => builder.yield_diverges().into(),
                                Lowered::Some(_) => unreachable!()
                            }
                        });
                        builder.if_else_diverge(then_do, else_do);
                        Lowered::Empty
                    }
                    Lowered::Some(ty) => {
                        let then_do: lir::BlockValueOrDiverge = builder.build(|mut builder| {
                            match self.lower_expr(&mut builder, then_do, mir) {
                                Lowered::Unit => unreachable!(),
                                Lowered::Empty => builder.yield_diverges().into(),
                                Lowered::Some(ty) => builder.yield_value(ty).into()
                            }
                        });
                        let else_do: lir::BlockValueOrDiverge = builder.build(|mut builder| {
                            match self.lower_expr(&mut builder, else_do, mir) {
                                Lowered::Unit => unreachable!(),
                                Lowered::Empty => builder.yield_diverges().into(),
                                Lowered::Some(ty) => builder.yield_value(ty).into()
                            }
                        });
                        if then_do.diverges() && else_do.diverges() {
                            builder.if_else_diverge(then_do.into_diverges().unwrap(), else_do.into_diverges().unwrap());
                            Lowered::Empty
                        } else {
                            builder.if_else_value(ty.clone(), then_do, else_do);
                            Lowered::Some(ty)
                        }
                    }
                }
            },
            mir::Expr::Closure { parameters, fn_type, body, closed_blocks } => {
                let closure_ty = builder.lir_builder.declare_struct();
                let name = format!("{}_closure_{}_captures", &self.fn_name, self.closure_count);
                builder.lir_builder.define_struct(closure_ty, name, |builder| {
                    for (i, closed) in closed_blocks.iter().enumerate() {
                        builder.field(format!("{i}"), lir::Type::gc_struct(self.block_captures[closed].capture_ty));
                    }
                });

                let closure_fn = builder.lir_builder.declare_function();
                let name = format!("{}_closure_{}", &self.fn_name, self.closure_count);
                let sig = builder.lir_builder.define_function(closure_fn, name.clone(), |mut builder| {
                    let mut closure_lowering = FunctionLowering::new(self.lowering, name);

                    match self.lower_type(mir, &fn_type.ret) {
                        Lowered::Empty | Lowered::Unit => builder.return_void(),
                        Lowered::Some(ret_ty) => builder.return_type(ret_ty)
                    }

                    let closure = builder.parameter("$closure".into(), lir::Type::AnyGc);

                    let mut parameter_locals: HashMap<mir::LocalKey, lir::LocalID> = HashMap::new();
                    for param in parameters.iter() {
                        match self.lower_type(&mir, &param.typ) {
                            Lowered::Unit => (),
                            Lowered::Empty => unreachable!(),
                            Lowered::Some(lowered) => {
                                let local = builder.parameter(param.name.clone(), lowered);
                                parameter_locals.insert(param.key, local);
                            }
                        }
                    }

                    builder.build(|mut builder| {
                        for (i, closed) in closed_blocks.iter().enumerate() {
                            let capture_info = &self.block_captures[closed];
                            builder.load_variable(closure);
                            builder.downcast_gc(lir::Type::gc_struct(closure_ty));
                            builder.get_gc_field(closure_ty, (format!("{i}"), lir::Type::gc_struct(capture_info.capture_ty)));
                            let local = builder.declare_variable(lir::Type::gc_struct(capture_info.capture_ty));
                            closure_lowering.block_captures.insert(*closed, CaptureInfo {
                                obj: local,
                                capture_ty: capture_info.capture_ty,
                                captures: capture_info.captures.clone()
                            });
                        }

                        let block_yield = closure_lowering.lower_block(&mut builder, *body, mir, |closure_lowering, builder| {
                            for param in parameters {
                                // todo replace with iterating through parameter_locals
                                match self.lower_type(&mir, &param.typ) {
                                    Lowered::Unit => (),
                                    Lowered::Empty => unreachable!(),
                                    Lowered::Some(lowered) => {
                                        let info = &mir.locals[param.key];
                                        let local = parameter_locals[&param.key];

                                        if info.is_closed {
                                            builder.load_variable(closure_lowering.block_captures[&info.block].obj);
                                            builder.load_variable(local);
                                            builder.set_gc_field(closure_lowering.block_captures[&info.block].capture_ty, (closure_lowering.block_captures[&info.block].captures[&param.key].clone(), lowered));
                                        } else {
                                            closure_lowering.locals_mapping.insert(param.key, local);
                                        }
                                    }
                                }
                            }
                        });
                        match block_yield {
                            Lowered::Unit => builder.return_void(),
                            Lowered::Empty => (),
                            Lowered::Some(ty) => builder.return_value(ty)
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
                    field_names.push((format!("{i}"), lir::Type::gc_struct(capture_info.capture_ty)));
                }
                builder.create_struct(closure_ty, field_names);
                builder.create_new(lir::Type::Struct(closure_ty));
                builder.upcast_gc(lir::Type::gc_struct(closure_ty));

                builder.create_tuple(vec![
                    sig.clone(),
                    lir::Type::AnyGc
                ]);

                Lowered::Some(lir::Type::Tuple(vec![
                    sig,
                    lir::Type::AnyGc
                ]))
            }
            mir::Expr::CoerceFromNever(expr) => {
                match self.lower_expr(builder, expr, mir) {
                    Lowered::Unit => panic!(),
                    Lowered::Empty => return Lowered::Empty,
                    Lowered::Some(_) => panic!()
                }
            }
            mir::Expr::SignExtend(expr, _) => {
                match self.lower_expr(builder, expr, mir) {
                    Lowered::Unit => panic!(),
                    Lowered::Empty => return Lowered::Empty,
                    Lowered::Some(_) => todo!()
                }
            }
        }
    }

    #[must_use]
    fn lower_type(&self, mir: &mir::MIR, ty: &mir::Type) -> Lowered<lir::Type> {
        self.lowering.lower_type(mir, ty)
    }
}