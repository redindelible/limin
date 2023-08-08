use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use indexmap::{IndexMap, IndexSet};
use slotmap::{SecondaryMap, SlotMap};
use crate::lowering::{hir, mir};
use crate::util::map_join;

pub fn monomorphize(hir: hir::HIR) -> mir::MIR {
    let lowerer = Monomorphize::new();
    lowerer.monomorphize(hir)
}

enum QueuedResolve {
    Function {
        hir_key: hir::FunctionKey,
        mir_key: mir::FunctionKey,
        map: HashMap<u64, mir::Type>
    },
    Struct {
        hir_key: hir::StructKey,
        mir_key: mir::StructKey,
        map: HashMap<u64, mir::Type>
    }
}

struct Monomorphize {
    blocks: SlotMap<mir::BlockKey, mir::Block>,
    locals: SlotMap<mir::LocalKey, mir::LocalInfo>,
    struct_prototypes: SlotMap<mir::StructKey, mir::StructPrototype>,
    struct_bodies: SecondaryMap<mir::StructKey, mir::StructBody>,
    function_prototypes: SlotMap<mir::FunctionKey, mir::FunctionPrototype>,
    function_bodies: SecondaryMap<mir::FunctionKey, mir::FunctionBody>,

    structs: SecondaryMap<hir::StructKey, StructInfo>,
    functions: SecondaryMap<hir::FunctionKey, FunctionInfo>,

    locals_map: HashMap<hir::NameKey, mir::LocalKey>,

    resolve_queue: RefCell<VecDeque<QueuedResolve>>,
    current_level: usize,
    used_blocks: Vec<IndexSet<mir::BlockKey>>
}

struct FunctionInfo {
    is_generic: bool,
    variants: HashMap<Vec<mir::Type>, mir::FunctionKey>
}

struct StructInfo {
    variants: HashMap<Vec<mir::Type>, mir::StructKey>
}

impl Monomorphize {
    fn new() -> Monomorphize {
        Monomorphize {
            blocks: SlotMap::with_key(),
            locals: SlotMap::with_key(),
            struct_prototypes: SlotMap::with_key(),
            struct_bodies: SecondaryMap::new(),
            function_prototypes: SlotMap::with_key(),
            function_bodies: SecondaryMap::new(),
            functions: SecondaryMap::new(),
            structs: SecondaryMap::new(),
            resolve_queue: RefCell::new(VecDeque::new()),
            locals_map: HashMap::new(),
            current_level: 0,
            used_blocks: Vec::new()
        }
    }

    fn monomorphize(mut self, hir: hir::HIR) -> mir::MIR {
        for (func_key, func_proto) in &hir.function_prototypes {
            if func_proto.type_params.is_empty() {
                self.queue_function(func_key, &[], &hir);
            }
        }

        while !self.resolve_queue.borrow().is_empty() {
            let next = self.resolve_queue.borrow_mut().pop_front().unwrap();
            match next {
                QueuedResolve::Function { hir_key, mir_key, map } => {
                    self.resolve_function(hir_key, mir_key, map, &hir);
                }
                QueuedResolve::Struct { hir_key, mir_key, map } => {
                    self.resolve_struct(hir_key, mir_key, map, &hir);
                }
            }
        }

        mir::MIR {
            main_fn: self.functions[hir.main_function.unwrap()].variants[&vec![]],
            struct_prototypes: self.struct_prototypes,
            struct_bodies: self.struct_bodies,
            function_prototypes: self.function_prototypes,
            function_bodies: self.function_bodies,
            locals: self.locals,
            blocks: self.blocks,
        }
    }

    fn resolve_struct(&mut self, hir_struct: hir::StructKey, mir_struct: mir::StructKey, generic_map: HashMap<u64, mir::Type>, hir: &hir::HIR) {
        let struct_ = &hir.structs[hir_struct];

        let mut fields = IndexMap::new();
        for field in struct_.fields.values() {
            let typ = self.lower_type(&field.typ, &generic_map, hir);
            fields.insert(field.name.clone(), typ);
        }

        self.struct_bodies.insert(mir_struct, mir::StructBody { fields });
    }

    fn resolve_function(&mut self, hir_func: hir::FunctionKey, mir_func: mir::FunctionKey, generic_map: HashMap<u64, mir::Type>, hir: &hir::HIR) {
        self.current_level = 0;
        self.used_blocks = vec![IndexSet::new()];
        let func = &hir.function_bodies[hir_func];

        let body = self.lower_block(&func.body, &generic_map, hir);

        let mut params = Vec::new();
        for param in &hir.function_prototypes[hir_func].params {
            params.push(self.locals_map[&param.decl]);
        }

        self.function_bodies.insert(mir_func, mir::FunctionBody { params, body });
    }

    fn lower_block(&mut self, block: &hir::Block, map: &HashMap<u64, mir::Type>, hir: &hir::HIR) -> mir::BlockKey {
        let hir::Block { stmts, trailing_expr, declared, .. } = block;
        let block_key = self.blocks.insert(mir::Block { stmts: vec![], ret: Box::new(mir::Expr::Unit), ret_type: mir::Type::Unit, locals: vec![], level: self.current_level });

        let mut locals = vec![];
        for (local_name, local_name_key) in declared {
            let local_info = mir::LocalInfo {
                name: local_name.clone(),
                typ: self.lower_type(&hir.type_of_name(*local_name_key), map, hir),
                block: block_key,
                is_closed: false
            };
            let key = self.locals.insert(local_info);
            self.locals_map.insert(*local_name_key, key);
            locals.push(key);
        }

        let stmts: Vec<_> = stmts.iter().map(|stmt|
            self.lower_stmt(stmt, map, hir)
        ).collect();

        let (ret, ret_type) = if let Some(expr) = trailing_expr {
            (Box::new(self.lower_expr(expr, map, hir)), self.lower_type(&hir.type_of_expr(expr), map, hir))
        } else {
            (Box::new(mir::Expr::Never), mir::Type::Never)
        };

        let block = mir::Block { stmts, ret, locals, ret_type, level: self.current_level };
        self.blocks[block_key] = block;

        block_key
    }

    fn lower_expr(&mut self, expr: &hir::Expr, map: &HashMap<u64, mir::Type>, hir: &hir::HIR) -> mir::Expr {
        match expr {
            hir::Expr::Bool { value, .. } => mir::Expr::Boolean(*value),
            hir::Expr::Integer { num, .. } => mir::Expr::Integer(*num),
            hir::Expr::Unit { .. } => mir::Expr::Unit,
            hir::Expr::Block(block) => mir::Expr::Block(self.lower_block(block, map, hir)),
            hir::Expr::Name { decl, .. } => {
                match &hir.names[*decl] {
                    hir::NameInfo::Local { level, .. } => {
                        if *level < self.current_level {
                            self.locals[self.locals_map[decl]].is_closed = true;
                        }
                        let block = self.locals[self.locals_map[decl]].block;
                        self.used_blocks.last_mut().unwrap().insert(block);
                        mir::Expr::LoadLocal(self.locals_map[decl])
                    }
                    hir::NameInfo::Function { func } => {
                        assert!(!self.functions[*func].is_generic);
                        mir::Expr::LoadFunction(self.functions[*func].variants[&vec![]])
                    }
                }
            },
            hir::Expr::GetAttr { obj, attr, .. } => {
                let struct_ty = self.lower_type(&hir.type_of_expr(obj), map, hir);
                let mir::Type::Struct(struct_) = struct_ty else { panic!() };
                mir::Expr::GetAttr(struct_, Box::new(self.lower_expr(obj, map, hir)), attr.clone())
            },
            hir::Expr::Call { callee, arguments, .. } => {
                mir::Expr::Call(
                    Box::new(self.lower_expr(callee, map, hir)),
                    arguments.iter().map(|arg| self.lower_expr(arg, map, hir)).collect()
                )
            },
            hir::Expr::New { struct_, variant, fields: hir_fields, .. } => {
                let variant: Vec<_> = variant.iter().map(|t| self.lower_type(t, map, hir)).collect();
                let key = self.queue_struct(*struct_, &variant, hir);

                let mut fields = IndexMap::new();
                for (field_name, expr) in hir_fields {
                    fields.insert(field_name.clone(), self.lower_expr(expr, map, hir));
                }

                mir::Expr::New(key, fields)
            }
            hir::Expr::GenericCall { callee, generic, arguments, .. } => {
                let generic: Vec<_> = generic.iter().map(|t| self.lower_type(t, map, hir)).collect();
                let callee = mir::Expr::LoadFunction(self.queue_function(*callee, &generic, hir));
                mir::Expr::Call(Box::new(callee), arguments.iter().map(|arg| self.lower_expr(arg, map, hir)).collect())
            },
            hir::Expr::IfElse { cond, then_do, else_do, yield_type, .. } => {
                mir::Expr::IfElse {
                    cond: Box::new(self.lower_expr(cond, map, hir)),
                    then_do: Box::new(self.lower_expr(then_do, map, hir)),
                    else_do: Box::new(self.lower_expr(else_do, map, hir)),
                    yield_type: self.lower_type(yield_type, map, hir)
                }
            }
            hir::Expr::Closure { parameters, ret_type, body, .. } => {
                self.current_level += 1;
                self.used_blocks.push(IndexSet::new());

                let body = self.lower_block(body, map, hir);

                let used = self.used_blocks.pop().unwrap();
                self.current_level -= 1;
                self.used_blocks.last_mut().unwrap().extend(&used);

                let parameters: Vec<mir::ClosureParameter> = parameters.iter().map(|p| {
                    let typ = self.lower_type(&p.typ, map, hir);
                    mir::ClosureParameter { name: p.name.clone(), typ, key: self.locals_map[&p.key]}
                }).collect();

                let ret_type = self.lower_type(ret_type, map, hir);

                let closed_blocks: Vec<mir::BlockKey> = used.into_iter().filter(|block| {
                    self.blocks[*block].level <= self.current_level
                }).collect();

                mir::Expr::Closure { parameters, ret_type, body, closed_blocks }
            }
            // hir::Expr::LogicBinOp { .. } => todo!(),
            hir::Expr::Errored { .. } => panic!("Should not be able to reach here if there is an error"),
        }
    }

    fn lower_stmt(&mut self, stmt: &hir::Stmt, map: &HashMap<u64, mir::Type>, hir: &hir::HIR) -> mir::Stmt {
        match stmt {
            hir::Stmt::Expr { expr, .. } => {
                mir::Stmt::Expr(Box::new(self.lower_expr(expr, map, hir)))
            }
            hir::Stmt::Return { value, .. } => {
                mir::Stmt::Ret(Box::new(self.lower_expr(value, map, hir)))
            }
            hir::Stmt::Decl { decl, value, .. } => {
                let local = self.locals_map[decl];
                let expr = self.lower_expr(value, map, hir);
                let ty = self.locals[local].typ.clone();
                mir::Stmt::Decl(local, ty, Box::new(expr))
            }
        }
    }

    fn queue_struct(&mut self, struct_: hir::StructKey, variant: &[mir::Type], hir: &hir::HIR) -> mir::StructKey {
        let hir_struct = &hir.structs[struct_];

        if !self.structs.contains_key(struct_) {
            self.structs.insert(struct_, StructInfo { variants: HashMap::new() });
        }

        if let Some(key) = self.structs[struct_].variants.get(variant) {
            return *key;
        }

        let name = if variant.is_empty() {
            format!("{}", &hir_struct.name)
        } else {
            format!("{}<{}>", &hir_struct.name, map_join(variant, |t| self.name_of(t)))
        };

        let mut generic_map = HashMap::new();
        for (type_param, typ) in hir_struct.type_params.iter().zip(variant.iter()) {
            generic_map.insert(type_param.id, typ.clone());
        }

        let key = self.struct_prototypes.insert(mir::StructPrototype { name });
        self.structs[struct_].variants.insert(variant.to_vec(), key);
        self.resolve_queue.borrow_mut().push_back(QueuedResolve::Struct { hir_key: struct_, mir_key: key, map: generic_map });
        key
    }

    fn queue_function(&mut self, func: hir::FunctionKey, variant: &[mir::Type], hir: &hir::HIR) -> mir::FunctionKey {
        let proto = &hir.function_prototypes[func];

        if !self.functions.contains_key(func) {
            self.functions.insert(func, FunctionInfo {
                is_generic: !proto.type_params.is_empty(),
                variants: HashMap::new()
            });
        }

        if let Some(key) = self.functions[func].variants.get(variant) {
            return *key;
        }

        let name = if variant.is_empty() {
            format!("{}", &proto.name)
        } else {
            format!(
                "{}<{}>",
                &proto.name,
                map_join(variant, |t| self.name_of(t))
            )
        };

        let mut generic_map = HashMap::new();
        for (type_param, typ) in proto.type_params.iter().zip(variant.iter()) {
            generic_map.insert(type_param.id, typ.clone());
        }

        let mut params = Vec::new();
        for param in proto.params.clone() {
            let name = param.name;
            let typ = self.lower_type(&param.typ, &generic_map, hir);
            params.push((name, typ))
        }

        let ret = self.lower_type(&proto.ret, &generic_map, hir);

        let key = self.function_prototypes.insert(mir::FunctionPrototype {
            name,
            params,
            ret
        });
        self.functions[func].variants.insert(variant.to_vec(), key);
        self.resolve_queue.borrow_mut().push_back(QueuedResolve::Function { hir_key: func, mir_key: key, map: generic_map });
        key
    }

    fn lower_type(&mut self, typ: &hir::Type, map: &HashMap<u64, mir::Type>, hir: &hir::HIR) -> mir::Type {
        match typ {
            hir::Type::Never => mir::Type::Never,
            hir::Type::Unit => mir::Type::Unit,
            hir::Type::Boolean => mir::Type::Boolean,
            hir::Type::Integer { bits } => mir::Type::Integer(*bits),
            hir::Type::TypeParameter { id, .. } => {
                map[id].clone()
            },
            hir::Type::Struct { struct_, variant } => {
                let variant = variant.iter().map(|t| self.lower_type(t, map, hir)).collect::<Vec<_>>();
                mir::Type::Struct(self.queue_struct(*struct_, &variant, hir))
            },
            hir::Type::Function { params, ret } => {
                let params: Vec<_> = params.iter().map(|t| self.lower_type(t, map, hir)).collect();
                mir::Type::Function(params, Box::new(self.lower_type(ret, map, hir)))
            },
            hir::Type::GenericFunction { .. } => unreachable!(),
            hir::Type::TypeParameterInstance { .. } => unreachable!(),
            hir::Type::Errored => panic!("Should not be able to reach here if there is an error"),
        }
    }

    fn name_of(&self, ty: &mir::Type) -> String {
        match ty {
            mir::Type::Never => "!".into(),
            mir::Type::Unit => "()".into(),
            mir::Type::Boolean => "bool".into(),
            mir::Type::Integer(bits) => format!("i{bits}"),
            mir::Type::Struct(struct_) => self.struct_prototypes[*struct_].name.clone(),
            mir::Type::Function(params, ret) => format!(
                "({}) -> {}",
                map_join(params, |p| self.name_of(p)),
                self.name_of(ret)
            )
        }
    }
}