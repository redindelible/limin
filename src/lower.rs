use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use indexmap::IndexMap;
use slotmap::{SecondaryMap, SlotMap};
use crate::{hir, lir};
use crate::common::map_join;

pub fn lower(hir: hir::HIR) -> lir::LIR {
    let lowerer = Lower::new();
    lowerer.lower(hir)
}

enum QueuedResolve {
    Function {
        hir_key: hir::FunctionKey,
        lir_key: lir::FunctionKey,
        map: HashMap<u64, lir::Type>
    },
    Struct {
        hir_key: hir::StructKey,
        lir_key: lir::StructKey,
        map: HashMap<u64, lir::Type>
    }
}

struct Lower {
    blocks: SlotMap<lir::BlockKey, lir::Block>,
    locals: SlotMap<lir::LocalKey, lir::LocalInfo>,
    struct_prototypes: SlotMap<lir::StructKey, lir::StructPrototype>,
    struct_bodies: SecondaryMap<lir::StructKey, lir::StructBody>,
    function_prototypes: SlotMap<lir::FunctionKey, lir::FunctionPrototype>,
    function_bodies: SecondaryMap<lir::FunctionKey, lir::FunctionBody>,

    structs: SecondaryMap<hir::StructKey, StructInfo>,
    functions: SecondaryMap<hir::FunctionKey, FunctionInfo>,

    resolve_queue: RefCell<VecDeque<QueuedResolve>>,

    locals_map: HashMap<hir::NameKey, lir::LocalKey>
}

struct FunctionInfo {
    is_generic: bool,
    variants: HashMap<Vec<lir::Type>, lir::FunctionKey>
}

struct StructInfo {
    variants: HashMap<Vec<lir::Type>, lir::StructKey>
}

impl Lower {
    fn new() -> Lower {
        Lower {
            blocks: SlotMap::with_key(),
            locals: SlotMap::with_key(),
            struct_prototypes: SlotMap::with_key(),
            struct_bodies: SecondaryMap::new(),
            function_prototypes: SlotMap::with_key(),
            function_bodies: SecondaryMap::new(),
            functions: SecondaryMap::new(),
            structs: SecondaryMap::new(),
            resolve_queue: RefCell::new(VecDeque::new()),
            locals_map: HashMap::new()
        }
    }

    fn lower(mut self, hir: hir::HIR) -> lir::LIR {
        for (func_key, func_proto) in &hir.function_prototypes {
            if func_proto.type_params.is_empty() {
                self.queue_function(func_key, &[], &hir);
            }
        }

        while !self.resolve_queue.borrow().is_empty() {
            let next = self.resolve_queue.borrow_mut().pop_front().unwrap();
            match next {
                QueuedResolve::Function { hir_key, lir_key, map } => {
                    self.resolve_function(hir_key, lir_key, map, &hir);
                }
                QueuedResolve::Struct { hir_key, lir_key, map } => {
                    self.resolve_struct(hir_key, lir_key, map, &hir);
                }
            }
        }

        lir::LIR {
            main_fn: self.functions[hir.main_function.unwrap()].variants[&vec![]],
            struct_prototypes: self.struct_prototypes,
            struct_bodies: self.struct_bodies,
            function_prototypes: self.function_prototypes,
            function_bodies: self.function_bodies,
            locals: self.locals,
            blocks: self.blocks
        }
    }

    fn resolve_struct(&mut self, hir_struct: hir::StructKey, lir_struct: lir::StructKey, generic_map: HashMap<u64, lir::Type>, hir: &hir::HIR) {
        let struct_ = &hir.structs[hir_struct];

        let mut fields = IndexMap::new();
        for field in struct_.fields.values() {
            let typ = self.lower_type(&field.typ, &generic_map, hir);
            fields.insert(field.name.clone(), typ);
        }

        self.struct_bodies.insert(lir_struct, lir::StructBody { fields });
    }

    fn resolve_function(&mut self, hir_func: hir::FunctionKey, lir_func: lir::FunctionKey, generic_map: HashMap<u64, lir::Type>, hir: &hir::HIR) {
        let func = &hir.function_bodies[hir_func];

        let body = self.lower_block(&func.body, &generic_map, hir);

        for (i, param) in hir.function_prototypes[hir_func].params.iter().enumerate() {
            let stmt = lir::Stmt::Decl(self.locals_map[&param.decl], Box::new(lir::Expr::Parameter(lir_func, i)));
            self.blocks[body].stmts.insert(0, stmt);
        }

        self.function_bodies.insert(lir_func, lir::FunctionBody { body });
    }

    fn lower_block(&mut self, block: &hir::Block, map: &HashMap<u64, lir::Type>, hir: &hir::HIR) -> lir::BlockKey {
        let hir::Block { stmts, trailing_expr, declared, .. } = block;
        let block_key = self.blocks.insert(lir::Block { stmts: vec![], ret: Box::new(lir::Expr::Unit), ret_type: lir::Type::Unit, locals: vec![]});

        let mut locals = vec![];
        for (local_name, local_name_key) in declared {
            let local_info = lir::LocalInfo {
                name: local_name.clone(),
                typ: self.lower_type(&hir.type_of_name(*local_name_key), map, hir),
                block: block_key
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
            (Box::new(lir::Expr::Never), lir::Type::Never)
        };

        let block = lir::Block { stmts, ret, locals, ret_type };
        self.blocks[block_key] = block;

        block_key
    }

    fn lower_expr(&mut self, expr: &hir::Expr, map: &HashMap<u64, lir::Type>, hir: &hir::HIR) -> lir::Expr {
        match expr {
            hir::Expr::Bool { value, .. } => lir::Expr::Boolean(*value),
            hir::Expr::Integer { num, .. } => lir::Expr::Integer(*num),
            hir::Expr::Unit { .. } => lir::Expr::Unit,
            hir::Expr::Block(block) => lir::Expr::Block(self.lower_block(block, map, hir)),
            hir::Expr::Name { decl, .. } => {
                match &hir.names[*decl] {
                    hir::NameInfo::Local { .. } => {
                        lir::Expr::LoadLocal(self.locals_map[decl])
                    }
                    hir::NameInfo::Function { func } => {
                        assert!(!self.functions[*func].is_generic);
                        lir::Expr::LoadFunction(self.functions[*func].variants[&vec![]])
                    }
                }
            },
            hir::Expr::GetAttr { obj, attr, .. } => {
                let struct_ty = self.lower_type(&hir.type_of_expr(obj), map, hir);
                let lir::Type::Struct(struct_) = struct_ty else { panic!() };
                lir::Expr::GetAttr(struct_, Box::new(self.lower_expr(obj, map, hir)), attr.clone())
            },
            hir::Expr::Call { callee, arguments, .. } => {
                lir::Expr::Call(
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

                lir::Expr::New(key, fields)
            }
            hir::Expr::GenericCall { callee, generic, arguments, .. } => {
                let generic: Vec<_> = generic.iter().map(|t| self.lower_type(t, map, hir)).collect();
                let callee = lir::Expr::LoadFunction(self.queue_function(*callee, &generic, hir));
                lir::Expr::Call(Box::new(callee), arguments.iter().map(|arg| self.lower_expr(arg, map, hir)).collect())
            },
            // hir::Expr::LogicBinOp { .. } => todo!(),
            hir::Expr::Errored { .. } => panic!("Should not be able to reach here if there is an error"),
        }
    }

    fn lower_stmt(&mut self, stmt: &hir::Stmt, map: &HashMap<u64, lir::Type>, hir: &hir::HIR) -> lir::Stmt {
        match stmt {
            hir::Stmt::Expr { expr, .. } => {
                lir::Stmt::Expr(Box::new(self.lower_expr(expr, map, hir)))
            }
            hir::Stmt::Return { value, .. } => {
                lir::Stmt::Ret(Box::new(self.lower_expr(value, map, hir)))
            }
            hir::Stmt::Decl { decl, value, .. } => {
                lir::Stmt::Decl(self.locals_map[decl], Box::new(self.lower_expr(value, map, hir)))
            }
        }
    }

    fn queue_struct(&mut self, struct_: hir::StructKey, variant: &[lir::Type], hir: &hir::HIR) -> lir::StructKey {
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

        let key = self.struct_prototypes.insert(lir::StructPrototype { name });
        self.structs[struct_].variants.insert(variant.to_vec(), key);
        self.resolve_queue.borrow_mut().push_back(QueuedResolve::Struct { hir_key: struct_, lir_key: key, map: generic_map });
        key
    }

    fn queue_function(&mut self, func: hir::FunctionKey, variant: &[lir::Type], hir: &hir::HIR) -> lir::FunctionKey {
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

        let key = self.function_prototypes.insert(lir::FunctionPrototype {
            name,
            params,
            ret
        });
        self.functions[func].variants.insert(variant.to_vec(), key);
        self.resolve_queue.borrow_mut().push_back(QueuedResolve::Function { hir_key: func, lir_key: key, map: generic_map });
        key
    }

    fn lower_type(&mut self, typ: &hir::Type, map: &HashMap<u64, lir::Type>, hir: &hir::HIR) -> lir::Type {
        match typ {
            hir::Type::Never => lir::Type::Never,
            hir::Type::Unit => lir::Type::Unit,
            hir::Type::Boolean => lir::Type::Boolean,
            hir::Type::Integer { bits } => lir::Type::Integer(*bits),
            hir::Type::TypeParameter { id, .. } => {
                map[id].clone()
            },
            hir::Type::Struct { struct_, variant } => {
                let variant = variant.iter().map(|t| self.lower_type(t, map, hir)).collect::<Vec<_>>();
                lir::Type::Struct(self.queue_struct(*struct_, &variant, hir))
            },
            hir::Type::Function { params, ret } => {
                let params: Vec<_> = params.iter().map(|t| self.lower_type(t, map, hir)).collect();
                lir::Type::Function(params, Box::new(self.lower_type(ret, map, hir)))
            },
            hir::Type::GenericFunction { .. } => unreachable!(),
            hir::Type::TypeParameterInstance { .. } => unreachable!(),
            hir::Type::Errored => panic!("Should not be able to reach here if there is an error"),
        }
    }

    fn name_of(&self, ty: &lir::Type) -> String {
        match ty {
            lir::Type::Never => "!".into(),
            lir::Type::Unit => "()".into(),
            lir::Type::Boolean => "bool".into(),
            lir::Type::Integer(bits) => format!("i{bits}"),
            lir::Type::Struct(struct_) => self.struct_prototypes[*struct_].name.clone(),
            lir::Type::Function(params, ret) => format!(
                "({}) -> {}",
                map_join(params, |p| self.name_of(p)),
                self.name_of(ret)
            )
        }
    }
}