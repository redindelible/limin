use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use slotmap::{SecondaryMap, SlotMap};
use crate::{hir, lir};
use crate::common::map_join;

pub fn lower(hir: hir::HIR) -> lir::LIR {
    let lowerer = Lower::new();
    lowerer.lower(hir)
}

struct Lower {
    blocks: SlotMap<lir::BlockKey, lir::Block>,
    locals: SlotMap<lir::LocalKey, lir::LocalInfo>,
    struct_prototypes: SlotMap<lir::StructKey, lir::StructPrototype>,
    function_prototypes: SlotMap<lir::FunctionKey, lir::FunctionPrototype>,
    function_bodies: SecondaryMap<lir::FunctionKey, lir::FunctionBody>,

    functions: SecondaryMap<hir::FunctionKey, FunctionInfo>,

    resolve_queue: RefCell<VecDeque<(hir::FunctionKey, lir::FunctionKey, HashMap<u64, lir::Type>)>>,

    locals_map: HashMap<hir::NameKey, lir::LocalKey>
}

struct FunctionInfo {
    is_generic: bool,
    variants: HashMap<Vec<lir::Type>, lir::FunctionKey>
}

impl Lower {
    fn new() -> Lower {
        Lower {
            blocks: SlotMap::with_key(),
            locals: SlotMap::with_key(),
            struct_prototypes: SlotMap::with_key(),
            function_prototypes: SlotMap::with_key(),
            function_bodies: SecondaryMap::new(),
            functions: SecondaryMap::new(),
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
            let (hir_func, lir_func, generic) = self.resolve_queue.borrow_mut().pop_front().unwrap();
            self.resolve_function(hir_func, lir_func, generic, &hir);
        }

        lir::LIR {
            main_fn: self.functions[hir.main_function.unwrap()].variants[&vec![]],
            struct_prototypes: self.struct_prototypes,
            struct_bodies: SecondaryMap::new(),
            function_prototypes: self.function_prototypes,
            function_bodies: self.function_bodies,
            locals: self.locals,
            blocks: self.blocks
        }
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
                typ: self.lower_type(&hir.type_of_name(*local_name_key), map),
                block: block_key
            };
            let key = self.locals.insert(local_info);
            self.locals_map.insert(*local_name_key, key);
            locals.push(key);
        }

        let stmts: Vec<_> = stmts.iter().map(|stmt|
            self.resolve_stmt(stmt, map, hir)
        ).collect();

        let (ret, ret_type) = if let Some(expr) = trailing_expr {
            (Box::new(self.resolve_expr(expr, map, hir)), self.lower_type(&hir.type_of_expr(expr), map))
        } else {
            (Box::new(lir::Expr::Never), lir::Type::Never)
        };

        let block = lir::Block { stmts, ret, locals, ret_type };
        self.blocks[block_key] = block;

        block_key
    }

    fn resolve_expr(&mut self, expr: &hir::Expr, map: &HashMap<u64, lir::Type>, hir: &hir::HIR) -> lir::Expr {
        match expr {
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
            hir::Expr::Call { callee, arguments, .. } => {
                lir::Expr::Call(
                    Box::new(self.resolve_expr(callee, map, hir)),
                    arguments.iter().map(|arg| self.resolve_expr(arg, map, hir)).collect()
                )
            },
            hir::Expr::GenericCall { callee, generic, arguments, .. } => {
                let generic: Vec<_> = generic.iter().map(|t| self.lower_type(t, map)).collect();
                let callee = lir::Expr::LoadFunction(self.queue_function(*callee, &generic, hir));
                lir::Expr::Call(Box::new(callee), arguments.iter().map(|arg| self.resolve_expr(arg, map, hir)).collect())
            },
            _ => panic!("{:?}", expr)
        }
    }

    fn resolve_stmt(&mut self, stmt: &hir::Stmt, map: &HashMap<u64, lir::Type>, hir: &hir::HIR) -> lir::Stmt {
        match stmt {
            hir::Stmt::Expr { expr, .. } => {
                lir::Stmt::Expr(Box::new(self.resolve_expr(expr, map, hir)))
            }
            hir::Stmt::Return { value, .. } => {
                lir::Stmt::Ret(Box::new(self.resolve_expr(value, map, hir)))
            }
            hir::Stmt::Decl { decl, value, .. } => {
                lir::Stmt::Decl(self.locals_map[decl], Box::new(self.resolve_expr(value, map, hir)))
            }
        }
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
            format!("{}", &hir.function_prototypes[func].name)
        } else {
            format!(
                "{}<{}>",
                &hir.function_prototypes[func].name,
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
            let typ = self.lower_type(&param.typ, &generic_map);
            params.push((name, typ))
        }

        let ret = self.lower_type(&proto.ret, &generic_map);

        let key = self.function_prototypes.insert(lir::FunctionPrototype {
            name,
            params,
            ret
        });
        self.functions[func].variants.insert(variant.to_vec(), key);
        self.resolve_queue.borrow_mut().push_back((func, key, generic_map));
        key
    }

    fn lower_type(&self, typ: &hir::Type, map: &HashMap<u64, lir::Type>) -> lir::Type {
        match typ {
            hir::Type::Unit => lir::Type::Unit,
            hir::Type::Boolean => lir::Type::Boolean,
            hir::Type::Integer { bits } => lir::Type::Integer(*bits),
            hir::Type::TypeParameter { id, .. } => map[id].clone(),
            _ => panic!("{:?}", typ)
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