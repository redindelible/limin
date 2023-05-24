use std::collections::{HashMap, HashSet};
use std::ops::RangeFrom;
use std::path::PathBuf;
use indexmap::IndexMap;
use slotmap::{SecondaryMap, SlotMap};
use crate::ast;
use crate::common::map_join;
use crate::error::Message;
use crate::hir::{HIR, NameKey, NameInfo, Struct, StructKey, Type, StructField, FunctionKey, Parameter, FunctionPrototype, FunctionBody, Expr, LogicOp, Stmt, MayBreak, TypeParameter, TypeParamKey, Block};
use crate::source::{HasLoc, Location};
use crate::type_check::type_check_state::NamespaceKey;

pub fn resolve_types(ast: ast::AST) -> Result<HIR, Vec<TypeCheckError>> {
    collect_function_bodies(collect_functions(collect_fields(collect_structs(initialize(ast)))))
}

#[derive(Debug, Eq, PartialEq)]
pub enum DisplayType<'a> {
    Unit,
    Never,
    Boolean,
    Errored,
    Integer { bits: u8 },
    Struct { name: String, variant: Vec<DisplayType<'a>>, loc: Location<'a> },
    Function { params: Vec<DisplayType<'a>>, ret: Box<DisplayType<'a>> },
    GenericFunction { type_params: Vec<DisplayType<'a>>, params: Vec<DisplayType<'a>>, ret: Box<DisplayType<'a>> },
    TypeParameter { name: String, bound: Option<Box<DisplayType<'a>>> },
    TypeParamInstance { name: String }
}

impl DisplayType<'_> {
    fn render(&self) -> String {
        match self {
            DisplayType::Unit => "()".into(),
            DisplayType::Never => "!".into(),
            DisplayType::Boolean => "bool".into(),
            DisplayType::Errored => "<could not resolve>".into(),
            DisplayType::Integer { bits } => format!("i{bits}"),
            DisplayType::Struct { name, variant, .. } => {
                if variant.is_empty() {
                    format!("{name}")
                } else {
                    format!("{name}<{}>", map_join(variant, Self::render))
                }
            },
            DisplayType::Function { params, ret } => {
                let rendered: Vec<String> = params.iter().map(|t| t.render()).collect();
                format!("({}) -> {}", rendered.join(", "), ret.render())
            },
            DisplayType::TypeParameter { name, bound } => {
                if let Some(bound) = bound {
                    format!("{name}: {}", bound.render())
                } else {
                    format!("{name}")
                }
            },
            DisplayType::GenericFunction { type_params, params, ret } => {
                let rendered_type_params: Vec<String> = type_params.iter().map(|t| t.render()).collect();
                let rendered_params: Vec<String> = params.iter().map(|t| t.render()).collect();
                format!("<{}>({}) -> {}", rendered_type_params.join(", "), rendered_params.join(", "), ret.render())
            }
            DisplayType::TypeParamInstance { name } => format!("{name}"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypeCheckError<'a> {
    NameDuplicated(String, Location<'a>),
    CouldNotResolveName(String, Location<'a>),
    CouldNotResolveType(String, Location<'a>),
    IncompatibleTypes { expected: DisplayType<'a>, got: DisplayType<'a>, loc: Location<'a> },
    ExpectedFunction { got: DisplayType<'a>, loc: Location<'a> },
    MismatchedArguments { expected: usize, got: usize, loc: Location<'a> },
    MismatchedTypeArguments { expected: usize, got: usize, loc: Location<'a> },
    RequiredTypeArguments { got: String, loc: Location<'a>, note_loc: Location<'a> },
    NotEnoughInfoToInfer(Location<'a>),
    ExpectedStructName { got: String, loc: Location<'a> },
    ExpectedStruct { got: DisplayType<'a>, loc: Location<'a> },
    NoSuchFieldName { field: String, typ: String, loc: Location<'a> },
    MissingFields { fields: Vec<String>, typ: String, loc: Location<'a> },
    CouldNotInferParameter(String, Location<'a>),
    NoMainFunction,
}

impl<'a> Message for TypeCheckError<'a> {
    fn render(&self) {
        match self {
            TypeCheckError::NameDuplicated(name, loc) => {
                eprintln!("Error: The name '{}' is already used and cannot be redefined.", name);
                Self::show_location(loc);
            }
            TypeCheckError::CouldNotResolveName(name, loc) => {
                eprintln!("Error: Could not resolve the name '{}'.", name);
                Self::show_location(loc);
            }
            TypeCheckError::CouldNotResolveType(name, loc) => {
                eprintln!("Error: Could not resolve the type '{}'.", name);
                Self::show_location(loc);
            }
            TypeCheckError::IncompatibleTypes { expected, got, loc } => {
                eprintln!("Error: Incompatible types. Expected '{}' but got '{}'.", expected.render(), got.render());
                Self::show_location(loc);
            }
            TypeCheckError::ExpectedFunction { got, loc } => {
                eprintln!("Error: Expected a function to call, but got '{}'.", got.render());
                Self::show_location(loc);
            }
            TypeCheckError::MismatchedArguments { expected, got, loc } => {
                eprintln!("Error: Expected {expected} arguments, got {got} arguments.");
                Self::show_location(loc);
            }
            TypeCheckError::MismatchedTypeArguments { expected, got, loc } => {
                eprintln!("Error: Expected {expected} type parameters, got {got} type parameters.");
                Self::show_location(loc);
            }
            TypeCheckError::RequiredTypeArguments { got, loc, note_loc } => {
                eprintln!("Error: Expected type parameters to be supplied for '{got}'.");
                Self::show_location(loc);
                eprintln!(" | Note: '{got}' defined here.");
                Self::show_note_location(note_loc);
            }
            TypeCheckError::NotEnoughInfoToInfer(loc) => {
                eprintln!("Error: Could not infer type.");
                Self::show_location(loc);
            }
            TypeCheckError::NoMainFunction => {
                eprintln!("Error: No main function could be found.");
            }
            TypeCheckError::ExpectedStructName { got, loc } => {
                eprintln!("Error: Could not find a struct named '{}'.", got);
                Self::show_location(loc);
            }
            TypeCheckError::ExpectedStruct { got, loc } => {
                eprintln!("Error: Expected a struct, but got '{}'.", got.render());
                Self::show_location(loc);
            }
            TypeCheckError::NoSuchFieldName { field, typ, loc } => {
                eprintln!("Error: '{}' Does not contain a field named '{}'.", typ, field);
                Self::show_location(loc);
            }
            TypeCheckError::MissingFields { fields, typ, loc } => {
                let rendered_fields: Vec<_> = fields.iter().map(|f| format!("'{f}'")).collect();
                eprintln!("Error: Fields {} were not supplied to initialize '{}'.", rendered_fields.join(", "), typ);
                Self::show_location(loc);
            }
            TypeCheckError::CouldNotInferParameter(name, loc) => {
                eprintln!("Error: Could not infer the type of '{}'.", name);
                Self::show_location(loc);
            }
        }
        eprint!("\n");
    }
}

mod type_check_state {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use slotmap::{new_key_type, SlotMap};
    use crate::hir::{HIR, NameKey, NameInfo, Struct, StructKey, Type, FunctionKey, FunctionPrototype, Expr};
    pub use crate::type_check::TypeCheckError;

    new_key_type! {
        pub struct NamespaceKey;
    }

    pub struct Namespace {
        pub parent: Option<NamespaceKey>,
        pub names: HashMap<String, NameKey>,
        pub types: HashMap<String, Type>,
        pub structs: HashMap<String, StructKey>,
        pub namespaces: HashMap<String, NamespaceKey>,
        _private: (),
    }

    impl Namespace {
        fn new(parent: Option<NamespaceKey>) -> Namespace {
            Namespace {
                parent,
                names: HashMap::new(),
                types: HashMap::new(),
                structs: HashMap::new(),
                namespaces: HashMap::new(),
                _private: ()
            }
        }

        pub fn get_names(&self) -> HashMap<String, NameKey> {
            self.names.clone()
        }
    }

    pub struct TypeCheck<'a> {
        pub namespaces: SlotMap<NamespaceKey, Namespace>,
        pub errors: RefCell<Vec<TypeCheckError<'a>>>,
        pub hir: HIR<'a>,
        pub type_param_counter: u64
    }

    impl<'a> TypeCheck<'a> {
        pub fn new(name: String) -> TypeCheck<'a> {
            TypeCheck { namespaces: SlotMap::with_key(), hir: HIR::new(name), errors: Default::default(), type_param_counter: 0 }
        }

        pub fn add_type_param(&mut self) -> u64 {
            let count = self.type_param_counter;
            self.type_param_counter += 1;
            count
        }

        pub fn add_type(&mut self, ns: NamespaceKey, name: String, typ: Type) {
            self.namespaces[ns].types.insert(name, typ);
        }

        pub fn add_name(&mut self, ns: NamespaceKey, name: String, info: NameInfo<'a>) -> NameKey {
            let key = self.hir.names.insert(info);
            self.namespaces[ns].names.insert(name, key);
            key
        }

        pub fn add_namespace(&mut self, parent: Option<NamespaceKey>) -> NamespaceKey {
            self.namespaces.insert(Namespace::new(parent))
        }

        pub fn add_struct(&mut self, ns: NamespaceKey, struct_: Struct<'a>) -> StructKey {
            let name = struct_.name.clone();
            let key = self.hir.structs.insert(struct_);
            self.namespaces[ns].structs.insert(name, key);
            key
        }

        pub fn add_function_proto(&mut self, proto: FunctionPrototype<'a>) -> FunctionKey {
            self.hir.function_prototypes.insert(proto)
        }

        pub fn get_struct_key(&self, file: NamespaceKey, name: &str) -> StructKey {
            self.namespaces[file].structs[name]
        }

        pub fn get_function_key(&self, file: NamespaceKey, name: &str) -> FunctionKey {
            match self.hir.names[self.namespaces[file].names[name]] {
                NameInfo::Function { func } => func,
                _ => panic!("not a function")
            }
        }

        pub fn type_of_expr(&self, expr: &Expr<'a>) -> Type {
            self.hir.type_of_expr(expr)
        }

        pub fn push_error(&self, err: TypeCheckError<'a>) {
            self.errors.borrow_mut().push(err);
        }

        pub fn finalize(self) -> Result<HIR<'a>, Vec<TypeCheckError<'a>>> {
            if self.errors.borrow().is_empty() {
                Ok(self.hir)
            } else {
                Err(self.errors.into_inner())
            }
        }
    }
}

use self::type_check_state::TypeCheck;


struct Initial<'a> {
    checker: TypeCheck<'a>,

    root: NamespaceKey,
    files: HashMap<PathBuf, ast::File<'a>>
}

struct CollectedStructs<'a> {
    checker: TypeCheck<'a>,

    root: NamespaceKey,
    files: HashMap<PathBuf, ast::File<'a>>,
    file_namespaces: HashMap<PathBuf, NamespaceKey>,
}

struct CollectedFields<'a> {
    checker: TypeCheck<'a>,

    root: NamespaceKey,
    files: HashMap<PathBuf, ast::File<'a>>,
    file_namespaces: HashMap<PathBuf, NamespaceKey>,
    struct_namespaces: SecondaryMap<StructKey, NamespaceKey>,
}

struct CollectedFunctions<'a> {
    checker: TypeCheck<'a>,

    root: NamespaceKey,
    files: HashMap<PathBuf, ast::File<'a>>,
    file_namespaces: HashMap<PathBuf, NamespaceKey>,
    function_namespaces: SecondaryMap<FunctionKey, NamespaceKey>,
    struct_namespaces: SecondaryMap<StructKey, NamespaceKey>,
}

fn initialize(ast: ast::AST) -> Initial {
    let mut checker = TypeCheck::new(ast.name);
    let root = checker.add_namespace(None);
    checker.add_type(root, "i32".to_owned(), Type::Integer { bits: 32 });
    checker.add_type(root, "i64".to_owned(), Type::Integer { bits: 64 });
    checker.add_type(root, "bool".to_owned(), Type::Boolean);
    Initial { root, checker, files: ast.files }
}

fn collect_structs(initial: Initial) -> CollectedStructs {
    let Initial { mut checker, root, files } = initial;

    let mut file_namespaces = HashMap::new();
    
    for (file_path, file) in &files {
        let file_ns = checker.add_namespace(Some(root));

        file_namespaces.insert(file_path.clone(), file_ns);
        for ast::Struct { name, loc, .. } in file.iter_structs() {
            // todo ensure names unique
            checker.add_struct(file_ns, Struct { name: name.clone(), type_params: Vec::new(), fields: IndexMap::new(), loc: *loc });
        }
    }

    CollectedStructs { checker, file_namespaces, files, root }
}


fn resolve_type<'a>(checker: &TypeCheck<'a>, ns: NamespaceKey, typ: &ast::Type<'a>) -> Type {
    match typ {
        ast::Type::Name { name, loc } => {
            if let Some(t) = checker.namespaces[ns].types.get(name) {
                return t.clone();
            }
            if let Some(&t) = checker.namespaces[ns].structs.get(name) {
                if !checker.hir.structs[t].type_params.is_empty() {
                    checker.push_error(TypeCheckError::RequiredTypeArguments { got: checker.hir.structs[t].name.clone(), loc: *loc, note_loc: checker.hir.structs[t].loc });
                    return Type::Errored
                }
                return Type::Struct { struct_: t, variant: vec![] };
            }
            if let Some(parent) = checker.namespaces[ns].parent {
                return resolve_type(checker,parent, typ);
            }
            checker.push_error(TypeCheckError::CouldNotResolveType(name.clone(), *loc));
            Type::Errored
        }
        ast::Type::Function { parameters, ret, loc } => {
            let parameters: Vec<Type> = parameters.iter().map(|p| resolve_type(checker, ns, p)).collect();
            let ret = resolve_type(checker, ns, ret);
            Type::Function { params: parameters, ret: Box::new(ret) }
        }
        ast::Type::Generic { name, type_args, loc } => {
            if let Some(s) = resolve_struct(checker, ns, name) {
                let type_params = &checker.hir.structs[s].type_params;
                if type_params.len() != type_args.len() {
                    checker.push_error(TypeCheckError::MismatchedTypeArguments { expected: type_params.len(), got: type_args.len(), loc: *loc });
                    return Type::Errored;
                }
                let type_args: Vec<Type> = type_args.iter().map(|arg| resolve_type(checker, ns, arg)).collect();
                return Type::Struct { struct_: s, variant: type_args };
            }
            checker.push_error(TypeCheckError::CouldNotResolveType(name.clone(), *loc));
            Type::Errored
        }
    }
}

fn resolve_struct<'a>(checker: &TypeCheck<'a>, ns: NamespaceKey, name: &str) -> Option<StructKey> {
    if let Some(s) = checker.namespaces[ns].structs.get(name) {
        return Some(*s);
    };
    if let Some(parent) = checker.namespaces[ns].parent {
        return resolve_struct(checker,parent, name);
    }
    None
}

fn collect_fields(collected: CollectedStructs) -> CollectedFields {
    let CollectedStructs {
        mut checker, files, root,
        file_namespaces
    } = collected;

    let mut struct_namespaces = SecondaryMap::new();

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];
        for ast::Struct { name, type_params, items, .. } in file.iter_structs() {
            let key = checker.get_struct_key(file_ns, name);

            let struct_ns = checker.add_namespace(Some(file_ns));

            for type_param in type_params {
                let id = checker.add_type_param();
                let typ = Type::TypeParameter { name: type_param.name.clone(), id, bound: None };
                checker.add_type(struct_ns, type_param.name.clone(), typ);
                checker.hir.structs[key].type_params.push(TypeParameter {
                    name: type_param.name.clone(),
                    id: id,
                    bound: None
                });
            }
            struct_namespaces.insert(key, struct_ns);

            for item in items {
                if let ast::StructItem::Field { name, typ, loc } = item {
                    let resolved = resolve_type(&checker, struct_ns, typ);
                    checker.hir.structs[key].fields.insert(name.clone(), StructField { name: name.clone(), typ: resolved, loc: *loc });
                }
            }
        }
    };

    CollectedFields { checker, file_namespaces, struct_namespaces, files, root }
}

fn collect_functions(collected: CollectedFields) -> CollectedFunctions {
    let CollectedFields { mut checker, files, file_namespaces, root, struct_namespaces } = collected;

    let mut func_namespaces = SecondaryMap::new();

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];
        for func in file.iter_functions() {
            let func_ns = checker.add_namespace(Some(file_ns));

            let mut type_params = vec![];
            for type_param in &func.type_parameters {
                let bound = if let Some(bound) = &type_param.bound {
                    let typ = resolve_type(&checker, file_ns, bound);
                    Some(typ)
                } else {
                    None
                };
                let id = checker.add_type_param();
                let typ = Type::TypeParameter { name: type_param.name.clone(), bound: bound.clone().map(|t| Box::new(t)), id };
                type_params.push(TypeParameter { name: type_param.name.clone(), bound, id });
                checker.add_type(func_ns, type_param.name.clone(), typ);
            }

            let mut params = Vec::new();
            for param in &func.parameters {
                let typ = resolve_type(&checker, func_ns, &param.typ);
                let decl = checker.add_name(func_ns, param.name.clone(), NameInfo::Local { typ: typ.clone(), loc: param.loc });
                params.push(Parameter { name: param.name.clone(), typ, loc: param.loc, decl });
            }
            let ret = func.return_type.as_ref().map_or(Type::Unit, |t| resolve_type(&checker, func_ns, t));
            // let sig = Type::Function { params: params.iter().map(|p| p.typ.clone()).collect(), ret: Box::new(ret.clone()) };

            let key = checker.hir.function_prototypes.insert_with_key(|key| {
                let decl = checker.hir.names.insert(NameInfo::Function { func: key });
                checker.namespaces[file_ns].names.insert(func.name.clone(), decl);
                let sig = Type::GenericFunction { func: key, type_params: type_params.clone(), params: params.iter().map(|p| p.typ.clone()).collect(), ret: Box::new(ret.clone()) };
                FunctionPrototype { name: func.name.clone(), type_params, params, ret, sig, decl }
            });

            if func.name == "main" {
                checker.hir.main_function = Some(key);
            }

            // checker.add_name(file_ns, func.name.clone(), NameInfo::Function { func: key });
            func_namespaces.insert(key, func_ns);
        }
    }

    if checker.hir.main_function.is_none() {
        checker.push_error(TypeCheckError::NoMainFunction);
    }

    CollectedFunctions { checker, files, file_namespaces, function_namespaces: func_namespaces, root, struct_namespaces }
}

fn collect_function_bodies(collected: CollectedFunctions) -> Result<HIR, Vec<TypeCheckError>> {
    let CollectedFunctions { mut checker, files, file_namespaces, function_namespaces, .. } = collected;

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];

        for ast_func in file.iter_functions() {
            let func_key = checker.get_function_key(file_ns, &ast_func.name);
            let func_ns = function_namespaces[func_key];

            let func = &checker.hir.function_prototypes[func_key];
            let ret = func.ret.clone();

            let mut core = ResolveContextCore::new();
            let mut resolver = ResolveContext::create(&mut checker, func_key, func_ns, &mut core);

            let body = resolver.resolve_block(&ast_func.body, Some(ret), Some(func_ns));
            checker.hir.function_bodies.insert(func_key, FunctionBody { body });
        }
    }

    checker.finalize()
}


type ExpectedType = Option<Type>;

struct ResolveContextCore {
    count: RangeFrom<u64>,
    generic_stack: SlotMap<TypeParamKey, Option<Type>>
}

impl ResolveContextCore {
    fn new() -> ResolveContextCore {
        ResolveContextCore {
            count: 0..,
            generic_stack: SlotMap::with_key()
        }
    }
}

struct ResolveContext<'a, 'b> where 'a: 'b {
    checker: &'b mut TypeCheck<'a>,
    func: FunctionKey,
    namespace: NamespaceKey,
    core: &'b mut ResolveContextCore
}

impl<'a, 'b> ResolveContext<'a, 'b>  where 'a: 'b  {
    fn create(checker: &'b mut TypeCheck<'a>, func: FunctionKey, namespace: NamespaceKey, core: &'b mut ResolveContextCore) -> ResolveContext<'a, 'b> {
        ResolveContext {
            checker, func, namespace, core
        }
    }

    fn push_error(&self, error: TypeCheckError<'a>) {
        self.checker.push_error(error)
    }

    fn resolve_name(&self, name: &str) -> Option<NameKey> {
        self._resolve_name(self.namespace, name)
    }

    fn _resolve_name(&self, ns: NamespaceKey, name: &str) -> Option<NameKey> {
        match self.checker.namespaces[ns].names.get(name) {
            Some(n) => Some(*n),
            None => self.checker.namespaces[ns].parent.and_then(|p| self._resolve_name(p, name))
        }
    }

    fn add_name(&mut self, name: String, info: NameInfo<'a>) -> NameKey {
        self.checker.add_name(self.namespace, name, info)
    }

    fn resolve_type(&mut self, typ: &ast::Type<'a>) -> Type {
        resolve_type(self.checker, self.namespace, typ)
    }

    fn type_of(&self, expr: &Expr<'a>) -> Type {
        self.checker.type_of_expr(expr)
    }

    fn display_type(&self, ty: &Type) -> DisplayType<'a> {
        match ty {
            Type::Unit => DisplayType::Unit,
            Type::Never => DisplayType::Never,
            Type::Boolean => DisplayType::Boolean,
            Type::Errored => DisplayType::Errored,
            Type::Integer { bits } => DisplayType::Integer { bits: *bits },
            Type::Struct { struct_, variant } => {
                let s = &self.checker.hir.structs[*struct_];
                DisplayType::Struct {
                    name: s.name.clone(),
                    variant: variant.iter().map(|t| self.display_type(t)).collect(),
                    loc: s.loc
                }
            },
            Type::Function { params, ret } => DisplayType::Function {
                params: params.iter().map(|t| self.display_type(t)).collect(),
                ret: Box::new(self.display_type(ret))
            },
            Type::GenericFunction { type_params, params, ret, .. } => {
                let type_params: Vec<_> = type_params.iter().map(|p| self.display_type(&p.as_type())).collect();
                let params: Vec<_> = params.iter().map(|p| self.display_type(p)).collect();
                let ret = Box::new(self.display_type(ret));
                DisplayType::GenericFunction { type_params, params, ret }
            },
            Type::TypeParameter { name, bound, .. } => {
                DisplayType::TypeParameter { name: name.clone(), bound: bound.as_ref().map(|t| Box::new(self.display_type(t))) }
            }
            Type::TypeParameterInstance { name, id, .. } => {
                if let Some(ty) = &self.core.generic_stack[*id] {
                    self.display_type(ty)
                } else {
                    DisplayType::TypeParamInstance { name: name.clone() }
                }
            }
        }
    }

    fn check(&mut self, got_type: &Type, expected: ExpectedType, loc: &Location<'a>) -> bool {
        match expected {
            Some(Type::Errored) => false,
            Some(Type::TypeParameterInstance { id, .. }) => {
                let inferred_type = (&self.core.generic_stack[id]).as_ref();
                if let Some(typ) = inferred_type {
                    return self.check(got_type, Some(typ.clone()), loc);
                } else {
                    self.core.generic_stack[id] = Some(got_type.clone());
                    return true
                }
            }
            None => true,
            Some(t) => {
                if let Type::TypeParameterInstance { id, .. } = got_type {
                    let inferred_type = self.core.generic_stack[*id].clone();
                    if let Some(typ) = inferred_type {
                        return self.check(&typ, Some(t), loc);
                    } else {
                        return false;
                    }
                }
                let is_compat = self.checker.hir.is_subtype(got_type, &t);
                if !is_compat {
                    self.push_error(TypeCheckError::IncompatibleTypes{
                        expected: self.display_type(&t),
                        got: self.display_type(got_type),
                        loc: *loc
                    })
                }
                is_compat
            }
        }
    }

    fn resolve_block(&mut self, block: &ast::Block<'a>, yield_type: ExpectedType, with_ns: Option<NamespaceKey>) -> Block<'a> {
        let ast::Block { stmts, trailing_expr, loc } = block;

        let block_ns = with_ns.unwrap_or_else(||
            self.checker.add_namespace(Some(self.namespace))
        );
        let stmts = {
            let mut child = ResolveContext {
                namespace: block_ns,
                func: self.func,
                checker: self.checker,
                core: self.core
            };
            let stmts: Vec<_> = stmts.iter().map(|stmt| child.resolve_stmt(stmt)).collect();
            stmts
        };
        let always_breaks = stmts.iter().any(|stmt| stmt.does_break());

        let trailing_expr = match trailing_expr {
            Some(e) => {
                let mut child = ResolveContext {
                    namespace: block_ns,
                    func: self.func,
                    checker: self.checker,
                    core: self.core
                };
                Some(Box::new(child.resolve_expr(e, yield_type)))
            },
            None => {
                if always_breaks {
                    None
                } else {
                    if !self.check(&Type::Unit, yield_type, loc) {
                        Some(Box::new(Expr::Errored { loc: *loc }))
                    } else {
                        Some(Box::new(Expr::Unit { loc: *loc }))
                    }
                }
            }
        };
        let declared = self.checker.namespaces[block_ns].get_names();
        Block { stmts, trailing_expr, declared, loc: *loc }
    }

    fn resolve_expr(&mut self, expr: &ast::Expr<'a>, yield_type: ExpectedType) -> Expr<'a> {
        match expr {
            ast::Expr::Integer { number, loc } => {
                let compat = self.check(&Type::Integer { bits: 32 }, yield_type, loc);
                if compat {
                    Expr::Integer { num: *number, loc: *loc }
                } else {
                    Expr::Errored { loc: *loc }
                }
            }
            ast::Expr::Name { name, loc } => {
                let resolved = self.resolve_name(name);
                let decl = match resolved {
                    Some(key) => key,
                    None => {
                        self.push_error(TypeCheckError::CouldNotResolveName(name.clone(), *loc));
                        return Expr::Errored { loc: *loc };
                    }
                };
                let typ = self.checker.hir.type_of_name(decl);
                let compat = self.check(&typ, yield_type, loc);
                if compat {
                    Expr::Name { decl, loc: *loc }
                } else {
                    Expr::Errored { loc: *loc }
                }
            },
            ast::Expr::Block(block) => {
                Expr::Block(self.resolve_block(block, yield_type, None))
            }
            ast::Expr::Call { callee, arguments, loc } => {
                let resolved_callee = self.resolve_expr(callee, None);
                let callee_ty = self.checker.hir.type_of_expr(&resolved_callee);
                match &callee_ty {
                    Type::Function { params, ret } => {
                        if !self.check(ret, yield_type, loc) {
                            return Expr::Errored { loc: *loc };
                        }
                        if params.len() != arguments.len() {
                            self.push_error(TypeCheckError::MismatchedArguments { expected: params.len(), got: arguments.len(), loc: *loc });
                            return Expr::Errored { loc: *loc };
                        }
                        let mut resolved_arguments = Vec::new();
                        for (param, arg) in params.iter().zip(arguments.iter()) {
                            let resolved_arg = self.resolve_expr(arg, Some(param.clone()));
                            resolved_arguments.push(resolved_arg);
                        }
                        Expr::Call { callee: Box::new(resolved_callee), arguments: resolved_arguments, loc: *loc }
                    }
                    Type::GenericFunction { func, type_params, params, ret } => {
                        let mut map = HashMap::new();
                        for type_param in type_params {
                            let type_param_key = self.core.generic_stack.insert(None);
                            map.insert(type_param.id, Type::TypeParameterInstance { name: type_param.name.clone(), id: type_param_key });
                        }

                        if params.len() != arguments.len() {
                            self.push_error(TypeCheckError::MismatchedArguments { expected: params.len(), got: arguments.len(), loc: *loc });
                            return Expr::Errored { loc: *loc };
                        }

                        let mut resolved_arguments = Vec::new();
                        for (param, arg) in params.iter().zip(arguments.iter()) {
                            let resolved_arg = self.resolve_expr(arg, Some(param.subs(&map)));
                            resolved_arguments.push(resolved_arg);
                        }

                        if !self.check(&ret.subs(&map), yield_type, loc) {
                            println!("displaying error");
                            return Expr::Errored { loc: *loc };
                        }

                        let mut generic_tuple = vec![];
                        for type_param in type_params {
                            let Type::TypeParameterInstance { id, .. } = map[&type_param.id] else {
                                panic!()
                            };
                            if let Some(typ) = &self.core.generic_stack[id] {
                                generic_tuple.push(typ.clone());
                            } else {
                                self.push_error(TypeCheckError::CouldNotInferParameter(type_param.name.clone(), *loc));
                            }
                        };
                        if generic_tuple.len() != map.len() {
                            return Expr::Errored { loc: *loc };
                        }

                        Expr::GenericCall { generic: generic_tuple, callee: *func, arguments: resolved_arguments, loc: *loc }
                    }
                    _ => {
                        self.push_error(TypeCheckError::ExpectedFunction { got: self.display_type(&callee_ty), loc: callee.loc() });
                        Expr::Errored { loc: *loc }
                    }
                }
            }
            ast::Expr::New { struct_, type_args, fields, loc } => {
                let Some(struct_key) = resolve_struct(self.checker, self.namespace, struct_) else {
                    self.push_error(TypeCheckError::ExpectedStructName { got: struct_.clone(), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                let mut map = HashMap::new();
                for type_param in &self.checker.hir.structs[struct_key].type_params {
                    let type_param_key = self.core.generic_stack.insert(None);
                    map.insert(type_param.id, Type::TypeParameterInstance { name: type_param.name.clone(), id: type_param_key });
                }

                let mut expected_fields = self.checker.hir.structs[struct_key].fields.clone();
                let mut resolved_fields = IndexMap::new();
                for field in fields {
                    if !expected_fields.contains_key(&field.field_name) {
                        self.push_error(TypeCheckError::NoSuchFieldName { field: field.field_name.clone(), typ: struct_.clone(), loc: field.name_loc });
                        return Expr::Errored { loc: *loc };
                    }
                    let expected_type = expected_fields[&field.field_name].typ.subs(&map);
                    let resolved = Box::new(self.resolve_expr(&field.argument, Some(expected_type)));
                    resolved_fields.insert(field.field_name.clone(), resolved);
                    expected_fields.remove(&field.field_name);
                };
                if !expected_fields.is_empty() {
                    self.push_error(TypeCheckError::MissingFields { fields: expected_fields.drain(..).map(|p| p.0).collect(), typ: struct_.clone(), loc: *loc });
                    return Expr::Errored { loc: *loc };
                }

                let mut variant = vec![];
                for type_param in &self.checker.hir.structs[struct_key].type_params {
                    let Type::TypeParameterInstance { id, .. } = map[&type_param.id] else {
                        panic!()
                    };
                    if let Some(typ) = &self.core.generic_stack[id] {
                        variant.push(typ.clone());
                    } else {
                        self.push_error(TypeCheckError::CouldNotInferParameter(type_param.name.clone(), *loc));
                    }
                }

                Expr::New { struct_: struct_key, variant, fields: resolved_fields, loc: *loc }
            }
            ast::Expr::GetAttr { obj, attr, loc } => {
                let obj = self.resolve_expr(obj, None);

                let ty = self.checker.hir.type_of_expr(&obj);

                let Type::Struct { struct_, variant } = ty else {
                    self.push_error(TypeCheckError::ExpectedStruct { got: self.display_type(&ty), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                let mut map = HashMap::new();
                for (type_param, type_arg) in self.checker.hir.structs[struct_].type_params.iter().zip(variant.iter()) {
                    map.insert(type_param.id, type_arg.clone());
                }

                let Some(field) = self.checker.hir.structs[struct_].fields.get(attr) else {
                    self.push_error(TypeCheckError::NoSuchFieldName { typ: self.checker.hir.structs[struct_].name.clone(), field: attr.clone(), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                if !self.check(&field.typ.subs(&map), yield_type, loc) {
                    return Expr::Errored { loc: *loc };
                }

                Expr::GetAttr { obj: Box::new(obj), attr: attr.clone(), loc: *loc }

            },
            ast::Expr::GenericCall { .. } => todo!(),
            ast::Expr::BinOp { .. } => todo!()
        }
    }

    fn resolve_stmt(&mut self, stmt: &ast::Stmt<'a>) -> Stmt<'a> {
        match stmt {
            ast::Stmt::Decl { name, typ, value, loc } => {
                match typ {
                    Some(t) => {
                        let typ = self.resolve_type(t);
                        let value = self.resolve_expr(value, Some(typ.clone()));
                        let decl = self.add_name(name.clone(), NameInfo::Local { typ, loc: *loc});
                        Stmt::Decl { decl, value, loc: *loc }
                    },
                    None => {
                        let value = self.resolve_expr(value, None);
                        let decl = self.add_name(name.clone(), NameInfo::Local { typ: self.type_of(&value), loc: *loc});
                        Stmt::Decl { decl, value, loc: *loc }
                    }
                }
            },
            ast::Stmt::Expr { expr, loc } => {
                Stmt::Expr { expr: self.resolve_expr(expr, None), loc: *loc }
            },
            ast::Stmt::Return { value, loc} => {
                let expected_return = self.checker.hir.function_prototypes[self.func].ret.clone();
                Stmt::Return { value: self.resolve_expr(value, Some(expected_return)), loc: *loc }
                // let value = self.resolve_expr(value, None);
                // self.check(&self.checker.hir.type_of_expr(&value), Some(expected_return), loc);
                // Stmt::Return { value, loc: *loc }
            }
        }
    }
}


// todo fix tests
#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::path::Path;
    use crate::ast;
    use crate::hir::{FunctionKey, HIR, StructKey, Type};
    use crate::source::Source;
    use crate::type_check::{collect_fields, collect_function_bodies, collect_functions, collect_structs, initialize, resolve_types};

    fn source(name: &str, text: &str) -> Source {
        Source::from_text(name, text.to_owned())
    }

    fn parse_one(s: &Source) -> ast::AST {
        let file = crate::parser::parse_file(s).unwrap();
        let mut map = HashMap::new();
        map.insert(file.path.clone(), file);
        ast::AST { name: "".into(), files: map }
    }

    #[test]
    fn verify_collect_structs() {
        let s = source("<test>", r"
            struct Alpha { }

            struct Beta { }
        ");
        let ast = parse_one(&s);

        let initial = initialize(ast);
        let collected = collect_structs(initial);

        let hir = collected.checker.finalize().unwrap();
        let structs: Vec<_> = hir.structs.values().collect();

        let expected_names = vec!["Alpha", "Beta"];
        assert!(expected_names.iter().all(|&name| structs.iter().any(|s| s.name == name)));
    }

    #[test]
    fn verify_collect_fields() {
        let s = source("<test>", r"
            struct Alpha {
                x: i32;
                y: Alpha;
                z: Beta;
            }

            struct Beta {
                x: bool;
                z: Beta;
            }
        ");
        let ast = parse_one(&s);

        let initial = initialize(ast);
        let collected = collect_fields(collect_structs(initial));

        let file = collected.file_namespaces[Path::new("<test>")];

        let alpha_key = collected.checker.get_struct_key(file, "Alpha");
        let beta_key = collected.checker.get_struct_key(file, "Beta");

        let hir = collected.checker.finalize().unwrap();

        let alpha = &hir.structs[alpha_key];
        assert_eq!(alpha.fields.len(), 3);
        assert_eq!(alpha.fields["x"].typ, Type::Integer { bits: 32 });
        assert_eq!(alpha.fields["y"].typ, Type::Struct { struct_: alpha_key, variant: vec![] });
        assert_eq!(alpha.fields["z"].typ, Type::Struct { struct_: beta_key, variant: vec![] });

        let beta = &hir.structs[beta_key];

        assert_eq!(beta.fields.len(), 2);
        assert_eq!(beta.fields["x"].typ, Type::Boolean);
        assert_eq!(beta.fields["z"].typ, Type::Struct { struct_: beta_key, variant: vec![] });
    }

    #[test]
    fn verify_collect_functions() {
        let s = source("<test>", r"
            struct Alpha { }

            struct Beta { }

            fn aleph(a: i32) -> Alpha { }

            fn bet(b: Beta, c: Gamma) -> bool { }

            struct Gamma { }

            fn main() {

            }
        ");
        let ast = parse_one(&s);

        let initial = initialize(ast);
        let collected = collect_functions(collect_fields(collect_structs(initial)));

        let file = collected.file_namespaces[Path::new("<test>")];

        let alpha_key = collected.checker.get_struct_key(file, "Alpha");
        let beta_key = collected.checker.get_struct_key(file, "Beta");
        let gamma_key = collected.checker.get_struct_key(file, "Gamma");
        let aleph_key = collected.checker.get_function_key(file, "aleph");
        let bet_key = collected.checker.get_function_key(file, "bet");

        let hir = collected.checker.finalize().unwrap();

        let aleph = &hir.function_prototypes[aleph_key];
        let Type::GenericFunction { params, ret, .. } = &aleph.sig else { panic!("{:?}", &aleph.sig) };
        assert_eq!(ret.as_ref(), &Type::Struct { struct_: alpha_key, variant: vec![] });
        assert_eq!(params, &vec![Type::Integer { bits: 32 }]);

        let bet = &hir.function_prototypes[bet_key];
        let Type::GenericFunction { params, ret, ..} = &bet.sig else { panic!("{:?}", &bet.sig) };
        assert_eq!(ret.as_ref(), &Type::Boolean);
        assert_eq!(params, &vec![Type::Struct { struct_: beta_key, variant: vec![] }, Type::Struct { struct_: gamma_key, variant: vec![] }]);
    }

    fn get_struct_with_name(hir: &HIR, name: &str) -> StructKey {
        hir.structs.iter().find(|(_, struc)| struc.name == name).unwrap().0
    }

    fn get_func_with_name(hir: &HIR, name: &str) -> FunctionKey {
        hir.function_prototypes.iter().find(|(_, proto)| proto.name == name).unwrap().0
    }

    #[test]
    fn verify_collect_function_bodies() {
        let s = source("<test>", r"
            struct Alpha {
                a: i32;
                b: Alpha;
            }

            fn aleph(thing: Alpha) -> Alpha {
                return thing;
            }

            fn main() {

            }
        ");
        let ast = parse_one(&s);

        let initial = initialize(ast);
        let _ = collect_function_bodies(collect_functions(collect_fields(collect_structs(initial)))).unwrap();
    }

    #[test]
    fn test_resolve_types_1() {
        let s = source("<test>", r"
            struct Alpha {
                a: i32;
                b: Alpha;
            }

            fn aleph(thing: Alpha) -> Alpha {
                return thing;
            }

            fn main() {

            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_resolve_types_2() {
        let s = source("<test>", r"
            struct Alpha {
                a: i32;
                b: Alpha;
            }

            fn aleph(thing: Alpha) -> Alpha {
                return 1;
            }

            fn main() {

            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_resolve_types_3() {
        let s = source("<test>", r"
            struct Alpha {
                a: i32;
                b: Alpha;
            }

            fn aleph(thing: Alpha) -> Alpha {
                1
            }

            fn main() {

            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_resolve_types_4() {
        let s = source("<test>", r"
            struct Alpha {
                a: i32;
                b: Alpha;
            }

            fn aleph(thing: Alpha) {
                return 1;
            }

            fn main() {

            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_resolve_types_5() {
        let s = source("<test>", r"
            struct Alpha {
                a: i32;
                b: Alpha;
            }

            fn aleph(thing: Alpha) { }

            fn main() {

            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }
}
