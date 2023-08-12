use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use indexmap::IndexMap;
use slotmap::{new_key_type, SecondaryMap, SlotMap};
use crate::parsing::ast;
use crate::util::{map_join, pluralize};
use crate::error::Message;
use crate::lowering::hir::*;
use crate::source::{HasLoc, Location};

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
    StructDuplicated(String, Location<'a>, Location<'a>),
    FieldDuplicated(String, String, Location<'a>, Location<'a>),
    CouldNotResolveName(String, Location<'a>),
    CouldNotResolveType(String, Location<'a>),
    IncompatibleTypes { expected: DisplayType<'a>, got: DisplayType<'a>, loc: Location<'a> },
    CannotUseNever { loc: Location<'a> },
    ExpectedFunction { got: DisplayType<'a>, loc: Location<'a> },
    UnexpectedClosure { expected: DisplayType<'a>, loc: Location<'a> },
    ClosureWithWrongParameters { expected: DisplayType<'a>, got: usize, loc: Location<'a> },
    MismatchedArguments { expected: usize, got: usize, loc: Location<'a> },
    MismatchedTypeArguments { expected: usize, got: usize, loc: Location<'a> },
    RequiredTypeArguments { got: String, loc: Location<'a>, note_loc: Location<'a> },
    ExpectedStructName { got: String, loc: Location<'a> },
    ExpectedStruct { got: DisplayType<'a>, loc: Location<'a> },
    NoSuchFieldName { field: String, typ: String, loc: Location<'a> },
    MissingFields { fields: Vec<String>, typ: String, loc: Location<'a> },
    CouldNotInferTypeParameter(String, Location<'a>),
    CouldNotInferParameters(Location<'a>),
    NoMainFunction,
    MultipleMainFunctions(Location<'a>, Location<'a>),
    MainMustHaveNoArguments(Location<'a>),
    MainMustReturnI32(Location<'a>),
}

impl<'a> Message for TypeCheckError<'a> {
    fn write_into<W: fmt::Write>(&self, to: &mut W) -> fmt::Result {
        match self {
            TypeCheckError::StructDuplicated(name, loc, prev_loc) => {
                writeln!(to, "Error: A struct called '{name}' was already defined in this scope.")?;
                Self::show_location(loc, to)?;
                writeln!(to, " | Note: A struct called '{name}' was previously defined here.")?;
                Self::show_note_location(prev_loc, to)
            }
            TypeCheckError::FieldDuplicated(name, struct_name, loc, prev_loc) => {
                writeln!(to, "Error: The struct '{struct_name}' already has a field called '{name}'.")?;
                Self::show_location(loc, to)?;
                writeln!(to, " | Note: A field called '{name}' was previously defined here.")?;
                Self::show_note_location(prev_loc, to)
            }
            TypeCheckError::CouldNotResolveName(name, loc) => {
                writeln!(to, "Error: Could not resolve the name '{}'.", name)?;
                Self::show_location(loc, to)
            }
            TypeCheckError::CouldNotResolveType(name, loc) => {
                writeln!(to, "Error: Could not resolve the type '{}'.", name)?;
                Self::show_location(loc, to)
            }
            TypeCheckError::IncompatibleTypes { expected, got, loc } => {
                writeln!(to, "Error: Incompatible types. Expected '{}' but got '{}'.", expected.render(), got.render())?;
                Self::show_location(loc, to)
            }
            TypeCheckError::CannotUseNever { loc } => {
                writeln!(to, "Error: Values of type '!' cannot exist.")?;
                Self::show_location(loc, to)
            }
            TypeCheckError::ExpectedFunction { got, loc } => {
                writeln!(to, "Error: Expected a function to call, but got '{}'.", got.render())?;
                Self::show_location(loc, to)
            }
            TypeCheckError::UnexpectedClosure { expected, loc } => {
                writeln!(to, "Error: Expected a value of type '{}', but got a closure.", expected.render())?;
                Self::show_location(loc, to)
            }
            TypeCheckError::ClosureWithWrongParameters { expected, got, loc } => {
                writeln!(to, "Error: Expected a closure of type '{}', but the closure has {}.", expected.render(), pluralize("parameter", *got as u64))?;
                Self::show_location(loc, to)
            }
            TypeCheckError::MismatchedArguments { expected, got, loc } => {
                writeln!(to, "Error: Expected {}, got {}.", pluralize("argument", *expected as u64), pluralize("argument", *got as u64))?;
                Self::show_location(loc, to)
            }
            TypeCheckError::MismatchedTypeArguments { expected, got, loc } => {
                writeln!(to, "Error: Expected {}, got {}.", pluralize("type parameter", *expected as u64), pluralize("type parameter", *got as u64))?;
                Self::show_location(loc, to)
            }
            TypeCheckError::RequiredTypeArguments { got, loc, note_loc } => {
                writeln!(to, "Error: Expected type parameters to be supplied for '{got}'.")?;
                Self::show_location(loc, to)?;
                writeln!(to, " | Note: '{got}' defined here.")?;
                Self::show_note_location(note_loc, to)
            }
            TypeCheckError::NoMainFunction => {
                writeln!(to, "Error: No main function could be found.")
            }
            TypeCheckError::ExpectedStructName { got, loc } => {
                writeln!(to, "Error: Could not find a struct named '{}'.", got)?;
                Self::show_location(loc, to)
            }
            TypeCheckError::ExpectedStruct { got, loc } => {
                writeln!(to, "Error: Expected a struct, but got '{}'.", got.render())?;
                Self::show_location(loc, to)
            }
            TypeCheckError::NoSuchFieldName { field, typ, loc } => {
                writeln!(to, "Error: '{}' Does not contain a field named '{}'.", typ, field)?;
                Self::show_location(loc, to)
            }
            TypeCheckError::MissingFields { fields, typ, loc } => {
                let rendered_fields: Vec<_> = fields.iter().map(|f| format!("'{f}'")).collect();
                writeln!(to, "Error: Fields {} were not supplied to initialize '{}'.", rendered_fields.join(", "), typ)?;
                Self::show_location(loc, to)
            }
            TypeCheckError::CouldNotInferTypeParameter(name, loc) => {
                writeln!(to, "Error: Could not infer the type of '{}'.", name)?;
                Self::show_location(loc, to)
            }
            TypeCheckError::CouldNotInferParameters(loc) => {
                writeln!(to, "Error: Could not infer the type of the parameters of the closure.")?;
                Self::show_location(loc, to)
            }
            TypeCheckError::MainMustHaveNoArguments(loc) => {
                writeln!(to, "Error: 'main' function must have no arguments.")?;
                Self::show_location(loc, to)
            }
            TypeCheckError::MainMustReturnI32(loc) => {
                writeln!(to, "Error: 'main' function must return 'i32'.")?;
                Self::show_location(loc, to)
            }
            TypeCheckError::MultipleMainFunctions(new_loc, old_loc) => {
                writeln!(to, "Error: Multiple 'main' functions defined.")?;
                Self::show_location(new_loc, to)?;
                writeln!(to, " | Note: Previous 'main' function defined here.")?;
                Self::show_note_location(old_loc, to)
            }
        }
    }
}

new_key_type! {
    pub struct NamespaceKey;
}

struct Namespace {
    pub parent: Option<NamespaceKey>,
    pub names: HashMap<String, NameKey>,
    pub types: HashMap<String, Type>,
    pub structs: HashMap<String, StructKey>,
    pub namespaces: HashMap<String, NamespaceKey>,
}

impl Namespace {
    fn new(parent: Option<NamespaceKey>) -> Namespace {
        Namespace {
            parent,
            names: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            namespaces: HashMap::new(),
        }
    }

    pub fn get_names(&self) -> HashMap<String, NameKey> {
        self.names.clone()
    }
}

struct TypeCheck<'a> {
    namespaces: SlotMap<NamespaceKey, Namespace>,
    errors: RefCell<Vec<TypeCheckError<'a>>>,
    hir: HIR<'a>,
    type_param_counter: u64
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
            if checker.namespaces[file_ns].structs.contains_key(name) {
                let prev_key = checker.namespaces[file_ns].structs[name];
                checker.push_error(TypeCheckError::StructDuplicated(name.clone(), *loc, checker.hir.structs[prev_key].loc));
            }

            checker.add_struct(file_ns, Struct { name: name.clone(), type_params: Vec::new(), super_struct: None, fields: IndexMap::new(), loc: *loc });
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
        ast::Type::Function { parameters, ret, .. } => {
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
        for ast::Struct { name, type_params, items, super_struct, .. } in file.iter_structs() {
            let key = checker.get_struct_key(file_ns, name);

            if let Some((super_name, loc)) = super_struct {
                if let Some(super_key) = checker.namespaces[file_ns].structs.get(super_name) {
                    checker.hir.structs[key].super_struct = Some((*super_key, vec![], *loc));
                } else {
                    checker.push_error(TypeCheckError::CouldNotResolveName(super_name.clone(), *loc));
                }
            }

            let struct_ns = checker.add_namespace(Some(file_ns));

            for type_param in type_params {
                let id = checker.add_type_param();
                let typ = Type::TypeParameter { name: type_param.name.clone(), id, bound: None };
                checker.add_type(struct_ns, type_param.name.clone(), typ);
                checker.hir.structs[key].type_params.push(TypeParameter { name: type_param.name.clone(), id, bound: None });
            }
            struct_namespaces.insert(key, struct_ns);

            for item in items {
                if let ast::StructItem::Field { name: field_name, typ, loc } = item {
                    let resolved = resolve_type(&checker, struct_ns, typ);
                    if let Some(prev) = checker.hir.structs[key].fields.get(field_name) {
                        checker.push_error(TypeCheckError::FieldDuplicated(field_name.clone(), name.clone(), *loc, prev.loc));
                    } else {
                        checker.hir.structs[key].fields.insert(field_name.clone(), StructField { name: field_name.clone(), typ: resolved, loc: *loc });
                    }
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
                let decl = checker.add_name(func_ns, param.name.clone(), NameInfo::Local { typ: typ.clone(), loc: param.loc, level: 0 });
                params.push(Parameter { name: param.name.clone(), typ, loc: param.loc, decl });
            }
            let ret = func.return_type.as_ref().map_or(Type::Unit, |t| resolve_type(&checker, func_ns, t));

            let key = checker.hir.function_prototypes.insert_with_key(|key| {
                let decl = checker.hir.names.insert(NameInfo::Function { func: key });
                checker.namespaces[file_ns].names.insert(func.name.clone(), decl);
                let sig = Type::GenericFunction { func: key, type_params: type_params.clone(), params: params.iter().map(|p| p.typ.clone()).collect(), ret: Box::new(ret.clone()) };
                FunctionPrototype { name: func.name.clone(), type_params, params, ret, sig, decl, loc: func.loc }
            });

            if func.name == "main" {
                let proto = &checker.hir.function_prototypes[key];
                if !proto.params.is_empty() {
                    checker.push_error(TypeCheckError::MainMustHaveNoArguments(func.loc))
                }
                if !matches!(proto.ret, Type::Integer { bits: 32 }) {
                    checker.push_error(TypeCheckError::MainMustReturnI32(func.loc))
                }

                if let Some(prev_func) = checker.hir.main_function {
                    let prev_loc = checker.hir.function_prototypes[prev_func].loc;
                    checker.push_error(TypeCheckError::MultipleMainFunctions(func.loc, prev_loc))
                } else {
                    checker.hir.main_function = Some(key);
                }
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
            let mut resolver = ResolveContext::create_for_function(&mut checker, Some(ret.clone()), func_ns, &mut core);

            let body = resolver.resolve_block(&ast_func.body, Some(ret), Some(func_ns));
            checker.hir.function_bodies.insert(func_key, FunctionBody { body });
        }
    }

    checker.finalize()
}


type ExpectedType = Option<Type>;

struct ResolveContextCore {
    generic_stack: SlotMap<TypeParamKey, Option<Type>>
}

impl ResolveContextCore {
    fn new() -> ResolveContextCore {
        ResolveContextCore {
            generic_stack: SlotMap::with_key()
        }
    }
}

struct ResolveContext<'a, 'b> where 'a: 'b {
    checker: &'b mut TypeCheck<'a>,
    expected_return: Option<Type>,
    namespace: NamespaceKey,
    core: &'b mut ResolveContextCore,
    level: usize,

    return_types: Vec<Type>
}

impl<'a, 'b> ResolveContext<'a, 'b>  where 'a: 'b  {
    fn create_for_function(checker: &'b mut TypeCheck<'a>, expected_return: Option<Type>, namespace: NamespaceKey, core: &'b mut ResolveContextCore) -> ResolveContext<'a, 'b> {
        ResolveContext {
            checker, expected_return, namespace, core,
            level: 0,
            return_types: vec![]
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

    fn cast_to_maybe_type(&self, to_type: Option<Type>, resolved_expr: Expr<'a>) -> Expr<'a> {
        if let Some(ty) = to_type {
            self.cast_to_type(resolved_expr, &ty)
        } else {
            resolved_expr
        }
    }

    fn cast_to_type(&self, resolved_expr: Expr<'a>, to_type: &Type) -> Expr<'a> {
        let actual_type = self.type_of(&resolved_expr);

        let incompatible_types = || {
            self.push_error(TypeCheckError::IncompatibleTypes {
                expected: self.display_type(to_type),
                got: self.display_type(&actual_type),
                loc: resolved_expr.loc()
            });
            Expr::Errored { loc: resolved_expr.loc() }
        };
        if &actual_type == to_type {
            resolved_expr
        } else {
            match (&actual_type, to_type) {
                (Type::Errored, _) | (_, Type::Errored) => {
                    return Expr::Errored { loc: resolved_expr.loc() };
                },
                (Type::Never, to_ty) => Expr::Cast(Box::new(resolved_expr), to_ty.clone()),
                (Type::Integer { bits: a }, Type::Integer { bits: b}) => {
                    if a <= b {
                        return Expr::Cast(Box::new(resolved_expr), Type::Integer { bits: *b });
                    } else {
                        return incompatible_types();
                    }
                },
                (Type::Struct { struct_: a, variant: a_var}, Type::Struct { struct_: b, variant: b_var}) => {
                    let mut curr_struct = (a, a_var);
                    while curr_struct != (b, b_var) {
                        if let Some((super_key, super_var, _)) = &self.checker.hir.structs[*curr_struct.0].super_struct {
                            curr_struct = (super_key, super_var);
                        } else {
                            return incompatible_types();
                        }
                    }

                    return Expr::Cast(Box::new(resolved_expr), Type::Struct { struct_: *curr_struct.0, variant: curr_struct.1.clone()});
                },
                (Type::Function { params: a_params, ret: a_ret }, Type::Function { params: b_params, ret: b_ret }) => {
                    todo!()
                },
                (Type::TypeParameter { id: a, .. }, Type::TypeParameter { id: b, .. }) => {
                    todo!()
                },
                (_, _) => {
                    return incompatible_types();
                }
            }
        }
    }

    fn can_cast_to_maybe_type(&self, from_type: &Type, to_type: Option<&Type>) -> bool {
        to_type.map_or(true, |ty| self.can_cast_to_type(from_type, ty))
    }

    fn can_cast_to_type(&self, from_type: &Type, to_type: &Type) -> bool {
        if from_type == to_type {
            true
        } else {
            match (from_type, to_type) {
                (Type::Errored, _) | (_, Type::Errored) => {
                    return true;
                },
                (Type::Never, _) => true,
                (Type::Integer { bits: a }, Type::Integer { bits: b}) => {
                    return a <= b;
                },
                (Type::Struct { struct_: a, variant: a_var}, Type::Struct { struct_: b, variant: b_var}) => {
                    let mut curr_struct = (a, a_var);
                    while curr_struct != (b, b_var) {
                        if let Some((super_key, super_var, _)) = &self.checker.hir.structs[*curr_struct.0].super_struct {
                            curr_struct = (super_key, super_var);
                        } else {
                            return false;
                        }
                    }

                    return true;
                },
                (Type::Function { params: a_params, ret: a_ret }, Type::Function { params: b_params, ret: b_ret }) => {
                    todo!()
                },
                (Type::TypeParameter { id: a, .. }, Type::TypeParameter { id: b, .. }) => {
                    todo!()
                },
                (_, _) => {
                    return false;
                }
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
                expected_return: self.expected_return.clone(),
                checker: self.checker,
                core: self.core,
                level: 0,
                return_types: vec![]
            };
            let stmts: Vec<_> = stmts.iter().map(|stmt| child.resolve_stmt(stmt)).collect();
            stmts
        };
        let always_breaks = stmts.iter().any(|stmt| stmt.always_diverges(&self.checker.hir));

        let trailing_expr = match trailing_expr {
            Some(e) => {
                let mut child = ResolveContext {
                    namespace: block_ns,
                    expected_return: self.expected_return.clone(),
                    checker: self.checker,
                    core: self.core,
                    level: 0,
                    return_types: vec![]
                };
                Some(Box::new(child.resolve_expr(e, yield_type)))
            },
            None => {
                if always_breaks {
                    None
                } else {
                    if yield_type.is_none() || yield_type.is_some_and(|ty| matches!(ty, Type::Unit)) {
                        Some(Box::new(Expr::Unit { loc: *loc }))
                    } else {
                        Some(Box::new(Expr::Errored { loc: *loc }))
                    }
                }
            }
        };
        let declared = self.checker.namespaces[block_ns].get_names();
        let yield_type = if always_breaks {
            Type::Never
        } else if let Some(expr) = &trailing_expr {
            self.type_of(expr)
        } else {
            Type::Unit
        };
        Block { stmts, trailing_expr, yield_type, declared, loc: *loc }
    }

    fn is_never(&self, expr: &Expr<'a>) -> bool {
        if self.checker.hir.type_of_expr(expr).is_never(&self.checker.hir) {
            self.push_error(TypeCheckError::CannotUseNever { loc: expr.loc() });
            return true;
        } else {
            return false;
        }
    }

    fn resolve_expr(&mut self, expr: &ast::Expr<'a>, yield_type: ExpectedType) -> Expr<'a> {
        let to_type = yield_type.clone();
        let resolved_expr = match expr {
            ast::Expr::Integer { number, loc } => {
                Expr::Integer { num: *number, loc: *loc }
            }
            ast::Expr::Bool { value, loc} => {
                Expr::Bool { value: *value, loc: *loc }
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
                Expr::Name { decl, loc: *loc }
            },
            ast::Expr::Block(block) => {
                Expr::Block(self.resolve_block(block, yield_type, None))
            }
            ast::Expr::Call { callee, arguments, loc } => {
                let resolved_callee = self.resolve_expr(callee, None);
                if self.is_never(&resolved_callee) {
                    return Expr::Errored { loc: *loc };
                };

                let callee_ty = self.type_of(&resolved_callee);
                match &callee_ty {
                    Type::Function { params, .. } => {
                        if params.len() != arguments.len() {
                            self.push_error(TypeCheckError::MismatchedArguments { expected: params.len(), got: arguments.len(), loc: *loc });
                            return Expr::Errored { loc: *loc };
                        }
                        let mut resolved_arguments = Vec::new();
                        for (param, arg) in params.iter().zip(arguments.iter()) {
                            let resolved_arg = self.resolve_expr(arg, Some(param.clone()));
                            if self.is_never(&resolved_arg) {
                                return Expr::Errored { loc: *loc };
                            };
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
                            if self.is_never(&resolved_callee) {
                                return Expr::Errored { loc: *loc };
                            };
                            resolved_arguments.push(resolved_arg);
                        }

                        if !self.can_cast_to_maybe_type(&ret.subs(&map), yield_type.as_ref()) {
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
                                self.push_error(TypeCheckError::CouldNotInferTypeParameter(type_param.name.clone(), *loc));
                            }
                        };
                        if generic_tuple.len() != map.len() {
                            return Expr::Errored { loc: *loc };
                        }

                        Expr::GenericCall { generic: generic_tuple, callee: *func, arguments: resolved_arguments, loc: *loc }
                    }
                    Type::Errored => {
                        Expr::Errored { loc: *loc }
                    }
                    _ => {
                        self.push_error(TypeCheckError::ExpectedFunction { got: self.display_type(&callee_ty), loc: callee.loc() });
                        Expr::Errored { loc: *loc }
                    }
                }
            }
            ast::Expr::New { struct_, fields, loc, .. } => {
                let Some(struct_key) = resolve_struct(self.checker, self.namespace, struct_) else {
                    self.push_error(TypeCheckError::ExpectedStructName { got: struct_.clone(), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                let mut map = HashMap::new();
                for type_param in &self.checker.hir.structs[struct_key].type_params {
                    let type_param_key = self.core.generic_stack.insert(None);
                    map.insert(type_param.id, Type::TypeParameterInstance { name: type_param.name.clone(), id: type_param_key });
                }

                let mut expected_fields = self.checker.hir.structs[struct_key].all_fields(&self.checker.hir);
                let mut resolved_fields = IndexMap::new();
                for field in fields {
                    let name = field.field_name.clone();
                    if !expected_fields.contains_key(&name) {
                        self.push_error(TypeCheckError::NoSuchFieldName { field: name.clone(), typ: struct_.clone(), loc: field.name_loc });
                        return Expr::Errored { loc: *loc };
                    }
                    let expected_type = expected_fields[&name].subs(&map);
                    let resolved = Box::new(self.resolve_expr(&field.argument, Some(expected_type)));
                    if self.is_never(&resolved) {
                        return Expr::Errored { loc: *loc };
                    };
                    expected_fields.remove(&name);
                    resolved_fields.insert(name, resolved);
                };
                if !expected_fields.is_empty() {
                    self.push_error(TypeCheckError::MissingFields { fields: expected_fields.into_iter().map(|p| p.0).collect(), typ: struct_.clone(), loc: *loc });
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
                        self.push_error(TypeCheckError::CouldNotInferTypeParameter(type_param.name.clone(), *loc));
                    }
                }

                Expr::New { struct_: struct_key, variant, fields: resolved_fields, loc: *loc }
            }
            ast::Expr::GetAttr { obj, attr, loc } => {
                let resolved_obj = self.resolve_expr(obj, None);
                if self.is_never(&resolved_obj) {
                    return Expr::Errored { loc: *loc };
                };

                let ty = self.checker.hir.type_of_expr(&resolved_obj);

                let Type::Struct { struct_, variant } = ty else {
                    self.push_error(TypeCheckError::ExpectedStruct { got: self.display_type(&ty), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                let mut map = HashMap::new();
                for (type_param, type_arg) in self.checker.hir.structs[struct_].type_params.iter().zip(variant.iter()) {
                    map.insert(type_param.id, type_arg.clone());
                }

                let Some(_) = self.checker.hir.structs[struct_].all_fields(&self.checker.hir).get(attr).cloned() else {
                    self.push_error(TypeCheckError::NoSuchFieldName { typ: self.checker.hir.structs[struct_].name.clone(), field: attr.clone(), loc: *loc });
                    return Expr::Errored { loc: *loc };
                };

                Expr::GetAttr { obj: Box::new(resolved_obj), attr: attr.clone(), loc: *loc }
            },
            ast::Expr::GenericCall { .. } => todo!(),
            ast::Expr::BinOp { .. } => todo!(),
            ast::Expr::IfElse { cond, then_do, else_do, loc } => {
                let resolved_cond = Box::new(self.resolve_expr(cond, Some(Type::Boolean)));
                if self.is_never(&resolved_cond) {
                    return Expr::Errored { loc: *loc };
                };

                let resolved_then_do = Box::new(self.resolve_expr(then_do, yield_type.clone()));
                let resolved_else_do = Box::new(self.resolve_expr(else_do, yield_type.clone()));

                let expr_yield_type: Type;
                if resolved_then_do.always_diverges(&self.checker.hir) && resolved_else_do.always_diverges(&self.checker.hir) {
                    expr_yield_type = Type::Never;
                } else if let Some(yield_type) = yield_type {
                    expr_yield_type = yield_type;
                } else {
                    let then_ty = self.checker.hir.type_of_expr(&resolved_then_do);
                    let else_ty = self.checker.hir.type_of_expr(&resolved_else_do);

                    if then_ty.is_error() || else_ty.is_error() {
                        return Expr::Errored { loc: *loc };
                    }

                    if self.can_cast_to_type(&then_ty, &else_ty) {
                        expr_yield_type = else_ty;
                    } else if self.can_cast_to_type(&else_ty, &then_ty) {
                        expr_yield_type = then_ty;
                    } else {
                        self.push_error(TypeCheckError::IncompatibleTypes { expected: self.display_type(&then_ty), got: self.display_type(&else_ty), loc: else_do.loc() });
                        return Expr::Errored { loc: *loc };
                    }
                }

                Expr::IfElse { cond: resolved_cond, then_do: resolved_then_do, else_do: resolved_else_do, yield_type: expr_yield_type, loc: *loc }
            }
            ast::Expr::Closure { parameters, body, loc } => {
                let closure_ns = self.checker.add_namespace(Some(self.namespace));

                let expected_return;
                let mut parameter_types: Vec<Type> = vec![];

                if let Some(ty) = yield_type {
                    let Type::Function { params, ret } = &ty else {
                        self.push_error(TypeCheckError::UnexpectedClosure { expected: self.display_type(&ty), loc: *loc });
                        return Expr::Errored { loc: *loc };
                    };
                    expected_return = Some(ret.as_ref().clone());

                    if parameters.len() != params.len() {
                        self.push_error(TypeCheckError::ClosureWithWrongParameters { expected: self.display_type(&ty), got: parameters.len(), loc: *loc});
                        return Expr::Errored { loc: *loc };
                    }

                    for (param, expected_type) in parameters.iter().zip(params) {
                        if let Some(ty) = &param.typ {
                            parameter_types.push(self.resolve_type(ty));
                        } else {
                            parameter_types.push(expected_type.clone());
                        }
                    }
                } else {
                    for param in parameters {
                        if let Some(ty) = &param.typ {
                            parameter_types.push(self.resolve_type(ty));
                        } else {
                            self.push_error(TypeCheckError::CouldNotInferParameters(*loc));
                            return Expr::Errored { loc: *loc };
                        }
                    }
                    expected_return = None;
                }

                let mut child = ResolveContext {
                    namespace: closure_ns,
                    expected_return: expected_return.clone(),
                    checker: self.checker,
                    core: self.core,
                    level: self.level + 1,
                    return_types: vec![]
                };

                let mut resolved_parameters = vec![];
                for (parameter, parameter_type) in parameters.iter().zip(parameter_types) {
                    let key = child.add_name(parameter.name.clone(), NameInfo::Local { typ: parameter_type.clone(), loc: parameter.loc, level: child.level });
                    resolved_parameters.push(ClosureParameter { name: parameter.name.clone(), key, typ: parameter_type, loc: parameter.loc });
                }

                let resolved_body = child.resolve_expr(body, expected_return.clone());

                let ret_type: Type;
                let yield_type: Type;
                if let Some(return_type) = expected_return {
                    ret_type = return_type;
                    yield_type = self.type_of(&resolved_body);
                } else {
                    if !child.return_types.is_empty() {
                        panic!();
                    }
                    yield_type = self.type_of(&resolved_body);
                    ret_type = yield_type.clone();
                }

                let declared = self.checker.namespaces[closure_ns].get_names();
                let body = Block { stmts: vec![], trailing_expr: Some(Box::new(resolved_body)), yield_type, declared, loc: body.loc() };

                Expr::Closure { parameters: resolved_parameters, body, ret_type, loc: *loc }
            }
        };

        self.cast_to_maybe_type(to_type, resolved_expr)
    }

    fn resolve_stmt(&mut self, stmt: &ast::Stmt<'a>) -> Stmt<'a> {
        match stmt {
            ast::Stmt::Decl { name, typ, value, loc } => {
                match typ {
                    Some(t) => {
                        let typ = self.resolve_type(t);
                        let resolved_value = self.resolve_expr(value, Some(typ.clone()));
                        self.is_never(&resolved_value);
                        let decl = self.add_name(name.clone(), NameInfo::Local { typ, loc: *loc, level: self.level });
                        Stmt::Decl { decl, value: resolved_value, loc: *loc }
                    },
                    None => {
                        let resolved_value = self.resolve_expr(value, None);
                        self.is_never(&resolved_value);
                        let decl = self.add_name(name.clone(), NameInfo::Local { typ: self.type_of(&resolved_value), loc: *loc, level: self.level });
                        Stmt::Decl { decl, value: resolved_value, loc: *loc }
                    }
                }
            },
            ast::Stmt::Expr { expr, loc } => {
                Stmt::Expr { expr: self.resolve_expr(expr, None), loc: *loc }
            },
            ast::Stmt::Return { value, loc} => {
                let expected_return = self.expected_return.clone();
                let resolved_value = self.resolve_expr(value, expected_return);

                self.return_types.push(self.checker.hir.type_of_expr(&resolved_value));

                self.is_never(&resolved_value);
                Stmt::Return { value: resolved_value, loc: *loc }
                // let value = self.resolve_expr(value, None);
                // self.check(&self.checker.hir.type_of_expr(&value), Some(expected_return), loc);
                // Stmt::Return { value, loc: *loc }
            }
        }
    }
}


#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::path::Path;
    use crate::error::Message;
    use crate::parsing::ast;
    use crate::lowering::hir::{FunctionKey, HIR, StructKey, Type};
    use crate::lowering::type_check::{collect_fields, collect_function_bodies, collect_functions, collect_structs, initialize, resolve_types};
    use crate::source::Source;

    fn source(name: &str, text: &str) -> Source {
        Source::from_text(name, text.to_owned())
    }

    fn parse_one(s: &Source) -> ast::AST {
        let file = crate::parsing::parse_file(s).unwrap();
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

            fn main() -> i32 {
                return 0;
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

            fn main() -> i32 {
                return 0;
            }
        ");
        let ast = parse_one(&s);

        let initial = initialize(ast);
        let _ = collect_function_bodies(collect_functions(collect_fields(collect_structs(initial)))).unwrap();
    }

    #[test]
    fn test_if_else() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let a = if true {
                    return 0;
                } else {
                    2
                };

                a
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_if_else_2() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let a = if true {
                    2
                } else {
                    return 3;
                };

                a
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
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

            fn main() -> i32 {
                return 0;
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

            fn main() -> i32 {
                return 0;
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_resolve_types_6() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let closure = || 20;
                return closure();
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_resolve_types_7() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let closure = |a: i32| a;
                return closure(20);
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_resolve_types_8() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let closure: (i32) -> i32 = |a| a;
                return closure(20);
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_never_as_value_1() {
        let s = source("<test>", r"
            fn main() -> i32 {
                return { return 0; };
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_never_as_value_2() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let b = { return 0; };
                return b;
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Error: Values of type '!' cannot exist.")
        ));
    }

    #[test]
    fn test_never_as_value_3() {
        let s = source("<test>", r"
            fn main() -> i32 {
                if true {
                    return 0;
                } else {
                    return 2;
                }
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_never_as_value_4() {
        let s = source("<test>", r"
            fn other(i: i32) -> i32 {
                return i;
            }

            fn main() -> i32 {
                other({ return 2; })
            }
        ");
        let ast = parse_one(&s);

        resolve_types(ast).unwrap();
    }

    #[test]
    fn test_error_no_such_field() {
        let s = source("<test>", r"
            struct Thing {
                a: bool;
            }

            fn main() -> i32 {
                let t = new Thing { a: false };
                return t.foo;
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Error: 'Thing' Does not contain a field named 'foo'.")
        ));
    }

    #[test]
    fn test_error_no_such_name() {
        let s = source("<test>", r"
            fn main() -> i32 {
                return t;
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Error: Could not resolve the name 't'.")
        ));
    }

    #[test]
    fn test_error_no_such_type() {
        let s = source("<test>", r"
            fn other() -> Thing {

            }

            fn main() -> i32 {
                return 0;
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Error: Could not resolve the type 'Thing'.")
        ));
    }

    #[test]
    fn test_error_too_many_arguments() {
        let s = source("<test>", r"
            fn other() -> i32 {
                return 0;
            }

            fn main() -> i32 {
                return other(100);
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Error: Expected 0 arguments, got 1 argument.")
        ));
    }

    #[test]
    fn test_error_too_few_arguments() {
        let s = source("<test>", r"
            fn other(i: i32) -> i32 {
                return i;
            }

            fn main() -> i32 {
                return other();
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Error: Expected 1 argument, got 0 arguments.")
        ));
    }

    #[test]
    fn test_error_closure_could_not_infer() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let thing = |a| a;
                return thing(20);
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Error: Could not infer the type of the parameters of the closure.")
        ));
    }

    #[test]
    fn test_error_closure_bad_infer() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let thing: () -> i32 = |a| a;
                return thing(20);
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Expected a closure of type '() -> i32', but the closure has 1 parameter.")
        ));
    }

    #[test]
    fn test_error_mismatched_if_else() {
        let s = source("<test>", r"
            fn main() -> i32 {
                let thing = if true { 32 } else { false };
                return thing;
            }
        ");
        let ast = parse_one(&s);

        assert!(resolve_types(ast).is_err_and(|e|
            e.render_to_string().contains("Error: Incompatible types. Expected 'i32' but got 'bool'.")
        ));
    }
}
