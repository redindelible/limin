mod collect_structs;
mod collect_fields;
mod collect_functions;
mod collect_function_bodies;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use slotmap::{new_key_type, SlotMap};
use crate::parsing::ast;
use crate::util::{map_join, pluralize};
use crate::error::Message;
use crate::lowering::hir::*;
use crate::source::Location;

pub fn resolve_types(ast: ast::AST) -> Result<HIR, Vec<TypeCheckError>> {
    let initialized = initialize(ast);
    let collected_structs = collect_structs::collect_structs(initialized);
    let collected_fields = collect_fields::collect_fields(collected_structs);
    let collected_function = collect_functions::collect_functions(collected_fields);
    let collected_bodies = collect_function_bodies::collect_function_bodies(collected_function);
    collected_bodies
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


pub struct Initial<'a> {
    checker: TypeCheck<'a>,

    root: NamespaceKey,
    files: HashMap<PathBuf, ast::File<'a>>
}

fn initialize(ast: ast::AST) -> Initial {
    let mut checker = TypeCheck::new(ast.name);
    let root = checker.add_namespace(None);
    checker.add_type(root, "i32".to_owned(), Type::Integer { bits: 32 });
    checker.add_type(root, "i64".to_owned(), Type::Integer { bits: 64 });
    checker.add_type(root, "bool".to_owned(), Type::Boolean);
    Initial { root, checker, files: ast.files }
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


#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::path::Path;
    use crate::error::Message;
    use crate::parsing::ast;
    use crate::lowering::hir::{FunctionKey, HIR, StructKey, Type};
    use crate::lowering::type_check::{initialize, resolve_types};
    use crate::lowering::type_check::collect_fields::collect_fields;
    use crate::lowering::type_check::collect_function_bodies::collect_function_bodies;
    use crate::lowering::type_check::collect_functions::collect_functions;
    use crate::lowering::type_check::collect_structs::collect_structs;
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
