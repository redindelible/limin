mod collect_structs;
mod collect_functions;
mod collect_function_bodies;
mod collect_types;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use indexmap::IndexMap;
use slotmap::{new_key_type, SlotMap};
use crate::parsing::ast;
use crate::util::{map_join, pluralize};
use crate::error::Message;
use crate::lowering::hir;
use crate::source::Location;

use crate::lowering::type_check::collect_types::CollectedTypes;
use crate::lowering::type_check::collect_structs::CollectedStructs;
use crate::lowering::type_check::collect_functions::CollectedPrototypes;


pub fn resolve_types(ast: ast::AST) -> Result<hir::HIR, Vec<TypeCheckError>> {
    let mut checker = TypeCheck::new(ast.name.clone());
    let root = checker.root();
    checker.add_type(root, "i32".to_owned(), Type::SignedInteger(32));
    checker.add_type(root, "i64".to_owned(), Type::SignedInteger(32));
    checker.add_type(root, "bool".to_owned(), Type::Boolean);

    let collected_structs = collect_structs::collect_structs(&mut checker, &ast);
    let collected_fields = collect_types::collect_types(&mut checker,collected_structs);
    let collected_function = collect_functions::collect_functions(&mut checker, collected_fields);
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
    NameDuplicated(String, Location<'a>, Location<'a>),
    StructDuplicated(String, Location<'a>, Location<'a>),
    FieldDuplicated(String, String, Location<'a>, Location<'a>),
    MethodDuplicated(String, String, Location<'a>, Location<'a>),
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
            TypeCheckError::NameDuplicated(name, loc, prev_loc) => {
                writeln!(to, "Error: A name called '{name}' was already defined in this scope.")?;
                Self::show_location(loc, to)?;
                writeln!(to, " | Note: A name called '{name}' was previously defined here.")?;
                Self::show_note_location(prev_loc, to)
            }
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
            TypeCheckError::MethodDuplicated(name, struct_name, loc, prev_loc) => {
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

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct StructKey(usize);

impl StructKey {
    #[must_use]
    fn next(&self) -> StructKey {
        StructKey(self.0 + 1)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct TypeParameterKey(usize);

impl TypeParameterKey {
    #[must_use]
    fn next(&self) -> TypeParameterKey {
        TypeParameterKey(self.0 + 1)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct FunctionKey(usize);

impl FunctionKey {
    #[must_use]
    fn next(&self) -> FunctionKey {
        FunctionKey(self.0 + 1)
    }
}

new_key_type! {
    pub struct NamespaceKey;
    pub struct NameKey;
}

struct Namespace {
    parent: Option<NamespaceKey>,
    names: HashMap<String, NameKey>,
    types: HashMap<String, Type>,
    structs: HashMap<String, StructKey>,
    namespaces: HashMap<String, NamespaceKey>,
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

#[derive(Clone)]
enum Type {
    TypeParameter(TypeParameterKey),
    Boolean,
    SignedInteger(u8),
    UnsignedInteger(u8),
    Struct(StructKey, Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Unit,
    Errored
}

// #[derive(Clone)]
// enum UncheckedType {
//     TypeParameter(TypeParameterKey),
//     Boolean,
//     SignedInteger(u8),
//     UnsignedInteger(u8),
//     Struct(StructKey, Vec<UncheckedType>),
//     Function(Vec<UncheckedType>, Box<UncheckedType>),
//     Unit,
//     Errored
// }
//
// impl UncheckedType {
//     fn from_type(ty: &Type) -> UncheckedType {
//         match ty {
//             Type::TypeParameter(key) => UncheckedType::TypeParameter(*key),
//             Type::Boolean => UncheckedType::Boolean,
//             Type::SignedInteger(bits) => UncheckedType::SignedInteger(*bits),
//             Type::UnsignedInteger(bits) => UncheckedType::UnsignedInteger(*bits),
//             Type::Struct(key, variant) => {
//                 UncheckedType::Struct(*key, variant.iter().map(|ty| UncheckedType::from_type(ty)).collect())
//             }
//             Type::Function(params, ret) => {
//                 UncheckedType::Function(params.iter().map(|ty| UncheckedType::from_type(ty)).collect(),
//                 Box::new(UncheckedType::from_type(ret)))
//             }
//             Type::Unit => UncheckedType::Unit,
//             Type::Errored => UncheckedType::Errored
//         }
//     }
// }

pub enum ResolveResult<'a, T> {
    Success(T),
    Failure(Vec<TypeCheckError<'a>>)
}

impl<'a, T> ResolveResult<'a, T> {
    fn collect_results<I: IntoIterator<Item=ResolveResult<'a, T>>>(iter: I) -> ResolveResult<'a, Vec<T>> {
        let mut items = Vec::new();
        let mut errors = Vec::new();
        for item in iter {
            match item {
                ResolveResult::Success(t) => items.push(t),
                ResolveResult::Failure(errs) => errors.extend(errs)
            }
        }
        if errors.is_empty() {
            ResolveResult::Success(items)
        } else {
            ResolveResult::Failure(errors)
        }
    }

    fn ok(self, checker: &mut TypeCheck<'a>) -> Option<T> {
        match self {
            ResolveResult::Success(t) => Some(t),
            ResolveResult::Failure(errs) => {
                for error in errs {
                    checker.push_error(error);
                }
                None
            }
        }
    }
}

impl<'a> ResolveResult<'a, Type> {
    fn expect_type(self, checker: &mut TypeCheck<'a>) -> Type {
        match self {
            ResolveResult::Success(ty) => ty,
            ResolveResult::Failure(errors) => {
                for error in errors {
                    checker.push_error(error)
                }
                Type::Errored
            }
        }
    }
}


macro_rules! combine_errors {
    ($($res:expr),*) => {
        {
            let mut errors: Vec<TypeCheckError<'_>> = Vec::new();
            $(
                if let ResolveResult::Failure(errs) = $res {
                    errors.extend(errs);
                }
            )*

            errors
        }
    };
}


pub enum NameInfo<'a> {
    Local {
        name: String,
        ty: Type,
        loc: Location<'a>
    },
    Function {
        name: String,
        key: FunctionKey,
        params: Vec<Type>,
        ret: Type,
        loc: Location<'a>
    }
}

impl<'a> NameInfo<'a> {
    pub fn name(&self) -> &String {
        match self {
            NameInfo::Local { name, .. } => name,
            NameInfo::Function { name, .. } => name
        }
    }

    pub fn loc(&self) -> Location<'a> {
        match self {
            NameInfo::Local { loc, .. } => *loc,
            NameInfo::Function { loc, .. } => *loc
        }
    }
}

struct TypeCheck<'a> {
    namespaces: SlotMap<NamespaceKey, Namespace>,
    root: NamespaceKey,

    errors: Vec<TypeCheckError<'a>>,
    type_parameter_key: TypeParameterKey,
    type_parameter_ctxt: Vec<HashMap<TypeParameterKey, Type>>,

    names: SlotMap<NameKey, NameInfo<'a>>
}

impl<'a> TypeCheck<'a> {
    pub fn new(name: String) -> TypeCheck<'a> {
        let mut namespaces = SlotMap::with_key();
        let root = namespaces.insert(Namespace::new(None));
        TypeCheck {
            namespaces, root,
            errors: Default::default(),
            type_parameter_key: TypeParameterKey::default(), type_parameter_ctxt: vec![],
            names: SlotMap::with_key()
        }
    }

    pub fn root(&self) -> NamespaceKey {
        self.root
    }

    pub fn add_type_param(&mut self) -> TypeParameterKey {
        let key = self.type_parameter_key;
        self.type_parameter_key = self.type_parameter_key.next();
        key
    }

    pub fn add_type(&mut self, ns: NamespaceKey, name: String, typ: Type) {
        // todo return if there's a previous value
        self.namespaces[ns].types.insert(name, typ);
    }

    #[must_use]
    pub fn add_local(&mut self, name: impl Into<String>, ty: Type, loc: Location<'a>, ns: NamespaceKey) -> (NameKey, Option<NameKey>) {
        let name = name.into();
        let info = NameInfo::Local { name: name.clone(), ty, loc };
        let key = self.names.insert(info);
        (key, self.namespaces[ns].names.insert(name, key))
    }

    #[must_use]
    pub fn add_function(&mut self, name: impl Into<String>, key: FunctionKey, params: Vec<Type>, ret: Type, loc: Location<'a>, ns: NamespaceKey) -> (NameKey, Option<NameKey>) {
        let name = name.into();
        let info = NameInfo::Function { name: name.clone(), key, params, ret, loc };
        let key = self.names.insert(info);
        (key, self.namespaces[ns].names.insert(name, key))
    }

    pub fn add_struct(&mut self, name: impl Into<String>, key: StructKey, ns: NamespaceKey) -> Option<StructKey> {
        self.namespaces[ns].structs.insert(name.into(), key)
    }

    pub fn add_namespace(&mut self, parent: Option<NamespaceKey>) -> NamespaceKey {
        self.namespaces.insert(Namespace::new(parent))
    }

    pub fn get_name(&self, name: NameKey) -> &NameInfo {
        &self.names[name]
    }

    pub fn resolve_struct<'b>(&'b self, name: impl AsRef<str>, ns: NamespaceKey, loc: Location<'a>) -> ResolveResult<'a, StructKey> {
        if let Some(&s) = self.namespaces[ns].structs.get(name.as_ref()) {
            return ResolveResult::Success(s);
        };
        if let Some(parent) = self.namespaces[ns].parent {
            return self.resolve_struct(name, parent, loc);
        } else {
            return ResolveResult::Failure(vec![TypeCheckError::CouldNotResolveType(name.as_ref().to_owned(), loc)]);
        }
    }

    pub fn resolve_type<'b>(&'b self, ty: &'a ast::Type<'a>, in_ns: NamespaceKey, ctxt: &CollectedTypes) -> ResolveResult<'a, Type> {
        match ty {
            ast::Type::Name { name, loc } => {
                if let Some(ty) = self.namespaces[in_ns].types.get(name) {
                    return ResolveResult::Success(ty.clone());
                }
                if let Some(&t) = self.namespaces[in_ns].structs.get(name) {
                    if !ctxt.structs[&t].type_parameters.is_empty() {
                        return ResolveResult::Failure(vec![TypeCheckError::RequiredTypeArguments { got: ctxt.structs[&t].name.clone(), loc: *loc, note_loc: ctxt.structs[&t].ast_struct.loc }]);
                    }
                    return ResolveResult::Success(Type::Struct(t, vec![]));
                }
                if let Some(parent) = self.namespaces[in_ns].parent {
                    return self.resolve_type(ty, parent, ctxt);
                }
                return ResolveResult::Failure(vec![TypeCheckError::CouldNotResolveType(name.clone(), *loc)]);
            }
            ast::Type::Function { parameters, ret, .. } => {
                let parameters: ResolveResult<'a, Vec<Type>> = ResolveResult::collect_results(parameters.iter().map(|p| self.resolve_type(p, in_ns, ctxt)));
                let ret = self.resolve_type(ret, in_ns, ctxt);
                match (parameters, ret) {
                    (ResolveResult::Success(parameters), ResolveResult::Success(ret)) => {
                        return ResolveResult::Success(Type::Function(parameters, Box::new(ret)))
                    },
                    (parameters, ret) => {
                        return ResolveResult::Failure(combine_errors!(parameters, ret));
                    }
                }
            }
            ast::Type::Generic { name, type_args, loc } => {
                let key_result = self.resolve_struct(name, in_ns, *loc);

                let type_args_result: ResolveResult<'a, Vec<Type>> = ResolveResult::collect_results(type_args.iter().map(|arg| self.resolve_type(arg, in_ns, ctxt)));

                if let (ResolveResult::Success(key), ResolveResult::Success(type_args)) = (&key_result, &type_args_result) {
                    let type_params = &ctxt.structs[key].type_parameters;
                    if type_params.len() != type_args.len() {
                        return ResolveResult::Failure(vec![TypeCheckError::MismatchedTypeArguments { expected: type_params.len(), got: type_args.len(), loc: *loc }]);
                    }
                    return ResolveResult::Success(Type::Struct(*key, type_args.clone()));
                } else {
                    return ResolveResult::Failure(combine_errors!(key_result, type_args_result));
                }
            }
        }
    }

    pub fn push_error(&mut self, err: TypeCheckError<'a>) {
        self.errors.push(err);
    }
}


#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::path::Path;
    use crate::error::Message;
    use crate::parsing::ast;
    use crate::lowering::hir::{FunctionKey, HIR, StructKey, Type};
    use crate::lowering::type_check::{initialize, resolve_types};
    use crate::lowering::type_check::collect_types::collect_types;
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
        let collected = collect_types(collect_structs(initial));

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
        let collected = collect_functions(collect_types(collect_structs(initial)));

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
        let _ = collect_function_bodies(collect_functions(collect_types(collect_structs(initial)))).unwrap();
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
