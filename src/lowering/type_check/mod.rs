mod collect_struct_prototypes;
mod collect_functions;
mod collect_function_bodies;
mod collect_structs;
mod collect_types;

use std::collections::HashMap;
use std::fmt;
use slotmap::{new_key_type, SlotMap};
use crate::parsing::ast;
use crate::util::{KeyMap, map_join, pluralize};
use crate::error::Message;
use crate::lowering::hir;
use crate::lowering::hir::{Type, NameKey, StructKey, FunctionKey, TypeParameterKey, InferenceVariableKey, NameInfo, TypeParameterInfo, InferenceVariableInfo, FunctionType, StructType, MethodKey};
use crate::source::Location;

use crate::lowering::type_check::collect_structs::CollectedTypes;
use crate::lowering::type_check::collect_struct_prototypes::CollectedStructPrototypes;
use crate::lowering::type_check::collect_functions::CollectedPrototypes;


pub fn resolve_types(ast: ast::AST) -> Result<hir::HIR, Vec<TypeCheckError>> {
    let mut checker = TypeCheck::new(ast.name.clone());
    let root = checker.root();
    checker.add_type(root, "i32".to_owned(), Type::SignedInteger(32));
    checker.add_type(root, "i64".to_owned(), Type::SignedInteger(64));
    checker.add_type(root, "bool".to_owned(), Type::Boolean);

    let collected_struct_prototypes = collect_struct_prototypes::collect_struct_prototypes(&mut checker, &ast);
    let collected_structs = collect_structs::collect_structs(&mut checker, collected_struct_prototypes);
    let collected_types = collect_types::collect_types(&mut checker, collected_structs);
    let collected_functions = collect_functions::collect_functions(&mut checker, collected_types);
    let collected_bodies = collect_function_bodies::collect_function_bodies(checker, collected_functions);
    collected_bodies.into_result()
}

#[derive(Debug, Eq, PartialEq)]
pub enum DisplayType<'a> {
    Unit,
    Never,
    Boolean,
    Errored,
    Integer { is_signed: bool, bits: u8 },
    Struct { name: String, variant: Vec<DisplayType<'a>>, loc: Location<'a> },
    Function { params: Vec<DisplayType<'a>>, ret: Box<DisplayType<'a>> },
    TypeParameter { name: String, bound: Option<Box<DisplayType<'a>>> },
    InferenceVariable { name: String, inferred: Option<Box<DisplayType<'a>>> }
}

impl DisplayType<'_> {
    fn render(&self) -> String {
        match self {
            DisplayType::Unit => "()".into(),
            DisplayType::Never => "!".into(),
            DisplayType::Boolean => "bool".into(),
            DisplayType::Errored => "<could not resolve>".into(),
            DisplayType::Integer { is_signed, bits } => if *is_signed { format!("i{bits}") } else { format!("u{bits}") },
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
            DisplayType::InferenceVariable { name, inferred } => {
                if let Some(inferred) = inferred {
                    format!("{name}: {}", inferred.render())
                } else {
                    format!("{name}")
                }
            },
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
    CannotUseGenericFunction { name: String, loc: Location<'a> },
    ExpectedFunction { got: DisplayType<'a>, loc: Location<'a> },
    UnexpectedClosure { expected: DisplayType<'a>, loc: Location<'a> },
    ClosureWithWrongParameters { expected: DisplayType<'a>, got: usize, loc: Location<'a> },
    MismatchedArguments { expected: usize, got: usize, loc: Location<'a> },
    MismatchedTypeArguments { expected: usize, got: usize, loc: Location<'a> },
    RequiredTypeArguments { got: String, loc: Location<'a>, note_loc: Location<'a> },
    ExpectedStructName { got: String, loc: Location<'a> },
    ExpectedStruct { got: DisplayType<'a>, loc: Location<'a> },
    NoSuchFieldName { field: String, typ: String, loc: Location<'a> },
    NoSuchMethodName { on_type: DisplayType<'a>, method: String, loc: Location<'a> },
    ConflictingMethods { on_type: DisplayType<'a>, method: String, loc: Location<'a>, possible: Vec<Location<'a>> },
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
            TypeCheckError::CannotUseGenericFunction { name, loc } => {
                writeln!(to, "Error: Attempted to load generic function '{name}'.")?;
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
                writeln!(to, "Error: '{}' does not contain a field named '{}'.", typ, field)?;
                Self::show_location(loc, to)
            }
            TypeCheckError::NoSuchMethodName { on_type, method, loc } => {
                writeln!(to, "Error: '{}' does not have a method named '{}'.", on_type.render(), method)?;
                Self::show_location(loc, to)
            }
            TypeCheckError::ConflictingMethods { on_type, method, loc, .. } => {
                writeln!(to, "Error: Method '{}' is ambiguous for '{}'.", method, on_type.render())?;
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


pub enum ResolveResult<'a, T> {
    Success(T),
    Failure(Vec<TypeCheckError<'a>>)
}

impl<'a, T> ResolveResult<'a, T> {
    fn collect_results<O: Default + Extend<T>>(iter: impl IntoIterator<Item=ResolveResult<'a, T>>) -> ResolveResult<'a, O> {
        let mut items = O::default();
        let mut errors = Vec::new();
        for item in iter {
            match item {
                ResolveResult::Success(t) => items.extend([t]),
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

    fn into_result(self) -> Result<T, Vec<TypeCheckError<'a>>> {
        match self {
            ResolveResult::Success(t) => Ok(t),
            ResolveResult::Failure(errs) => Err(errs)
        }
    }

    fn is_failure(&self) -> bool {
        match self {
            ResolveResult::Success(_) => false,
            ResolveResult::Failure(_) => true
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
            let mut errors: std::vec::Vec<$crate::lowering::type_check::TypeCheckError<'_>> = std::vec::Vec::new();
            $(
                if let $crate::lowering::type_check::ResolveResult::Failure(errs) = $res {
                    errors.extend(errs);
                }
            )*

            errors
        }
    };
}
pub(crate) use combine_errors;


struct TypeCheck<'a> {
    name: String,
    namespaces: SlotMap<NamespaceKey, Namespace>,
    root: NamespaceKey,

    errors: Vec<TypeCheckError<'a>>,

    type_parameters: KeyMap<TypeParameterKey, TypeParameterInfo<'a>>,
    names: KeyMap<NameKey, NameInfo<'a>>,
    inference_variables: KeyMap<InferenceVariableKey, InferenceVariableInfo<'a>>
}

impl<'a> TypeCheck<'a> {
    pub fn new(name: String) -> TypeCheck<'a> {
        let mut namespaces = SlotMap::with_key();
        let root = namespaces.insert(Namespace::new(None));
        TypeCheck {
            name,
            namespaces, root,
            errors: Default::default(),
            names: KeyMap::new(),
            type_parameters: KeyMap::new(),
            inference_variables: KeyMap::new()
        }
    }

    pub fn root(&self) -> NamespaceKey {
        self.root
    }

    pub fn add_inference_variable(&mut self, name: impl Into<String>, loc: Location<'a>) -> InferenceVariableKey {
        self.inference_variables.add(InferenceVariableInfo { ty: None, name: name.into(), loc })
    }

    pub fn add_type_param(&mut self, name: impl Into<String>, loc: Location<'a>) -> TypeParameterKey {
        self.type_parameters.add(TypeParameterInfo { name: name.into(), loc })
    }

    pub fn add_type(&mut self, ns: NamespaceKey, name: String, typ: Type) {
        // todo return if there's a previous value
        self.namespaces[ns].types.insert(name, typ);
    }

    #[must_use]
    pub fn add_local(&mut self, name: impl Into<String>, ty: Type, level: usize, loc: Location<'a>, ns: NamespaceKey) -> (NameKey, Option<NameKey>) {
        let name = name.into();
        let info = NameInfo::Local { name: name.clone(), ty, level, loc };
        let key = self.names.add(info);
        (key, self.namespaces[ns].names.insert(name, key))
    }

    #[must_use]
    pub fn add_function(&mut self, name: impl Into<String>, key: FunctionKey, loc: Location<'a>, ns: NamespaceKey) -> (NameKey, Option<NameKey>) {
        let name = name.into();
        let info = NameInfo::Function { name: name.clone(), key, loc };
        let key = self.names.add(info);
        (key, self.namespaces[ns].names.insert(name, key))
    }

    pub fn add_struct(&mut self, name: impl Into<String>, key: StructKey, ns: NamespaceKey) -> Option<StructKey> {
        self.namespaces[ns].structs.insert(name.into(), key)
    }

    pub fn add_namespace(&mut self, parent: Option<NamespaceKey>) -> NamespaceKey {
        self.namespaces.insert(Namespace::new(parent))
    }

    pub fn query_inference_variable<'s>(&'s self, key: InferenceVariableKey) -> &'s InferenceVariableInfo<'a> {
        &self.inference_variables[key]
    }

    pub fn query_inference_variable_mut<'s>(&'s mut self, key: InferenceVariableKey) -> &'s mut InferenceVariableInfo<'a> {
        &mut self.inference_variables[key]
    }

    pub fn resolve_struct(&self, name: impl AsRef<str>, ns: NamespaceKey) -> Option<StructKey> {
        if let Some(&s) = self.namespaces[ns].structs.get(name.as_ref()) {
            return Some(s);
        };
        if let Some(parent) = self.namespaces[ns].parent {
            return self.resolve_struct(name, parent);
        } else {
            return None;
        }
    }

    pub fn resolve_type<'b>(&'b self, ty: &'b ast::Type<'a>, in_ns: NamespaceKey, ctxt: &'b CollectedTypes<'a, 'b>) -> ResolveResult<'a, Type> {
        match ty {
            ast::Type::Name { name, loc } => {
                if let Some(ty) = self.namespaces[in_ns].types.get(name) {
                    return ResolveResult::Success(ty.clone());
                }
                if let Some(&t) = self.namespaces[in_ns].structs.get(name) {
                    if !ctxt.structs[t].type_parameters.is_empty() {
                        return ResolveResult::Failure(vec![TypeCheckError::RequiredTypeArguments { got: ctxt.structs[t].name.clone(), loc: *loc, note_loc: ctxt.structs[t].ast_struct.loc }]);
                    }
                    return ResolveResult::Success(StructType(t, vec![]).into());
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
                        return ResolveResult::Success(FunctionType(parameters, Box::new(ret)).into())
                    },
                    (parameters, ret) => {
                        return ResolveResult::Failure(combine_errors!(parameters, ret));
                    }
                }
            }
            ast::Type::Generic { name, type_args, loc } => {
                let key_result = self.resolve_struct(name, in_ns);

                let type_args_result: ResolveResult<'a, Vec<Type>> = ResolveResult::collect_results(type_args.iter().map(|arg| self.resolve_type(arg, in_ns, ctxt)));

                if let (&Some(key), ResolveResult::Success(type_args)) = (&key_result, &type_args_result) {
                    let type_params = &ctxt.structs[key].type_parameters;
                    if type_params.len() != type_args.len() {
                        return ResolveResult::Failure(vec![TypeCheckError::MismatchedTypeArguments { expected: type_params.len(), got: type_args.len(), loc: *loc }]);
                    }
                    return ResolveResult::Success(StructType(key, type_args.clone()).into());
                } else {
                    let key_result: ResolveResult<'a, Type> = ResolveResult::Failure(vec![TypeCheckError::ExpectedStructName { got: name.clone(), loc: *loc }]);
                    return ResolveResult::Failure(combine_errors!(key_result, type_args_result));
                }
            }
        }
    }

    pub fn subs(&self, ty: &Type, map: &HashMap<TypeParameterKey, Type>) -> Type {
        match ty {
            Type::Never => Type::Never,
            Type::Unit => Type::Unit,
            Type::Boolean => Type::Boolean,
            Type::Errored => Type::Errored,
            Type::SignedInteger(bits) => Type::SignedInteger(*bits),
            Type::UnsignedInteger(bits) => Type::UnsignedInteger(*bits),
            Type::TypeParameter(key) => {
                if let Some(ty) = map.get(key) {
                    self.subs(ty, map)
                } else {
                    Type::TypeParameter(*key)
                }
            },
            Type::InferenceVariable(key) => {
                if let Some(ty) = &self.inference_variables[*key].ty {
                    self.subs(ty, map)
                } else {
                    Type::InferenceVariable(*key)
                }
            }
            Type::Function(FunctionType(params, ret)) => {
                FunctionType(params.iter().map(|t| self.subs(t, map)).collect(), Box::new(self.subs(ret, map))).into()
            },
            Type::Struct(StructType(struct_, variant)) => {
                StructType(*struct_, variant.iter().map(|t| self.subs(t, map)).collect()).into()
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
    use crate::error::Message;
    use crate::parsing::ast;
    use crate::lowering::type_check::resolve_types;
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

        resolve_types(ast).unwrap();
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

        let resolved = resolve_types(ast);


        assert!(resolved.is_err_and(|e|
            e.render_to_string().contains("Error: Incompatible types. Expected 'i32' but got 'bool'.")
        ));
    }
}
