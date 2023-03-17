use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use slotmap::{new_key_type, SlotMap};
use crate::ast;
use crate::hir::{HIR, NameKey, NameInfo, TypeKey, Struct, StructKey, Type, StructField, FunctionKey, Parameter, FunctionPrototype, FunctionBody, Expr, LogicOp};
use crate::source::Location;

#[derive(Debug, Eq, PartialEq)]
enum TypeCheckError<'a> {
    NameDuplicated(String, Location<'a>),
    CouldNotResolveName(String, Location<'a>),
    CouldNotResolveType(String, Location<'a>),
    IncompatibleTypes(Type, Type, Location<'a>),
    NotEnoughInfoToInfer(Location<'a>)
}

new_key_type! {
    struct NamespaceKey;
}

#[derive(Default)]
struct Namespace {
    parent: Option<NamespaceKey>,
    names: HashMap<String, NameKey>,
    types: HashMap<String, Type>,
    namespaces: HashMap<String, NamespaceKey>
}

impl Namespace {
    fn new(parent: Option<NamespaceKey>) -> Namespace {
        Namespace {
            parent, ..Default::default()
        }
    }
}

enum FilePaths {
    File(PathBuf),
    Folder(HashMap<String, Box<FilePaths>>)
}

struct TypeCheck<'a> {
    root: NamespaceKey,
    namespaces: SlotMap<NamespaceKey, Namespace>,
    names: SlotMap<NameKey, NameInfo>,
    errors: RefCell<Vec<TypeCheckError<'a>>>,
}

impl<'a> TypeCheck<'a> {
    fn new() -> TypeCheck<'a> {
        let mut namespaces = SlotMap::with_key();
        let root = namespaces.insert(Namespace::new(None));
        TypeCheck { root, namespaces, names: SlotMap::with_key(), errors: Default::default() }
    }

    fn get_type(&self, ns: NamespaceKey, name: &str) -> Option<Type> {
        self.namespaces[ns].types.get(name).cloned()
    }

    fn add_type(&mut self, ns: NamespaceKey, name: String, typ: Type) {
        self.namespaces[ns].types.insert(name, typ);
    }

    fn add_name(&mut self, ns: NamespaceKey, name: String, info: NameInfo) -> NameKey {
        let key = self.names.insert(info);
        self.namespaces[ns].names.insert(name, key);
        key
    }

    fn get_struct_key(&self, file: NamespaceKey, name: &str) -> StructKey {
        match self.namespaces[file].types[name] {
            Type::Struct { struct_ } => struct_,
            _ => panic!("not a struct")
        }
    }

    fn get_function_key(&self, file: NamespaceKey, name: &str) -> FunctionKey {
        match self.names[self.namespaces[file].names[name]] {
            NameInfo::Function { func } => func,
            _ => panic!("not a function")
        }
    }

    fn resolve_type(&self, ns: NamespaceKey, typ: &ast::Type<'a>) -> Type {
        match typ {
            ast::Type::Name { name, loc } => {
                if let Some(t) = self.get_type(ns, name) {
                    return t;
                }
                if let Some(parent) = self.namespaces[ns].parent {
                    return self.resolve_type(parent, typ);
                }
                self.push_error(TypeCheckError::CouldNotResolveType(name.clone(), *loc));
                Type::Errored
            }
        }
    }

    fn push_error(&self, err: TypeCheckError<'a>) {
        self.errors.borrow_mut().push(err);
    }
}

struct Initial<'a> {
    checker: TypeCheck<'a>,

    files: HashMap<PathBuf, ast::File<'a>>
}

impl Initial<'_> {
    fn new(ast: ast::AST) -> Initial {
        let mut checker = TypeCheck::new();
        checker.add_type(checker.root, "i32".to_owned(), Type::Integer { bits: 32 });
        checker.add_type(checker.root, "i64".to_owned(), Type::Integer { bits: 64 });
        checker.add_type(checker.root, "bool".to_owned(), Type::Boolean);
        Initial { checker, files: ast.files }
    }
}

struct CollectedStructs<'a> {
    checker: TypeCheck<'a>,

    file_namespaces: HashMap<PathBuf, NamespaceKey>,

    files: HashMap<PathBuf, ast::File<'a>>,
    structs: SlotMap<StructKey, Struct<'a>>,
}

struct CollectedFields<'a> {
    checker: TypeCheck<'a>,

    file_namespaces: HashMap<PathBuf, NamespaceKey>,

    files: HashMap<PathBuf, ast::File<'a>>,
    structs: SlotMap<StructKey, Struct<'a>>,
}

struct CollectedFunctions<'a> {
    checker: TypeCheck<'a>,

    file_namespaces: HashMap<PathBuf, NamespaceKey>,

    files: HashMap<PathBuf, ast::File<'a>>,
    structs: SlotMap<StructKey, Struct<'a>>,

    function_signatures: SlotMap<FunctionKey, FunctionPrototype<'a>>
}

fn collect_structs(initial: Initial) -> CollectedStructs {
    let Initial { mut checker, files } = initial;
    let mut structs = SlotMap::with_key();
    let mut file_namespaces = HashMap::new();
    
    for (file_path, file) in &files {
        let file_ns = checker.namespaces.insert(Namespace::new(Some(checker.root)));
        file_namespaces.insert(file_path.clone(), file_ns);
        for ast::Struct { name, .. } in file.iter_structs() {
            // if common.get_type(file_ns, &name).is_some() {
            //     common.push_error(TypeCheckError::CouldNotResolveName(name.clone(), ))
            // }
            let struct_key = structs.insert(Struct { name: name.clone(), fields: HashMap::new() });
            checker.add_type(file_ns, name.clone(), Type::Struct { struct_: struct_key });
        }
    }

    CollectedStructs {
        checker, file_namespaces, files, structs
    }
}

fn collect_fields(collected: CollectedStructs) -> CollectedFields {
    let CollectedStructs {
        checker, files,
        file_namespaces, mut structs
    } = collected;

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];
        for ast::Struct { name, items} in file.iter_structs() {
            let key = checker.get_struct_key(file_ns, name);
            let struct_ = &mut structs[key];
            for item in items {
                if let ast::StructItem::Field { name, typ, loc } = item {
                    let resolved = checker.resolve_type(file_ns, typ);
                    struct_.fields.insert(name.clone(), StructField { name: name.clone(), typ: resolved, loc: *loc });
                }
            }
        }
    };

    CollectedFields {
        checker, file_namespaces, files, structs
    }
}


fn collect_functions(collected: CollectedFields) -> CollectedFunctions {
    let CollectedFields {
        mut checker, files,
        file_namespaces, structs
    } = collected;
    let mut functions = SlotMap::with_key();

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];
        for func in file.iter_functions() {
            let mut params = Vec::new();
            for param in &func.parameters {
                let typ = checker.resolve_type(file_ns, &param.typ);
                params.push(Parameter { name: param.name.clone(), typ, loc: param.loc})
            }
            let ret = func.return_type.as_ref().map_or(Type::Unit, |t| checker.resolve_type(file_ns, t));
            let sig = Type::Function { params: params.iter().map(|p| p.typ.clone()).collect(), ret: Box::new(ret.clone()) };
            let key = functions.insert(FunctionPrototype { name: func.name.clone(), params, ret, sig });
            checker.add_name(file_ns, func.name.clone(), NameInfo::Function { func: key });
        }
    }

    CollectedFunctions { checker, files, file_namespaces, structs, function_signatures: functions }
}


#[cfg(test)]
mod test {
    use std::path::Path;
    use crate::ast;
    use crate::hir::{Struct, Type};
    use crate::source::Source;
    use crate::type_check::{collect_fields, collect_functions, collect_structs, Initial};

    fn parse(s: &Source) -> ast::AST {
        crate::parser::parse(s).unwrap()
    }

    #[test]
    fn verify_collect_structs() {
        let s = Source::from_text("<test>", r"
            struct Alpha { }

            struct Beta { }
        ");
        let ast = parse(&s);

        let initial = Initial::new(ast);
        let collected = collect_structs(initial);

        let structs: Vec<&Struct> = collected.structs.values().collect();

        let expected_names = vec!["Alpha", "Beta"];
        assert!(expected_names.iter().all(|&name| structs.iter().any(|s| s.name == name)));
    }

    #[test]
    fn verify_collect_fields() {
        let s = Source::from_text("<test>", r"
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
        let ast = parse(&s);

        let initial = Initial::new(ast);
        let mut collected = collect_fields(collect_structs(initial));

        assert_eq!(collected.checker.errors.get_mut(), &vec![]);

        let file = collected.file_namespaces[Path::new("<test>")];

        let alpha_key = collected.checker.get_struct_key(file, "Alpha");
        let beta_key = collected.checker.get_struct_key(file, "Beta");

        let alpha = &collected.structs[alpha_key];
        assert_eq!(alpha.fields.len(), 3);
        assert_eq!(alpha.fields["x"].typ, Type::Integer { bits: 32 });
        assert_eq!(alpha.fields["y"].typ, Type::Struct { struct_: alpha_key });
        assert_eq!(alpha.fields["z"].typ, Type::Struct { struct_: beta_key });

        let beta = &collected.structs[beta_key];

        assert_eq!(beta.fields.len(), 2);
        assert_eq!(beta.fields["x"].typ, Type::Boolean);
        assert_eq!(beta.fields["z"].typ, Type::Struct { struct_: beta_key });
    }

    #[test]
    fn verify_collect_functions() {
        let s = Source::from_text("<test>", r"
            struct Alpha { }

            struct Beta { }

            fn aleph(a: i32) -> Alpha { }

            fn bet(b: Beta, c: Gamma) -> bool { }

            struct Gamma { }
        ");
        let ast = parse(&s);

        let initial = Initial::new(ast);
        let mut collected = collect_functions(collect_fields(collect_structs(initial)));

        assert_eq!(collected.checker.errors.get_mut(), &vec![]);

        let funcs: Vec<_> = collected.function_signatures.values().collect();

        let file = collected.file_namespaces[Path::new("<test>")];

        let alpha_key = collected.checker.get_struct_key(file, "Alpha");
        let beta_key = collected.checker.get_struct_key(file, "Beta");
        let gamma_key = collected.checker.get_struct_key(file, "Gamma");
        let aleph_key = collected.checker.get_function_key(file, "aleph");
        let bet_key = collected.checker.get_function_key(file, "bet");

        let aleph = &collected.function_signatures[aleph_key];
        let Type::Function { params, ret} = &aleph.sig else { panic!("{:?}", &aleph.sig) };
        assert_eq!(ret.as_ref(), &Type::Struct { struct_: alpha_key });
        assert_eq!(params, &vec![Type::Integer { bits: 32 }]);

        let bet = &collected.function_signatures[bet_key];
        let Type::Function { params, ret} = &bet.sig else { panic!("{:?}", &bet.sig) };
        assert_eq!(ret.as_ref(), &Type::Boolean);
        assert_eq!(params, &vec![Type::Struct { struct_: beta_key }, Type::Struct { struct_: gamma_key }]);
    }
}
