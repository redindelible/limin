use std::collections::HashMap;
use std::path::PathBuf;
use indexmap::IndexMap;
use slotmap::SecondaryMap;
use crate::ast;
use crate::error::Message;
use crate::hir::{HIR, NameKey, NameInfo, Struct, StructKey, Type, StructField, FunctionKey, Parameter, FunctionPrototype, FunctionBody, Expr, LogicOp, Stmt, MayBreak};
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
    Struct { name: String, loc: Location<'a> },
    Function { params: Vec<DisplayType<'a>>, ret: Box<DisplayType<'a>> }
}

impl DisplayType<'_> {
    fn render(&self) -> String {
        match self {
            DisplayType::Unit => "()".into(),
            DisplayType::Never => "!".into(),
            DisplayType::Boolean => "bool".into(),
            DisplayType::Errored => "<could not resolve>".into(),
            DisplayType::Integer { bits } => format!("i{bits}"),
            DisplayType::Struct { name, .. } => format!("{name}"),
            DisplayType::Function { params, ret } => {
                let rendered: Vec<String> = params.iter().map(|t| t.render()).collect();
                format!("({}) -> {}", rendered.join(", "), ret.render())
            }
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
    NotEnoughInfoToInfer(Location<'a>),
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
            TypeCheckError::NotEnoughInfoToInfer(loc) => {
                eprintln!("Error: Could not infer type.");
                Self::show_location(loc);
            }
            TypeCheckError::NoMainFunction => {
                eprintln!("Error: No main function could be found.");
            }
        }
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
        pub namespaces: HashMap<String, NamespaceKey>,
        _private: (),
    }

    impl Namespace {
        fn new(parent: Option<NamespaceKey>) -> Namespace {
            Namespace {
                parent,
                names: HashMap::new(),
                types: HashMap::new(),
                namespaces: HashMap::new(),
                _private: ()
            }
        }

        pub fn get_names(&self) -> Vec<NameKey> {
            self.names.values().copied().collect()
        }
    }

    pub struct TypeCheck<'a> {
        pub namespaces: SlotMap<NamespaceKey, Namespace>,
        pub errors: RefCell<Vec<TypeCheckError<'a>>>,
        pub hir: HIR<'a>
    }

    impl<'a> TypeCheck<'a> {
        pub fn new(name: String) -> TypeCheck<'a> {
            TypeCheck { namespaces: SlotMap::with_key(), hir: HIR::new(name), errors: Default::default() }
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

        pub fn add_struct(&mut self, struct_: Struct<'a>) -> StructKey {
            self.hir.structs.insert(struct_)
        }

        pub fn add_function_proto(&mut self, proto: FunctionPrototype<'a>) -> FunctionKey {
            self.hir.function_prototypes.insert(proto)
        }

        pub fn get_struct_key(&self, file: NamespaceKey, name: &str) -> StructKey {
            match self.namespaces[file].types[name] {
                Type::Struct { struct_ } => struct_,
                _ => panic!("not a struct")
            }
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
}

struct CollectedFunctions<'a> {
    checker: TypeCheck<'a>,

    root: NamespaceKey,
    files: HashMap<PathBuf, ast::File<'a>>,
    file_namespaces: HashMap<PathBuf, NamespaceKey>,
    function_namespaces: SecondaryMap<FunctionKey, NamespaceKey>
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
            // if common.get_type(file_ns, &name).is_some() {
            //     common.push_error(TypeCheckError::CouldNotResolveName(name.clone(), ))
            // }
            let struct_key = checker.add_struct(Struct { name: name.clone(), fields: IndexMap::new(), loc: *loc });
            checker.add_type(file_ns, name.clone(), Type::Struct { struct_: struct_key });
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
            if let Some(parent) = checker.namespaces[ns].parent {
                return resolve_type(checker,parent, typ);
            }
            checker.push_error(TypeCheckError::CouldNotResolveType(name.clone(), *loc));
            Type::Errored
        }
    }
}

fn collect_fields(collected: CollectedStructs) -> CollectedFields {
    let CollectedStructs {
        mut checker, files, root,
        file_namespaces
    } = collected;

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];
        for ast::Struct { name, items, .. } in file.iter_structs() {
            let key = checker.get_struct_key(file_ns, name);
            for item in items {
                if let ast::StructItem::Field { name, typ, loc } = item {
                    let resolved = resolve_type(&checker, file_ns, typ);
                    checker.hir.structs[key].fields.insert(name.clone(), StructField { name: name.clone(), typ: resolved, loc: *loc });
                }
            }
        }
    };

    CollectedFields { checker, file_namespaces, files, root }
}

fn collect_functions(collected: CollectedFields) -> CollectedFunctions {
    let CollectedFields { mut checker, files, file_namespaces, root } = collected;

    let mut func_namespaces = SecondaryMap::new();

    for (file_path, file) in &files {
        let file_ns = file_namespaces[file_path];
        for func in file.iter_functions() {
            let func_ns = checker.add_namespace(Some(file_ns));

            let mut params = Vec::new();
            for param in &func.parameters {
                let typ = resolve_type(&checker,file_ns, &param.typ);
                let decl = checker.add_name(func_ns, param.name.clone(), NameInfo::Local { typ: typ.clone(), loc: param.loc });
                params.push(Parameter { name: param.name.clone(), typ, loc: param.loc, decl });
            }
            let ret = func.return_type.as_ref().map_or(Type::Unit, |t| resolve_type(&checker, file_ns, t));
            let sig = Type::Function { params: params.iter().map(|p| p.typ.clone()).collect(), ret: Box::new(ret.clone()) };

            let key = checker.hir.function_prototypes.insert_with_key(|key| {
                let decl = checker.hir.names.insert(NameInfo::Function { func: key });
                checker.namespaces[file_ns].names.insert(func.name.clone(), decl);
                FunctionPrototype { name: func.name.clone(), params, ret, sig, decl }
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

    CollectedFunctions { checker, files, file_namespaces, function_namespaces: func_namespaces, root }
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

            let mut resolver = ResolveContext::create(&mut checker, func_key, func_ns);

            let body = resolver.resolve_expr(&ast_func.body, Some(ret));
            let declared = checker.namespaces[func_ns].get_names();
            checker.hir.function_bodies.insert(func_key, FunctionBody { body, declared });
        }
    }

    checker.finalize()
}


type ExpectedType = Option<Type>;

struct ResolveContext<'a, 'b> where 'a: 'b {
    checker: &'b mut TypeCheck<'a>,
    func: FunctionKey,
    namespace: NamespaceKey,
}

impl<'a, 'b> ResolveContext<'a, 'b>  where 'a: 'b  {
    fn create(checker: &'b mut TypeCheck<'a>, func: FunctionKey, namespace: NamespaceKey) -> ResolveContext<'a, 'b> {
        ResolveContext {
            checker, func, namespace
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
            Type::Struct { struct_ } => {
                let s = &self.checker.hir.structs[*struct_];
                DisplayType::Struct {
                    name: s.name.clone(),
                    loc: s.loc
                }
            },
            Type::Function { params, ret } => DisplayType::Function {
                params: params.iter().map(|t| self.display_type(t)).collect(),
                ret: Box::new(self.display_type(ret))
            },
        }
    }

    fn check(&self, got_type: &Type, expected: ExpectedType, loc: &Location<'a>) -> bool {
        match expected {
            Some(Type::Errored) => false,
            None => true,
            Some(t) => {
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
            ast::Expr::Block { stmts, trailing_expr, loc } => {
                let block_ns = self.checker.add_namespace(Some(self.namespace));
                let mut child = ResolveContext {
                    namespace: block_ns,
                    func: self.func,
                    checker: self.checker
                };
                let stmts: Vec<_> = stmts.iter().map(|stmt| child.resolve_stmt(stmt)).collect();
                let always_breaks = stmts.iter().any(|stmt| stmt.does_break());

                let trailing_expr = match trailing_expr {
                    Some(e) => Some(Box::new(child.resolve_expr(e, yield_type))),
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
                Expr::Block { stmts, trailing_expr, declared, loc: *loc }
            }
            ast::Expr::Call { callee, arguments, loc } => {
                let resolved_callee = self.resolve_expr(callee, None);
                let callee_ty = self.checker.hir.type_of_expr(&resolved_callee);
                let Type::Function { params, ret } = &callee_ty else {
                    self.push_error(TypeCheckError::ExpectedFunction { got: self.display_type(&callee_ty), loc: callee.loc() });
                    return Expr::Errored { loc: *loc };
                };
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
            c => {
                panic!("{:?}", c);
            }
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
            }
        }
    }
}


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
        assert_eq!(alpha.fields["y"].typ, Type::Struct { struct_: alpha_key });
        assert_eq!(alpha.fields["z"].typ, Type::Struct { struct_: beta_key });

        let beta = &hir.structs[beta_key];

        assert_eq!(beta.fields.len(), 2);
        assert_eq!(beta.fields["x"].typ, Type::Boolean);
        assert_eq!(beta.fields["z"].typ, Type::Struct { struct_: beta_key });
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
        let Type::Function { params, ret} = &aleph.sig else { panic!("{:?}", &aleph.sig) };
        assert_eq!(ret.as_ref(), &Type::Struct { struct_: alpha_key });
        assert_eq!(params, &vec![Type::Integer { bits: 32 }]);

        let bet = &hir.function_prototypes[bet_key];
        let Type::Function { params, ret} = &bet.sig else { panic!("{:?}", &bet.sig) };
        assert_eq!(ret.as_ref(), &Type::Boolean);
        assert_eq!(params, &vec![Type::Struct { struct_: beta_key }, Type::Struct { struct_: gamma_key }]);
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
