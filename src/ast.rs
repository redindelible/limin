use std::collections::HashMap;
use std::ops::{Deref, Index};

pub use crate::key_table::Key;
use crate::key_table::KeyTable;


pub struct AST {
    file_table: KeyTable<File>,
    function_table: KeyTable<Function>,
    function_parameter_table: KeyTable<FunctionParameter>,
    expr_table: KeyTable<Expr>,
    stmt_table: KeyTable<Stmt>,
    type_table: KeyTable<Type>,
    namespace_table: KeyTable<Namespace>
}

pub trait Ann {
    fn get_ast(&self) -> &AST;
}

impl Ann for AST {
    fn get_ast(&self) -> &AST {
        self
    }
}

pub struct AnnotatedAST<D, A> where D: ASTData, A: Ann {
    ann: ASTTable<D>,
    contained: A
}

impl<D, A> AnnotatedAST<D, A> where D: ASTData, A: Ann {
    pub fn wrap(contained: A) -> Self {
        AnnotatedAST {
            ann: ASTTable::new(),
            contained
        }
    }

    pub fn get_ann(&self) -> &ASTTable<D> {
        &self.ann
    }
}

impl<D, A> Ann for AnnotatedAST<D, A> where D: ASTData, A: Ann {
    fn get_ast(&self) -> &AST {
        self.contained.get_ast()
    }
}

impl<D, A> Deref for AnnotatedAST<D, A> where D: ASTData, A: Ann {
    type Target = A;

    fn deref(&self) -> &Self::Target {
        &self.contained
    }
}

pub trait ASTData {
    type FileData;
    type FunctionData;
    type FunctionParameterData;
    type ExprData;
    type StmtData;
    type TypeData;
    type NamespaceData;
}

pub struct ASTTable<D> where D: ASTData {
    file_data: HashMap<Key<File>, D::FileData>,
    function_data: HashMap<Key<Function>, D::FunctionData>,
    function_parameter_data: HashMap<Key<FunctionParameter>, D::FunctionParameterData>,
    expr_data: HashMap<Key<Expr>, D::ExprData>,
    stmt_data: HashMap<Key<Stmt>, D::StmtData>,
    type_data: HashMap<Key<Type>, D::TypeData>,
    namespace_data: HashMap<Key<Namespace>, D::NamespaceData>,
}

impl<D> ASTTable<D> where D: ASTData {
    pub fn new() -> Self {
        ASTTable {
            file_data: HashMap::new(),
            function_data: HashMap::new(),
            function_parameter_data: HashMap::new(),
            expr_data: HashMap::new(),
            stmt_data: HashMap::new(),
            type_data: HashMap::new(),
            namespace_data: HashMap::new(),
        }
    }
}

impl Key<File> {
    pub fn get<D: ASTData>(self, table: &ASTTable<D>) -> &D::FileData {
        &table.file_data[&self]
    }

    pub fn set<D: ASTData>(self, table: &mut ASTTable<D>, data: D::FileData) -> Self {
        table.file_data.insert(self, data);
        self
    }
}

impl Key<Function> {
    pub fn get<D: ASTData>(self, table: &ASTTable<D>) -> &D::FunctionData {
        &table.function_data[&self]
    }

    pub fn set<D: ASTData>(self, table: &mut ASTTable<D>, data: D::FunctionData) -> Self {
        table.function_data.insert(self, data);
        self
    }
}

impl Key<FunctionParameter> {
    pub fn get<D: ASTData>(self, table: &ASTTable<D>) -> &D::FunctionParameterData {
        &table.function_parameter_data[&self]
    }

    pub fn set<D: ASTData>(self, table: &mut ASTTable<D>, data: D::FunctionParameterData) -> Self {
        table.function_parameter_data.insert(self, data);
        self
    }
}

impl Key<Expr> {
    pub fn get<D: ASTData>(self, table: &ASTTable<D>) -> &D::ExprData {
        &table.expr_data[&self]
    }

    pub fn set<D: ASTData>(self, table: &mut ASTTable<D>, data: D::ExprData) -> Self {
        table.expr_data.insert(self, data);
        self
    }
}

impl Key<Stmt> {
    pub fn get<D: ASTData>(self, table: &ASTTable<D>) -> &D::StmtData {
        &table.stmt_data[&self]
    }

    pub fn set<D: ASTData>(self, table: &mut ASTTable<D>, data: D::StmtData) -> Self {
        table.stmt_data.insert(self, data);
        self
    }
}

impl Key<Type> {
    pub fn get<D: ASTData>(self, table: &ASTTable<D>) -> &D::TypeData {
        &table.type_data[&self]
    }

    pub fn set<D: ASTData>(self, table: &mut ASTTable<D>, data: D::TypeData) -> Self {
        table.type_data.insert(self, data);
        self
    }
}

impl Key<Namespace> {
    pub fn get<D: ASTData>(self, table: &ASTTable<D>) -> &D::NamespaceData {
        &table.namespace_data[&self]
    }

    pub fn set<D: ASTData>(self, table: &mut ASTTable<D>, data: D::NamespaceData) -> Self {
        table.namespace_data.insert(self, data);
        self
    }
}

impl AST {
    pub fn new() -> Self {
        AST {
            file_table: KeyTable::new(),
            function_table: KeyTable::new(),
            function_parameter_table: KeyTable::new(),
            expr_table: KeyTable::new(),
            stmt_table: KeyTable::new(),
            type_table: KeyTable::new(),
            namespace_table: KeyTable::new()
        }
    }

    pub fn add_file(&mut self, file: File) -> Key<File> {
        self.file_table.insert(file)
    }

    pub fn add_function(&mut self, func: Function) -> Key<Function> {
        self.function_table.insert(func)
    }

    pub fn add_function_parameter(&mut self, param: FunctionParameter) -> Key<FunctionParameter> {
        self.function_parameter_table.insert(param)
    }

    pub fn add_expr(&mut self, expr: Expr) -> Key<Expr> {
        self.expr_table.insert(expr)
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> Key<Stmt> {
        self.stmt_table.insert(stmt)
    }

    pub fn add_type(&mut self, typ: Type) -> Key<Type> {
        self.type_table.insert(typ)
    }

    pub fn add_namespace(&mut self, namespace: Namespace) -> Key<Namespace> {
        self.namespace_table.insert(namespace)
    }
}

impl Index<Key<Expr>> for AST {
    type Output = Expr;

    fn index(&self, index: Key<Expr>) -> &Self::Output {
        &self.expr_table[index]
    }
}

impl Index<Key<Type>> for AST {
    type Output = Type;

    fn index(&self, index: Key<Type>) -> &Self::Output {
        &self.type_table[index]
    }
}

pub struct File {
    pub functions: Vec<Key<Function>>
}

pub struct Function {
    pub name: String,
    pub parameters: Vec<Key<FunctionParameter>>,
    pub ret: Option<Key<Type>>,
    pub body: Key<Expr>
}

pub struct FunctionParameter {
    pub name: String,
    pub typ: Key<Type>,
}

pub enum BinOp {
    Add
}

pub enum Expr {
    Integer { num: u64 },
    Name { ns: Option<Key<Namespace>>, name: String },
    BinOp { left: Key<Expr>, op: BinOp, right: Key<Expr> },
    Call { callee: Key<Expr>, generic: Vec<Key<Type>>, args: Vec<Key<Expr>> },
    Field { obj: Key<Expr>, field: String },
    Block { stmts: Vec<Key<Stmt>> }
}

pub enum Stmt {
    Return { expr: Key<Expr> },
    Expr { expr: Key<Expr> },
    Let { name: String, typ: Option<Key<Type>>, value: Key<Expr> }
}

pub enum Type {
    Name { ns: Option<Key<Namespace>>, name: String, generic: Vec<Key<Type>> },
}

pub struct Namespace {
    pub ns: Option<Key<Namespace>>,
    pub name: String,
    pub generic: Vec<Key<Type>>
}

impl Namespace {
    pub fn from_type(typ: &Type) -> Namespace {
        if let Type::Name { ns, name, generic} = typ {
            return Namespace { ns: *ns, name: name.clone(), generic: generic.clone() };
        }
        panic!("Type was not a Name")
    }

    pub fn from_expr(expr: &Expr, generic: Vec<Key<Type>>) -> Namespace {
        if let Expr::Name { ns, name} = expr {
            return Namespace { ns: *ns, name: name.clone(), generic };
        }
        panic!("Expr was not a Name")
    }
}