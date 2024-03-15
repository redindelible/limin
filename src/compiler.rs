use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use semver::{Version, VersionReq};
use typed_arena::Arena;
use crate::emit::emit_llvm;
use crate::source::Source;
use crate::lowering::{lower, TypeCheckError};
use crate::parsing::{ast, parse_file, ParserError};
use crate::parsing::ast::{LibID, LibInfo};


#[derive(Debug)]
pub enum CompileResult<'a> {
    CouldNotParse(Vec<ParserError<'a>>),
    CouldNotTypeCheck(Vec<TypeCheckError<'a>>),
    FileError(io::Error),
    ArgumentError(String),
    ClangError,
    Success
}

pub struct Compiler {
    output: PathBuf,
    clang: PathBuf,
    rt_path: PathBuf,
    
    root: Option<PathBuf>,
    std_id: LibID,
    main_id: LibID,
    libs: HashMap<LibID, LibInfo>,

    sources: Arena<Source>,
}

impl Compiler {
    pub fn new(output: PathBuf, clang: Option<PathBuf>) -> Compiler {
        let lib_path = std::env::current_exe().expect("Error: Could not get the path to this executable.").with_file_name("lib");
        let rt_path = lib_path.join("rt.c");
        
        let mut compiler = Compiler {
            sources: Arena::new(),
            output,
            clang: clang.unwrap_or(PathBuf::from("clang")),
            rt_path,
            
            root: None,
            libs: HashMap::new(),
            std_id: LibID("".into()),
            main_id: LibID("".into()),
        };
        let std_id = compiler.add_lib("<std>".into(), lib_path.join("std").join("lib.lmn"));
        compiler.std_id = std_id;
        
        compiler
    }

    pub fn debug(output: PathBuf, rt_path: PathBuf, clang: Option<PathBuf>) -> Compiler {
        let mut compiler = Compiler {
            sources: Arena::new(),
            output,
            clang: clang.unwrap_or(PathBuf::from("clang")),
            rt_path,

            root: None,
            libs: HashMap::new(),
            std_id: LibID("".into()),
            main_id: LibID("".into()),
        };
        let lib_path = std::env::current_exe().expect("Error: Could not get the path to this executable.").with_file_name("lib");
        let std_id = compiler.add_lib("<std>".into(), lib_path.join("std").join("lib.lmn"));
        compiler.std_id = std_id;
        
        compiler
    }
    
    pub fn set_root(&mut self, root: PathBuf) -> LibID {
        assert!(self.root.is_none());
        self.root = Some(root.clone());
        let root_id = self.add_lib("<main>".into(), root);
        self.main_id = root_id.clone();
        self.add_libdep(root_id.clone(), "std".into(), self.std_id.clone());
        root_id
    }
    
    pub fn add_lib(&mut self, name: String, path: PathBuf) -> LibID {
        let lib_id = LibID(name);
        self.libs.insert(lib_id.clone(), LibInfo { root_path: path, libs: HashMap::new() });
        lib_id
    }
    
    pub fn add_libdep(&mut self, for_lib: LibID, name: String, libdep: LibID) {
        self.libs.get_mut(&for_lib).unwrap().libs.insert(name, libdep);
    }

    pub fn compile(&mut self) -> CompileResult {
        let mut parse_errors: Vec<ParserError> = Vec::new();
        let mut files = Vec::new();
        
        let mut to_parse = VecDeque::from([
            (self.root.clone().unwrap(), None, None, LibID("<main>".into()))
        ]);
        for (lib_id, lib_info) in &self.libs {
            to_parse.push_back((lib_info.root_path.clone(), None, None, lib_id.clone()));
        }
        
        let mut parsed = HashSet::new();
        while let Some((source_file, name, loc, in_lib)) = to_parse.pop_front() {
            if parsed.contains(&source_file) {
                continue;
            }
            parsed.insert(source_file.clone());
            
            let source = match Source::from_file(&source_file) {
                Ok(source) => self.sources.alloc(source),
                Err(_) => { parse_errors.push(ParserError::CouldNotFindMod(source_file, name, loc)); continue }
            };
            
            let file = match parse_file(source, in_lib.clone()) {
                Ok(file) => file,
                Err(e) => { parse_errors.extend(e); continue }
            };
            
            for mod_ in file.mods() {
                to_parse.push_back((source_file.with_file_name(format!("{}.lmn", &mod_.name)), Some(mod_.name.clone()), Some(mod_.loc), in_lib.clone()));
            }
            
            files.push(file);
        }
        if !parse_errors.is_empty() {
            return CompileResult::CouldNotParse(parse_errors);
        }
        let ast = ast::AST::from_files("test".into(), files, self.libs.clone());

        let lowered = match lower(ast) {
            Ok(ir) => ir,
            Err(errs) => {
                return CompileResult::CouldNotTypeCheck(errs)
            }
        };

        let llvm = emit_llvm(lowered);

        let Some(name) = self.output.file_name() else {
            return CompileResult::ArgumentError("Output file path is not valid".into());
        };

        let build_dir = self.output.with_file_name("build");
        match std::fs::create_dir_all(&build_dir) {
            Ok(_) => (),
            Err(e) => {
                return CompileResult::FileError(e);
            }
        };
        let ll_file = build_dir.join(name).with_extension("ll");
        let mut ll_file_handle = match File::create(&ll_file) {
            Ok(f) => f,
            Err(e) => {
                return CompileResult::FileError(e);
            }
        };
        match ll_file_handle.write_all(llvm.emit().as_bytes()) {
            Ok(()) => {

            },
            Err(e) => {
                return CompileResult::FileError(e);
            }
        };
        drop(ll_file_handle);

        let output = String::from_utf8(match Command::new(&self.clang).arg("--version").output() {
            Ok(o) => o,
            Err(e) => {
                return CompileResult::FileError(e);
            }
        }.stdout).unwrap();
        let output: Vec<&str> = output.lines().collect();
        let Some(first) = output.get(0) else {
            return CompileResult::ArgumentError("Could not parse clang version information.".into());
        };
        let Some(version) = first.split(' ').skip(2).next() else {
            return CompileResult::ArgumentError("Could not parse clang version information.".into());
        };
        let req = VersionReq::parse(">=15.0.0").unwrap();
        let Ok(actual) = Version::parse(version) else {
            return CompileResult::ArgumentError(format!("Could not parse clang version information (found {}).", version));
        };
        if !req.matches(&actual) {
            return CompileResult::ArgumentError(format!("Clang version does not meet requirements (found version {}, required {}).", actual, req));
        }

        let output = match
            Command::new(&self.clang)
                .arg(&ll_file)
                .arg(&self.rt_path)
                .arg("-o")
                .arg(&self.output)
                .arg("-Wno-override-module")
                .status() {
            Ok(output) => output,
            Err(e) => {
                return CompileResult::FileError(e);
            }
        };
        if !output.success() {
            return CompileResult::ClangError;
        }

        CompileResult::Success
    }
}
