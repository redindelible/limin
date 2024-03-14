use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use semver::{Version, VersionReq};
use typed_arena::Arena;
use crate::emit::emit_llvm;
use crate::source::Source;
use crate::lowering::{lower, TypeCheckError};
use crate::parsing::{ast, parse_file, ParserError};


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
    // sources: Arena<Source>,
    output: PathBuf,
    clang: PathBuf,
    rt_path: PathBuf,
    
    root: Option<PathBuf>,
    libs: Vec<PathBuf>,

    sources: Arena<Source>,
}

impl Compiler {
    pub fn new(output: PathBuf, clang: Option<PathBuf>) -> Compiler {
        let lib_path = std::env::current_exe().expect("Error: Could not get the path to this executable.").with_file_name("lib");
        let rt_path = lib_path.join("rt.c");
        Compiler {
            sources: Arena::new(),
            output,
            clang: clang.unwrap_or(PathBuf::from("clang")),
            rt_path,
            
            root: None,
            libs: Vec::new()
        }
    }

    pub fn debug(output: PathBuf, rt_path: PathBuf, clang: Option<PathBuf>) -> Compiler {
        Compiler {
            sources: Arena::new(),
            output,
            clang: clang.unwrap_or(PathBuf::from("clang")),
            rt_path,

            root: None,
            libs: Vec::new()
        }
    }
    
    pub fn set_root(&mut self, root: PathBuf) {
        self.root = Some(root);
    }

    pub fn compile(&mut self) -> CompileResult {
        let mut parse_errors: Vec<ParserError> = Vec::new();
        let mut files = Vec::new();
        
        let mut to_parse = VecDeque::from([
            (self.root.clone().unwrap(), None, None)
        ]);
        let mut parsed = HashSet::new();
        while let Some((source_file, name, loc)) = to_parse.pop_front() {
            if parsed.contains(&source_file) {
                continue;
            }
            parsed.insert(source_file.clone());
            
            let source = match Source::from_file(&source_file) {
                Ok(source) => self.sources.alloc(source),
                Err(_) => { parse_errors.push(ParserError::CouldNotFindMod(source_file, name, loc)); continue }
            };
            
            let file = match parse_file(source) {
                Ok(file) => file,
                Err(e) => { parse_errors.extend(e); continue }
            };
            
            for mod_ in file.mods() {
                to_parse.push_back((source_file.with_file_name(format!("{}.lmn", &mod_.name)), Some(mod_.name.clone()), Some(mod_.loc)));
            }
            
            files.push(file);
        }
        if !parse_errors.is_empty() {
            return CompileResult::CouldNotParse(parse_errors);
        }
        let ast = ast::AST::from_files("test".into(), files);

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
