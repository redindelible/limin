use std::borrow::Borrow;
use std::cell::{RefCell};
use std::collections::HashMap;
use indexmap::IndexMap;
use crate::emit::{ilir, lir, llvm};
use crate::emit::llvm::{GEPIndex, MemoryAccess, Value};
use crate::emit::llvm::CallingConvention::CCC;
use crate::emit::trace_roots::trace_roots;
use crate::util;

const FRAME_HEADER_COUNT: u32 = 3;
const STRUCT_HEADER_COUNT: u32 = 3;
const FUNCTION_EXTRA_COUNT: usize = 1;

pub fn emit_llvm(lir: lir::LIR) -> llvm::Module {
    LLVMEmitter::emit_lir(&lir)
}

struct FramesInfo<'a> {
    lir: &'a lir::LIR,
    frame_ptr: llvm::ValueRef,
    func: &'a ilir::Function,
    unmanaged: HashMap<u32, llvm::ValueRef>,
    frame_types: Vec<llvm::TypeRef>
}

impl FramesInfo<'_> {
    fn get_location_ptr(&self, builder: &mut llvm::Builder, loc: impl Borrow<ilir::Location>) -> llvm::ValueRef {
        match &self.func.locations[loc.borrow()] {
            ilir::LocationInfo::InFrame { frame_id, idx, .. } => {
                let ty = self.frame_types[*frame_id as usize].clone();
                builder.gep(None, ty, self.frame_ptr.clone(), vec![
                    GEPIndex::ConstantIndex(0),
                    GEPIndex::ConstantIndex(*idx as u32 + FRAME_HEADER_COUNT)
                ]).to_value()
            },
            ilir::LocationInfo::Unmanaged { id, .. } => {
                self.unmanaged[id].clone()
            }
        }
    }

    fn get_ty(&self, loc: impl Borrow<ilir::Location>) -> &lir::Type {
        match &self.func.locations[loc.borrow()] {
            ilir::LocationInfo::InFrame { ty, .. } | ilir::LocationInfo::Unmanaged { ty, .. } => ty
        }
    }
}

struct Mangler {
    curr: RefCell<u32>
}

impl Mangler {
    fn new() -> Mangler {
        Mangler {
            curr: RefCell::new(0)
        }
    }

    fn next_num(&self) -> String {
        let value = format!("{:08x}", *self.curr.borrow());
        *self.curr.borrow_mut() += 1;
        value
    }

    fn mangle<'a, T: IntoIterator>(&self, s: T) -> String where T::Item: ToString {
        format!("{}_{}", util::join_with(s, "_"), self.next_num())
    }
}

struct StructInfo {
    type_ref: llvm::StructRef,

    trace_fn: llvm::FunctionRef,
    trace_obj_ptr: llvm::ParameterRef,
    type_info: llvm::GlobalRef
}

struct FunctionInfo {
    function_ref: llvm::FunctionRef,

    parent_frame: llvm::ValueRef
}

struct LLVMEmitter {
    struct_mapping: IndexMap<lir::StructID, StructInfo>,
    function_mapping: IndexMap<lir::FunctionID, FunctionInfo>,
    mangling: Mangler,

    create_obj_fn: llvm::ValueRef,
    mark_obj_fn: llvm::ValueRef,
}

impl LLVMEmitter {
    fn emit_lir(lir: &lir::LIR) -> llvm::Module {
        let mut module = llvm::Module::new("Placeholder");

        let create_obj_fn = module.add_function(
            "limin_create_object",
            llvm::Types::ptr(),
            vec![
                llvm::ParameterRef::new("type", llvm::Types::ptr()).nowrite().nofree().noalias(),
                llvm::ParameterRef::new("frame", llvm::Types::ptr()).nocapture().nowrite().nofree().noalias()
            ],
            CCC
        ).ret_noalias().willreturn().nounwind()
            .memory(MemoryAccess::Read)    // this is probably technically not safe since we access inaccessiblemem
            .to_value();

        let mark_obj_fn = module.add_function(
            "limin_mark_object",
            llvm::Types::void(),
            vec![
                llvm::ParameterRef::new("obj", llvm::Types::ptr()).nofree().noalias().nocapture()
            ],
            CCC
        ).willreturn().nounwind().to_value();

        let mut emit = LLVMEmitter {
            struct_mapping: IndexMap::new(),
            function_mapping: IndexMap::new(),
            mangling: Mangler::new(),

            create_obj_fn,
            mark_obj_fn
        };

        for (struct_id, struct_) in &lir.structs {
            let name = emit.mangling.mangle(["struct", &struct_.name]);
            let struct_ref = module.types.add_struct(&name);
            let trace_obj_ptr = llvm::ParameterRef::new("obj", llvm::Types::ptr());
            let trace_fn = module.add_function(
                emit.mangling.mangle(["trace", &name]),
                llvm::Types::void(),
                vec![trace_obj_ptr.clone()],
                CCC
            );

            let info = module.add_global_constant(emit.mangling.mangle([&name, "info"]), llvm::Types::zeroinit(
                llvm::Types::struct_(vec![llvm::Types::ptr(), llvm::Types::int(64)])
            ));
            emit.struct_mapping.insert(*struct_id, StructInfo {
                type_ref: struct_ref,
                trace_fn,
                trace_obj_ptr,
                type_info: info,
            });
        }

        for (struct_id, struct_) in &lir.structs {
            let struct_ref = emit.struct_mapping[struct_id].type_ref.clone();
            let obj_ptr = emit.struct_mapping[struct_id].trace_obj_ptr.clone().to_value();
            let mut trace_builder = llvm::Builder::new(&emit.struct_mapping[struct_id].trace_fn);

            let mut fields = vec![
                llvm::Types::ptr(),
                llvm::Types::ptr(),
                llvm::Types::int(8),
            ];
            debug_assert_eq!(fields.len() as u32, STRUCT_HEADER_COUNT);
            for (i, field) in struct_.fields.iter().enumerate() {
                let field_ty = emit.emit_type(&field.ty, lir);
                fields.push(field_ty.clone());

                let field_ptr = trace_builder.gep(None, struct_ref.as_type_ref(), obj_ptr.clone(), vec![
                    GEPIndex::ConstantIndex(0),
                    GEPIndex::ConstantIndex(i as u32 + STRUCT_HEADER_COUNT),
                ]).to_value();
                let field_loaded = trace_builder.load(None, field_ty, field_ptr).to_value();
                emit.emit_type_tracer(field_loaded, &field.ty, &mut trace_builder);
            }
            struct_ref.set_fields(fields);

            trace_builder.ret_void();
        }

        for (struct_id, _) in &lir.structs {
            let struct_ref = emit.struct_mapping[struct_id].type_ref.clone();

            let trace = llvm::Types::function_constant(&emit.struct_mapping[struct_id].trace_fn);
            let size = llvm::Types::int_constant(64, struct_ref.as_type_ref().size() as u64);
            emit.struct_mapping[struct_id].type_info.initialize(llvm::Types::struct_constant(vec![
                trace,
                size
            ]));
        }

        for (func_id, func) in &lir.functions {
            let name = emit.mangling.mangle(["function", &func.name]);

            let ret = if let Some(ret) = &func.ret {
                emit.emit_type(ret, lir)
            } else {
                llvm::Types::void()
            };

            let parent_frame = llvm::ParameterRef::new("parent_frame", llvm::Types::ptr());
            let mut parameters = vec![
                parent_frame.clone()
            ];
            debug_assert_eq!(parameters.len(), FUNCTION_EXTRA_COUNT);

            parameters.extend(func.parameters.iter().map(|param| {
                let name = emit.mangling.mangle(["param", &param.name]);
                let ty = emit.emit_type(&param.ty, lir);
                llvm::ParameterRef::new(name, ty)
            }));
            let func_ref = module.add_function(&name, ret, parameters, CCC);

            emit.function_mapping.insert(*func_id, FunctionInfo {
                function_ref: func_ref,
                parent_frame: parent_frame.to_value()
            });
        }

        for (func_id, _) in &lir.functions {
            emit.emit_function(*func_id, lir);
        }

        let main = module.add_function("main", llvm::Types::int(32), vec![], CCC);
        let main_builder = llvm::Builder::new(&main);
        let exit_code = main_builder.call(
            None, CCC, llvm::Types::int(32),
            &emit.function_mapping[&lir.main_function].function_ref.clone().to_value(),
            vec![llvm::Types::null().to_value(), llvm::Types::null().to_value()]
        ).to_value();
        main_builder.ret(exit_code);

        module
    }

    fn get_frame_info<'a>(&self, id: lir::FunctionID, func: &'a ilir::Function, lir: &'a lir::LIR, builder: &mut llvm::Builder) -> FramesInfo<'a> {
        let mut unmanaged: HashMap<u32, llvm::ValueRef> = HashMap::new();
        for loc in func.locations.values() {
            match loc {
                ilir::LocationInfo::Unmanaged { id, ty } => {
                    let value = builder.alloca(None, self.emit_type(ty, lir)).to_value();
                    unmanaged.insert(*id, value);
                }
                _ => { }
            }
        }

        // todo generate trace function
        let mut frame_types: Vec<llvm::TypeRef> = vec![];

        for frame in &func.frames {
            let mut frame_fields: Vec<llvm::TypeRef> = vec![
                llvm::Types::ptr(),  // parent frame ptr
                llvm::Types::ptr(),  // trace function ptr
                llvm::Types::int(32),  // frame state
            ];
            debug_assert_eq!(frame_fields.len() as u32, FRAME_HEADER_COUNT);

            for item_ty in &frame.types {
                frame_fields.push(self.emit_type(item_ty, lir))
            }

            frame_types.push(llvm::Types::struct_(frame_fields));
        }

        let frame_ptr;
        if let Some(largest_size) = frame_types.iter().map(|f| f.size()).max() {
            frame_ptr = builder.alloca(Some("frame".into()), llvm::Types::array(&llvm::Types::int(8), largest_size)).to_value();

            let parent_frame_ptr = builder.gep(None, llvm::Types::struct_(vec![
                llvm::Types::ptr(),  // parent frame ptr
                llvm::Types::ptr(),  // trace function ptr
                llvm::Types::int(32),  // frame state
            ]), frame_ptr.clone(), vec![
                GEPIndex::ConstantIndex(0),
                GEPIndex::ConstantIndex(0),
            ]).to_value();
            builder.store(parent_frame_ptr, self.function_mapping[&id].parent_frame.clone());
        } else {
            frame_ptr = llvm::Types::null().to_value();
        }

        FramesInfo {
            lir,
            frame_ptr,
            frame_types,
            unmanaged,
            func
        }
    }

    fn emit_function(&mut self, id: lir::FunctionID, lir: &lir::LIR) {
        let func = trace_roots(id, lir);

        let func_ref = self.function_mapping[&id].function_ref.clone();
        let mut builder = llvm::Builder::new(&func_ref);

        let frames_info = self.get_frame_info(id, &func, lir, &mut builder);

        for (param_ref, param) in func_ref.parameters().iter().skip(FUNCTION_EXTRA_COUNT).zip(&func.parameters) {
            let param_store_loc = frames_info.get_location_ptr(&mut builder, param.location);
            builder.store(param_store_loc, param_ref.clone().to_value());
        }

        self.emit_block(&mut builder, &func.block, lir, &frames_info);
    }

    fn load_and_store(&self, builder: &mut llvm::Builder, from: &ilir::Location, to: &ilir::Location, info: &FramesInfo) {
        let loc = info.get_location_ptr(builder, from);
        let value = builder.load(None, self.emit_type(info.get_ty(from), info.lir), loc).to_value();
        let local = info.get_location_ptr(builder, to);
        builder.store(local, value);
    }

    fn emit_block(&mut self, builder: &mut llvm::Builder, block: &ilir::Block, lir: &lir::LIR, frames_info: &FramesInfo) {
        use crate::emit::ilir::Instruction;

        for instr in &block.instructions {
            match instr {
                Instruction::DeclareLocal { ty, local, value } => {
                    let loc = frames_info.get_location_ptr(builder, value);
                    let value = builder.load(None, self.emit_type(ty, lir), loc).to_value();
                    let local = frames_info.get_location_ptr(builder, local);
                    builder.store(local, value);
                }
                Instruction::LoadLocal { local, store } => {
                    let loc = frames_info.get_location_ptr(builder, local);
                    let value = builder.load(None, self.emit_type(frames_info.get_ty(local), lir), loc).to_value();
                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, value);
                }
                Instruction::StoreLocal { value, local } => {
                    let loc = frames_info.get_location_ptr(builder, value);
                    let value = builder.load(None, self.emit_type(frames_info.get_ty(local), lir), loc).to_value();
                    let local = frames_info.get_location_ptr(builder, local);
                    builder.store(local, value);
                }
                Instruction::LoadFunction { func, store } => {
                    let value = self.function_mapping[func].function_ref.clone().to_value();
                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, value);
                }
                Instruction::LoadI32 { value, store } => {
                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, llvm::Types::int_constant(32, *value as u64).to_value());
                }
                Instruction::LoadBool { value, store } => {
                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, llvm::Types::int_constant(1, *value as u64).to_value());
                }
                Instruction::LoadNull { store } => {
                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, llvm::Types::null().to_value());
                }
                Instruction::Return { value } => {
                    let loc = frames_info.get_location_ptr(builder, value);
                    let value = builder.load(None, self.emit_type(frames_info.get_ty(value), lir), loc).to_value();
                    builder.ret(value);
                }
                Instruction::ReturnVoid => {
                    builder.ret_void();
                }
                Instruction::BlockValue { block, value, store } => {
                    self.emit_block(builder, block, lir, frames_info);
                    self.load_and_store(builder, value, store, frames_info);
                }
                Instruction::BlockVoid { block } => {
                    self.emit_block(builder, block, lir, frames_info);
                }
                Instruction::CreateTuple { values, store } => {
                    let tys: Vec<llvm::TypeRef> = values.iter().map(|v| self.emit_type(frames_info.get_ty(v), lir)).collect();

                    let tuple_type = llvm::Types::struct_(tys.clone());
                    let mut init_tuple = llvm::Types::zeroinit(tuple_type.clone()).to_value();

                    for (i, (value, ty)) in values.iter().zip(&tys).enumerate() {
                        let loc = frames_info.get_location_ptr(builder, value);
                        let value = builder.load(None, ty.clone(), loc).to_value();
                        init_tuple = builder.insertvalue(None, &init_tuple, value, vec![i as u32]).to_value();
                    }

                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, init_tuple);
                }
                Instruction::GetElement { value, idx, store } => {
                    let loc = frames_info.get_location_ptr(builder, value);
                    let ptr = builder.gep(None, self.emit_type(frames_info.get_ty(value), lir), loc, vec![
                        GEPIndex::ConstantIndex(0),
                        GEPIndex::ConstantIndex(*idx as u32)
                    ]).to_value();
                    let value = builder.load(None, self.emit_type(frames_info.get_ty(store), lir), ptr).to_value();
                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, value);
                }
                Instruction::Splat { value, stores } => {
                    let loc = frames_info.get_location_ptr(builder, value);
                    for (idx, store) in stores.iter().enumerate() {
                        let ptr = builder.gep(None, self.emit_type(frames_info.get_ty(value), lir), loc.clone(), vec![
                            GEPIndex::ConstantIndex(0),
                            GEPIndex::ConstantIndex(idx as u32)
                        ]).to_value();
                        let value = builder.load(None, self.emit_type(frames_info.get_ty(store), lir), ptr).to_value();
                        let store = frames_info.get_location_ptr(builder, store);
                        builder.store(store, value);
                    }
                }
                Instruction::CreateStruct { id, fields, store } => {
                    let type_info = self.struct_mapping[id].type_info.clone().to_value();
                    let frame_ptr = frames_info.frame_ptr.clone();
                    let obj_ptr = builder.call(None, CCC, llvm::Types::ptr(), &self.create_obj_fn, vec![
                        type_info,
                        frame_ptr
                    ]).to_value();
                    for (idx, field) in fields.iter().enumerate() {
                        let ptr = builder.gep(None, self.struct_mapping[id].type_ref.as_type_ref(), obj_ptr.clone(), vec![
                            GEPIndex::ConstantIndex(0),
                            GEPIndex::ConstantIndex(idx as u32 + STRUCT_HEADER_COUNT)
                        ]).to_value();
                        let field_value_ptr = frames_info.get_location_ptr(builder, field);
                        let field_value = builder.load(None, self.emit_type(frames_info.get_ty(field), lir), field_value_ptr).to_value();
                        builder.store(ptr, field_value);
                    }
                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, obj_ptr);
                }
                Instruction::GetField { value, id, field, store } => {
                    let obj_loc = frames_info.get_location_ptr(builder, value);
                    let obj = builder.load(None, llvm::Types::ptr(), obj_loc).to_value();
                    let field_ptr = builder.gep(None, self.struct_mapping[id].type_ref.as_type_ref(), obj, vec![
                        GEPIndex::ConstantIndex(0),
                        GEPIndex::ConstantIndex(*field as u32 + STRUCT_HEADER_COUNT),
                    ]).to_value();
                    let field = builder.load(None, self.emit_type(frames_info.get_ty(store), lir), field_ptr).to_value();
                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, field);
                }
                Instruction::Call { callee, arguments, store } => {
                    let lir::Type::Function(_, ret) = frames_info.get_ty(callee) else { panic!() };
                    let ret_type = self.emit_type(&ret.as_ref().unwrap(), lir);

                    let callee_ptr = frames_info.get_location_ptr(builder, callee);
                    let callee = builder.load(None, llvm::Types::ptr(), callee_ptr).to_value();

                    let mut llvm_arguments: Vec<llvm::ValueRef> = vec![
                        frames_info.frame_ptr.clone()
                    ];

                    llvm_arguments.extend(arguments.iter().map(|arg| {
                        let arg_ptr = frames_info.get_location_ptr(builder, arg);
                        builder.load(None, self.emit_type(frames_info.get_ty(arg), lir), arg_ptr).to_value()
                    }));

                    let res = builder.call(None, CCC, ret_type, &callee, llvm_arguments).to_value();

                    let store = frames_info.get_location_ptr(builder, store);
                    builder.store(store, res);
                }
                Instruction::CallVoid { .. } => {
                    todo!()
                }
                Instruction::StatePoint { id } => {
                    let frame_ty = frames_info.frame_types[*id as usize].clone();
                    let frame_state_ptr = builder.gep(None, frame_ty, frames_info.frame_ptr.clone(), vec![
                        GEPIndex::ConstantIndex(0),
                        GEPIndex::ConstantIndex(2),
                    ]).to_value();
                    builder.store(frame_state_ptr, llvm::Types::int_constant(32, *id as u64).to_value());
                }
                Instruction::Unreachable => {
                    todo!()
                }
            }
        }
    }

    fn emit_type(&self, ty: &lir::Type, lir: &lir::LIR) -> llvm::TypeRef {
        use lir::Type;
        use llvm::Types;

        match ty {
            Type::AnyRef => Types::ptr(),
            Type::Boolean => Types::int(1),
            Type::Int32 => Types::int(32),
            Type::Function(_, _) => {
                Types::ptr()
            },
            Type::StructRef(_) => {
                Types::ptr()
            }
            Type::Tuple(items) => {
                Types::struct_(items.iter().map(|ty| self.emit_type(ty, lir)).collect())
            }
        }
    }

    fn emit_type_tracer(&self, value: llvm::ValueRef, ty: &lir::Type, builder: &mut llvm::Builder) {
        use lir::Type;

        match ty {
            Type::AnyRef => {
                builder.call_void(CCC, &self.mark_obj_fn, vec![
                    value
                ]);
            }
            Type::Boolean => { }
            Type::Int32 => { }
            Type::Function(_, _) => {
                todo!()
            }
            Type::StructRef(_) => {
                builder.call_void(CCC, &self.mark_obj_fn, vec![
                    value
                ]);
            }
            Type::Tuple(tys) => {
                for (i, ty) in tys.iter().enumerate() {
                    let inside_value = builder.extractvalue(None, &value, vec![i as u32]).to_value();
                    self.emit_type_tracer(inside_value, ty, builder);
                }
            }
        }
    }
}