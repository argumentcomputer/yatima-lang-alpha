#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuncIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TableIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MemIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GlobalIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ElemIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DataIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LocalIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LabelIndex(pub u32);

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Module {
  pub types: Vec<FuncType>,
  pub imports: Vec<Import>,
  pub exports: Vec<Export>,
  pub funcs: Vec<Func>,
  pub elems: Vec<ElemSegment>,
  pub tables: Vec<Table>,
  pub data: Vec<DataSegment>,
  pub memories: Vec<Memory>,
  pub globals: Vec<Global>,
  pub entrypoint: Option<StartFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportDesc {
  ImportFunc(TypeIndex),
  ImportTable(TableType),
  ImportMem(MemType),
  ImportGlobal(GlobalType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
  pub mod_name: Name,
  pub name: Name,
  pub desc: ImportDesc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ResultType(pub Vec<ValueType>);

#[derive(Debug, PartialEq, Clone)]
pub struct FuncType {
  pub params: ResultType,
  pub results: ResultType,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug)]
pub enum NumType {
  I32,
  I64,
  F32,
  F64,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug)]
pub enum RefType {
  FuncRef,
  ExternRef,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug)]
pub enum ValueType {
  Num(NumType),
  Ref(RefType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Name(pub Vec<char>);

#[derive(Debug, Clone, PartialEq)]
pub struct TableType {
  pub limit: Limits,
  pub ref_type: RefType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Limits {
  Range(u32, u32),
  From(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemType {
  pub limit: Limits,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExportDesc {
  ExportFunc(FuncIndex),
  ExportTable(TableIndex),
  ExportMem(MemIndex),
  ExportGlobal(GlobalIndex),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
  pub name: Name,
  pub desc: ExportDesc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression(pub Vec<Instruction>);

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
  pub typ: TypeIndex,
  pub locals: Vec<ValueType>,
  pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemArg {
  pub align: u32,
  pub offset: u32,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BlockType {
  BlockIndex(TypeIndex),
  BlockValue(ValueType),
  BlockNone,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
  Block { ty: BlockType, body: Vec<Instruction> },
  Loop { ty: BlockType, body: Vec<Instruction> },
  If { ty: BlockType, then_body: Vec<Instruction>, else_body: Vec<Instruction> },
  Unreachable,
  Nop,
  Br(LabelIndex),
  BrIf(LabelIndex),
  BrTable { labels: Vec<LabelIndex>, default_label: LabelIndex },
  Return,
  Call(FuncIndex),
  CallIndirect(TableIndex, TypeIndex),
  Drop,
  Select(Option<Vec<ValueType>>),
  RefNull(RefType),
  RefIsNull,
  RefFunc(FuncIndex),
  LocalGet(LocalIndex),
  LocalSet(LocalIndex),
  LocalTee(LocalIndex),
  GlobalGet(GlobalIndex),
  GlobalSet(GlobalIndex),
  TableGet(TableIndex),
  TableSet(TableIndex),
  TableSize(TableIndex),
  TableGrow(TableIndex),
  TableFill(TableIndex),
  TableCopy(TableIndex, TableIndex),
  TableInit(TableIndex, ElemIndex),
  ElemDrop(ElemIndex),
  I32Load(MemArg),
  I64Load(MemArg),
  F32Load(MemArg),
  F64Load(MemArg),
  I32Load8S(MemArg),
  I32Load8U(MemArg),
  I32Load16S(MemArg),
  I32Load16U(MemArg),
  I64Load8S(MemArg),
  I64Load8U(MemArg),
  I64Load16S(MemArg),
  I64Load16U(MemArg),
  I64Load32S(MemArg),
  I64Load32U(MemArg),
  I32Store(MemArg),
  I64Store(MemArg),
  F32Store(MemArg),
  F64Store(MemArg),
  I32Store8(MemArg),
  I32Store16(MemArg),
  I64Store8(MemArg),
  I64Store16(MemArg),
  I64Store32(MemArg),
  MemorySize,
  MemoryGrow,
  MemoryFill,
  MemoryCopy,
  MemoryInit(DataIndex),
  DataDrop(DataIndex),
  I32Const(i32),
  I64Const(i64),
  F32Const(f32),
  F64Const(f64),
  I32Clz,
  I32Ctz,
  I32Popcnt,
  I32Add,
  I32Sub,
  I32Mul,
  I32DivS,
  I32DivU,
  I32RemS,
  I32RemU,
  I32And,
  I32Or,
  I32Xor,
  I32Shl,
  I32ShrS,
  I32ShrU,
  I32Rotl,
  I32Rotr,
  I64Clz,
  I64Ctz,
  I64Popcnt,
  I64Add,
  I64Sub,
  I64Mul,
  I64DivS,
  I64DivU,
  I64RemS,
  I64RemU,
  I64And,
  I64Or,
  I64Xor,
  I64Shl,
  I64ShrS,
  I64ShrU,
  I64Rotl,
  I64Rotr,
  F32Abs,
  F32Neg,
  F32Ceil,
  F32Floor,
  F32Trunc,
  F32Nearest,
  F32Sqrt,
  F32Add,
  F32Sub,
  F32Mul,
  F32Div,
  F32Min,
  F32Max,
  F32Copysign,
  F64Abs,
  F64Neg,
  F64Ceil,
  F64Floor,
  F64Trunc,
  F64Nearest,
  F64Sqrt,
  F64Add,
  F64Sub,
  F64Mul,
  F64Div,
  F64Min,
  F64Max,
  F64Copysign,
  I32Eqz,
  I32Eq,
  I32Ne,
  I32LtS,
  I32LtU,
  I32GtS,
  I32GtU,
  I32LeS,
  I32LeU,
  I32GeS,
  I32GeU,
  I64Eqz,
  I64Eq,
  I64Ne,
  I64LtS,
  I64LtU,
  I64GtS,
  I64GtU,
  I64LeS,
  I64LeU,
  I64GeS,
  I64GeU,
  F32Eq,
  F32Ne,
  F32Lt,
  F32Gt,
  F32Le,
  F32Ge,
  F64Eq,
  F64Ne,
  F64Lt,
  F64Gt,
  F64Le,
  F64Ge,
  I32WrapI64,
  I32TruncF32S,
  I32TruncF32U,
  I32TruncF64S,
  I32TruncF64U,
  I64ExtendI32S,
  I64ExtendI32U,
  I64TruncF32S,
  I64TruncF32U,
  I64TruncF64S,
  I64TruncF64U,
  F32ConvertI32S,
  F32ConvertI32U,
  F32ConvertI64S,
  F32ConvertI64U,
  F32DemoteF64,
  F64ConvertI32S,
  F64ConvertI32U,
  F64ConvertI64S,
  F64ConvertI64U,
  F64PromoteF32,
  I32ReinterpretF32,
  I64ReinterpretF64,
  F32ReinterpretI32,
  F64ReinterpretI64,
  I32Extend8S,
  I32Extend16S,
  I64Extend8S,
  I64Extend16S,
  I64Extend32S,
  I32TruncSatF32S,
  I32TruncSatF32U,
  I32TruncSatF64S,
  I32TruncSatF64U,
  I64TruncSatF32S,
  I64TruncSatF32U,
  I64TruncSatF64S,
  I64TruncSatF64U,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElemMode {
  PassiveElem,
  DeclarativeElem,
  ActiveElem { table: TableIndex, offset: Expression },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElemSegment {
  pub typ: RefType,
  pub init: Vec<Expression>,
  pub mode: ElemMode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table(pub TableType);

#[derive(Debug, Clone, PartialEq)]
pub enum DataMode {
  PassiveData,
  ActiveData { memory: MemIndex, offset: Expression },
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataSegment {
  pub init: Vec<u8>,
  pub mode: DataMode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Memory(pub MemType);

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalType {
  pub is_mutable: bool,
  pub typ: ValueType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
  pub typ: GlobalType,
  pub init: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StartFunction(pub FuncIndex);
