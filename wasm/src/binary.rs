use leb128::{
  low_bits_of_byte,
  CONTINUATION_BIT,
};

use crate::ast::*;

pub trait ToBinary {
  fn push_to(self, v: &mut Vec<u8>);
}

impl ToBinary for usize {
  fn push_to(self, v: &mut Vec<u8>) { v.extend(self.to_le_bytes()); }
}

fn encode_vec<B: ToBinary>(vec: Vec<B>, v: &mut Vec<u8>) {
  vec.len().push_to(v);
  for x in vec {
    x.push_to(v);
  }
}

fn leb_u32(mut val: u32, v: &mut Vec<u8>) {
  loop {
    let mut byte = {
      let byte = val & (std::u8::MAX as u32);
      low_bits_of_byte(byte as u8)
    };
    val >>= 7;
    if val != 0 {
      byte |= CONTINUATION_BIT;
    }

    v.push(byte);

    if val == 0 {
      break;
    }
  }
}

fn leb_i32(mut val: i32, v: &mut Vec<u8>) {
  loop {
    let mut byte = val as u8;
    // Keep the sign bit for testing
    val >>= 6;
    let done = val == 0 || val == -1;
    if done {
      byte &= !CONTINUATION_BIT;
    }
    else {
      val >>= 1;
      byte |= CONTINUATION_BIT;
    }

    v.push(byte);

    if done {
      break;
    }
  }
}

fn leb_i64(mut val: i64, v: &mut Vec<u8>) {
  loop {
    let mut byte = val as u8;
    // Keep the sign bit for testing
    val >>= 6;
    let done = val == 0 || val == -1;
    if done {
      byte &= !CONTINUATION_BIT;
    }
    else {
      val >>= 1;
      byte |= CONTINUATION_BIT;
    }

    v.push(byte);

    if done {
      break;
    }
  }
}

impl ToBinary for LabelIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for MemIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for TableIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for DataIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for ElemIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for TypeIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for FuncIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for GlobalIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for LocalIndex {
  fn push_to(self, v: &mut Vec<u8>) { leb_u32(self.0, v) }
}

impl ToBinary for NumType {
  fn push_to(self, v: &mut Vec<u8>) {
    v.push(match self {
      Self::I32 => 0x7F,
      Self::I64 => 0x7E,
      Self::F32 => 0x7D,
      Self::F64 => 0x7C,
    });
  }
}

impl ToBinary for RefType {
  fn push_to(self, v: &mut Vec<u8>) {
    v.push(match self {
      Self::ExternRef => 0x6F,
      Self::FuncRef => 0x70,
    });
  }
}

impl ToBinary for ValueType {
  fn push_to(self, v: &mut Vec<u8>) {
    match self {
      Self::Num(n) => n.push_to(v),
      Self::Ref(r) => r.push_to(v),
    }
  }
}

impl ToBinary for ResultType {
  fn push_to(self, v: &mut Vec<u8>) { encode_vec(self.0, v); }
}

impl ToBinary for FuncType {
  fn push_to(self, v: &mut Vec<u8>) {
    v.push(0x60);
    self.params.push_to(v);
    self.results.push_to(v);
  }
}

impl ToBinary for Limits {
  fn push_to(self, v: &mut Vec<u8>) {
    match self {
      Limits::Range(n, m) => {
        v.push(0x01);
        leb_u32(n, v);
        leb_u32(m, v);
      }
      Limits::From(n) => {
        v.push(0x00);
        leb_u32(n, v);
      }
    }
  }
}

impl ToBinary for MemType {
  fn push_to(self, v: &mut Vec<u8>) { self.limit.push_to(v) }
}

impl ToBinary for TableType {
  fn push_to(self, v: &mut Vec<u8>) {
    self.ref_type.push_to(v);
    self.limit.push_to(v);
  }
}

impl ToBinary for GlobalType {
  fn push_to(self, v: &mut Vec<u8>) {
    self.typ.push_to(v);
    v.push(if self.is_mutable { 0x01 } else { 0x00 });
  }
}

impl ToBinary for BlockType {
  fn push_to(self, v: &mut Vec<u8>) {
    match self {
      Self::BlockIndex(TypeIndex(i)) => leb_i32(i as i32, v),
      Self::BlockValue(vt) => vt.push_to(v),
      Self::BlockNone => v.push(0x40),
    }
  }
}

impl ToBinary for MemArg {
  fn push_to(self, v: &mut Vec<u8>) {
    leb_u32(self.align, v);
    leb_u32(self.offset, v);
  }
}

impl ToBinary for Instruction {
  fn push_to(self, v: &mut Vec<u8>) {
    match self {
      Self::Block { ty, body } => {
        v.push(0x02);
        ty.push_to(v);
        for i in body {
          i.push_to(v);
        }
        v.push(0x0B);
      }
      Self::Loop { ty, body } => {
        v.push(0x03);
        ty.push_to(v);
        for i in body {
          i.push_to(v);
        }
        v.push(0x0B);
      }
      Self::If { ty, then_body, else_body } => {
        v.push(0x04);
        ty.push_to(v);
        for i in then_body {
          i.push_to(v);
        }
        if !else_body.is_empty() {
          v.push(0x05);
          for i in else_body {
            i.push_to(v);
          }
        }
        v.push(0x0B);
      }
      Self::Unreachable => v.push(0x00),
      Self::Nop => v.push(0x01),
      Self::Br(l) => {
        v.push(0x0C);
        l.push_to(v);
      }
      Self::BrIf(l) => {
        v.push(0x0D);
        l.push_to(v);
      }
      Self::BrTable { labels, default_label } => {
        v.push(0x0E);
        encode_vec(labels, v);
        default_label.push_to(v);
      }
      Self::Return => v.push(0x0F),
      Self::Call(x) => {
        v.push(0x10);
        x.push_to(v);
      }
      Self::CallIndirect(x, y) => {
        v.push(0x11);
        y.push_to(v);
        x.push_to(v);
      }
      Self::Drop => v.push(0x1A),
      Self::Select(o) => match o {
        None => v.push(0x1B),
        Some(ts) => {
          v.push(0x1C);
          encode_vec(ts, v);
        }
      },
      Self::RefNull(t) => {
        v.push(0xD0);
        t.push_to(v);
      }
      Self::RefIsNull => v.push(0xD1),
      Self::RefFunc(x) => {
        v.push(0xD2);
        x.push_to(v);
      }
      Self::LocalGet(x) => {
        v.push(0x20);
        x.push_to(v);
      }
      Self::LocalSet(x) => {
        v.push(0x21);
        x.push_to(v);
      }
      Self::LocalTee(x) => {
        v.push(0x22);
        x.push_to(v);
      }
      Self::GlobalGet(x) => {
        v.push(0x23);
        x.push_to(v);
      }
      Self::GlobalSet(x) => {
        v.push(0x24);
        x.push_to(v);
      }
      Self::TableGet(x) => {
        v.push(0x25);
        x.push_to(v);
      }
      Self::TableSet(x) => {
        v.push(0x26);
        x.push_to(v);
      }
      Self::TableSize(x) => {
        v.push(0xFC);
        leb_u32(16, v);
        x.push_to(v);
      }
      Self::TableGrow(x) => {
        v.push(0xFC);
        leb_u32(15, v);
        x.push_to(v);
      }
      Self::TableFill(x) => {
        v.push(0xFC);
        leb_u32(17, v);
        x.push_to(v);
      }
      Self::TableCopy(x, y) => {
        v.push(0xFC);
        leb_u32(14, v);
        x.push_to(v);
        y.push_to(v);
      }
      Self::TableInit(x, y) => {
        v.push(0xFC);
        leb_u32(12, v);
        y.push_to(v);
        x.push_to(v);
      }
      Self::ElemDrop(x) => {
        v.push(0xFC);
        leb_u32(13, v);
        x.push_to(v);
      }
      Self::I32Load(x) => {
        v.push(0x28);
        x.push_to(v);
      }
      Self::I64Load(x) => {
        v.push(0x29);
        x.push_to(v);
      }
      Self::F32Load(x) => {
        v.push(0x2A);
        x.push_to(v);
      }
      Self::F64Load(x) => {
        v.push(0x2B);
        x.push_to(v);
      }
      Self::I32Load8S(x) => {
        v.push(0x2C);
        x.push_to(v);
      }
      Self::I32Load8U(x) => {
        v.push(0x2D);
        x.push_to(v);
      }
      Self::I32Load16S(x) => {
        v.push(0x2E);
        x.push_to(v);
      }
      Self::I32Load16U(x) => {
        v.push(0x2F);
        x.push_to(v);
      }
      Self::I64Load8S(x) => {
        v.push(0x30);
        x.push_to(v);
      }
      Self::I64Load8U(x) => {
        v.push(0x31);
        x.push_to(v);
      }
      Self::I64Load16S(x) => {
        v.push(0x32);
        x.push_to(v);
      }
      Self::I64Load16U(x) => {
        v.push(0x33);
        x.push_to(v);
      }
      Self::I64Load32S(x) => {
        v.push(0x34);
        x.push_to(v);
      }
      Self::I64Load32U(x) => {
        v.push(0x35);
        x.push_to(v);
      }
      Self::I32Store(x) => {
        v.push(0x36);
        x.push_to(v);
      }
      Self::I64Store(x) => {
        v.push(0x37);
        x.push_to(v);
      }
      Self::F32Store(x) => {
        v.push(0x38);
        x.push_to(v);
      }
      Self::F64Store(x) => {
        v.push(0x39);
        x.push_to(v);
      }
      Self::I32Store8(x) => {
        v.push(0x3A);
        x.push_to(v);
      }
      Self::I32Store16(x) => {
        v.push(0x3B);
        x.push_to(v);
      }
      Self::I64Store8(x) => {
        v.push(0x3C);
        x.push_to(v);
      }
      Self::I64Store16(x) => {
        v.push(0x3D);
        x.push_to(v);
      }
      Self::I64Store32(x) => {
        v.push(0x3E);
        x.push_to(v);
      }
      Self::MemorySize => {
        v.push(0x3F);
        v.push(0x00);
      }
      Self::MemoryGrow => {
        v.push(0x40);
        v.push(0x00);
      }
      Self::MemoryFill => {
        v.push(0xFC);
        leb_u32(11, v);
        v.push(0x00);
      }
      Self::MemoryCopy => {
        v.push(0xFC);
        leb_u32(10, v);
        v.push(0x00);
        v.push(0x00);
      }
      Self::MemoryInit(x) => {
        v.push(0xFC);
        leb_u32(8, v);
        x.push_to(v);
        v.push(0x00);
      }
      Self::DataDrop(x) => {
        v.push(0xFC);
        leb_u32(9, v);
        x.push_to(v);
      }
      Self::I32Const(n) => {
        v.push(0x41);
        leb_i32(n, v);
      }
      Self::I64Const(n) => {
        v.push(0x42);
        leb_i64(n, v);
      }
      Self::F32Const(f) => {
        v.push(0x43);
        for byte in f.to_le_bytes() {
          v.push(byte);
        }
      }
      Self::F64Const(f) => {
        v.push(0x44);
        for byte in f.to_le_bytes() {
          v.push(byte);
        }
      }
      Self::I32Clz => v.push(0x67),
      Self::I32Ctz => v.push(0x68),
      Self::I32Popcnt => v.push(0x69),
      Self::I32Add => v.push(0x6A),
      Self::I32Sub => v.push(0x6B),
      Self::I32Mul => v.push(0x6C),
      Self::I32DivS => v.push(0x6D),
      Self::I32DivU => v.push(0x6E),
      Self::I32RemS => v.push(0x6F),
      Self::I32RemU => v.push(0x70),
      Self::I32And => v.push(0x71),
      Self::I32Or => v.push(0x72),
      Self::I32Xor => v.push(0x73),
      Self::I32Shl => v.push(0x74),
      Self::I32ShrS => v.push(0x75),
      Self::I32ShrU => v.push(0x76),
      Self::I32Rotl => v.push(0x77),
      Self::I32Rotr => v.push(0x78),
      Self::I64Clz => v.push(0x79),
      Self::I64Ctz => v.push(0x7A),
      Self::I64Popcnt => v.push(0x7B),
      Self::I64Add => v.push(0x7C),
      Self::I64Sub => v.push(0x7D),
      Self::I64Mul => v.push(0x7E),
      Self::I64DivS => v.push(0x7F),
      Self::I64DivU => v.push(0x80),
      Self::I64RemS => v.push(0x81),
      Self::I64RemU => v.push(0x82),
      Self::I64And => v.push(0x83),
      Self::I64Or => v.push(0x84),
      Self::I64Xor => v.push(0x85),
      Self::I64Shl => v.push(0x86),
      Self::I64ShrS => v.push(0x87),
      Self::I64ShrU => v.push(0x88),
      Self::I64Rotl => v.push(0x89),
      Self::I64Rotr => v.push(0x8A),
      Self::F32Abs => v.push(0x8B),
      Self::F32Neg => v.push(0x8C),
      Self::F32Ceil => v.push(0x8D),
      Self::F32Floor => v.push(0x8E),
      Self::F32Trunc => v.push(0x8F),
      Self::F32Nearest => v.push(0x90),
      Self::F32Sqrt => v.push(0x91),
      Self::F32Add => v.push(0x92),
      Self::F32Sub => v.push(0x93),
      Self::F32Mul => v.push(0x94),
      Self::F32Div => v.push(0x95),
      Self::F32Min => v.push(0x96),
      Self::F32Max => v.push(0x97),
      Self::F32Copysign => v.push(0x98),
      Self::F64Abs => v.push(0x99),
      Self::F64Neg => v.push(0x9A),
      Self::F64Ceil => v.push(0x9B),
      Self::F64Floor => v.push(0x9C),
      Self::F64Trunc => v.push(0x9D),
      Self::F64Nearest => v.push(0x9E),
      Self::F64Sqrt => v.push(0x9F),
      Self::F64Add => v.push(0xA0),
      Self::F64Sub => v.push(0xA1),
      Self::F64Mul => v.push(0xA2),
      Self::F64Div => v.push(0xA3),
      Self::F64Min => v.push(0xA4),
      Self::F64Max => v.push(0xA5),
      Self::F64Copysign => v.push(0xA6),
      Self::I32Eqz => v.push(0x45),
      Self::I32Eq => v.push(0x46),
      Self::I32Ne => v.push(0x47),
      Self::I32LtS => v.push(0x48),
      Self::I32LtU => v.push(0x49),
      Self::I32GtS => v.push(0x4A),
      Self::I32GtU => v.push(0x4B),
      Self::I32LeS => v.push(0x4C),
      Self::I32LeU => v.push(0x4D),
      Self::I32GeS => v.push(0x4E),
      Self::I32GeU => v.push(0x4F),
      Self::I64Eqz => v.push(0x50),
      Self::I64Eq => v.push(0x51),
      Self::I64Ne => v.push(0x52),
      Self::I64LtS => v.push(0x53),
      Self::I64LtU => v.push(0x54),
      Self::I64GtS => v.push(0x55),
      Self::I64GtU => v.push(0x56),
      Self::I64LeS => v.push(0x57),
      Self::I64LeU => v.push(0x58),
      Self::I64GeS => v.push(0x59),
      Self::I64GeU => v.push(0x5A),
      Self::F32Eq => v.push(0x5B),
      Self::F32Ne => v.push(0x5C),
      Self::F32Lt => v.push(0x5D),
      Self::F32Gt => v.push(0x5E),
      Self::F32Le => v.push(0x5F),
      Self::F32Ge => v.push(0x60),
      Self::F64Eq => v.push(0x61),
      Self::F64Ne => v.push(0x62),
      Self::F64Lt => v.push(0x63),
      Self::F64Gt => v.push(0x64),
      Self::F64Le => v.push(0x65),
      Self::F64Ge => v.push(0x66),
      Self::I32WrapI64 => v.push(0xA7),
      Self::I32TruncF32S => v.push(0xA8),
      Self::I32TruncF32U => v.push(0xA9),
      Self::I32TruncF64S => v.push(0xAA),
      Self::I32TruncF64U => v.push(0xAB),
      Self::I64ExtendI32S => v.push(0xAC),
      Self::I64ExtendI32U => v.push(0xAD),
      Self::I64TruncF32S => v.push(0xAE),
      Self::I64TruncF32U => v.push(0xAF),
      Self::I64TruncF64S => v.push(0xB0),
      Self::I64TruncF64U => v.push(0xB1),
      Self::F32ConvertI32S => v.push(0xB2),
      Self::F32ConvertI32U => v.push(0xB3),
      Self::F32ConvertI64S => v.push(0xB4),
      Self::F32ConvertI64U => v.push(0xB5),
      Self::F32DemoteF64 => v.push(0xB6),
      Self::F64ConvertI32S => v.push(0xB7),
      Self::F64ConvertI32U => v.push(0xB8),
      Self::F64ConvertI64S => v.push(0xB9),
      Self::F64ConvertI64U => v.push(0xBA),
      Self::F64PromoteF32 => v.push(0xBB),
      Self::I32ReinterpretF32 => v.push(0xBC),
      Self::I64ReinterpretF64 => v.push(0xBD),
      Self::F32ReinterpretI32 => v.push(0xBE),
      Self::F64ReinterpretI64 => v.push(0xBF),
      Self::I32Extend8S => v.push(0xC0),
      Self::I32Extend16S => v.push(0xC1),
      Self::I64Extend8S => v.push(0xC2),
      Self::I64Extend16S => v.push(0xC3),
      Self::I64Extend32S => v.push(0xC4),
      Self::I32TruncSatF32S => {
        v.push(0xFC);
        leb_u32(0, v);
      }
      Self::I32TruncSatF32U => {
        v.push(0xFC);
        leb_u32(1, v);
      }
      Self::I32TruncSatF64S => {
        v.push(0xFC);
        leb_u32(2, v);
      }
      Self::I32TruncSatF64U => {
        v.push(0xFC);
        leb_u32(3, v);
      }
      Self::I64TruncSatF32S => {
        v.push(0xFC);
        leb_u32(4, v);
      }
      Self::I64TruncSatF32U => {
        v.push(0xFC);
        leb_u32(5, v);
      }
      Self::I64TruncSatF64S => {
        v.push(0xFC);
        leb_u32(6, v);
      }
      Self::I64TruncSatF64U => {
        v.push(0xFC);
        leb_u32(7, v);
      }
    }
  }
}

impl ToBinary for Expression {
  fn push_to(self, v: &mut Vec<u8>) {
    for i in self.0 {
      i.push_to(v)
    }
    v.push(0x0B);
  }
}

impl ToBinary for char {
  fn push_to(self, v: &mut Vec<u8>) { let _ = self.encode_utf8(v); }
}

impl ToBinary for Name {
  fn push_to(self, v: &mut Vec<u8>) { encode_vec(self.0, v); }
}

impl ToBinary for ImportDesc {
  fn push_to(self, v: &mut Vec<u8>) {
    match self {
      Self::ImportFunc(x) => {
        v.push(0x00);
        x.push_to(v);
      }
      Self::ImportTable(x) => {
        v.push(0x01);
        x.push_to(v);
      }
      Self::ImportMem(x) => {
        v.push(0x02);
        x.push_to(v);
      }
      Self::ImportGlobal(x) => {
        v.push(0x03);
        x.push_to(v);
      }
    }
  }
}

impl ToBinary for Import {
  fn push_to(self, v: &mut Vec<u8>) {
    self.mod_name.push_to(v);
    self.name.push_to(v);
    self.desc.push_to(v);
  }
}

impl ToBinary for Table {
  fn push_to(self, v: &mut Vec<u8>) { self.0.push_to(v) }
}

impl ToBinary for Memory {
  fn push_to(self, v: &mut Vec<u8>) { self.0.push_to(v) }
}

impl ToBinary for Global {
  fn push_to(self, v: &mut Vec<u8>) {
    self.typ.push_to(v);
    self.init.push_to(v);
  }
}

impl ToBinary for ExportDesc {
  fn push_to(self, v: &mut Vec<u8>) {
    match self {
      Self::ExportFunc(x) => {
        v.push(0x00);
        x.push_to(v);
      }
      Self::ExportTable(x) => {
        v.push(0x01);
        x.push_to(v);
      }
      Self::ExportMem(x) => {
        v.push(0x02);
        x.push_to(v);
      }
      Self::ExportGlobal(x) => {
        v.push(0x03);
        x.push_to(v)
      }
    }
  }
}

impl ToBinary for Export {
  fn push_to(self, v: &mut Vec<u8>) {
    self.name.push_to(v);
    self.desc.push_to(v);
  }
}

fn get_func_indices(exprs: Vec<Expression>) -> Option<Vec<FuncIndex>> {
  let mut indices = Vec::new();
  for expr in exprs {
    if let Instruction::RefFunc(index) = expr.0[0] {
      if expr.0.len() == 1 {
        indices.push(index);
      }
      else {
        return None;
      }
    }
    else {
      return None;
    }
  }
  Some(indices)
}

impl ToBinary for ElemSegment {
  fn push_to(self, v: &mut Vec<u8>) {
    match self.mode {
      ElemMode::DeclarativeElem => {
        if let Some(is) = get_func_indices(self.init.clone()) {
          v.push(0x03);
          self.typ.push_to(v);
          encode_vec(is, v);
        }
        else {
          v.push(0x07);
          self.typ.push_to(v);
          encode_vec(self.init, v);
        }
      }
      ElemMode::PassiveElem => {
        if let Some(is) = get_func_indices(self.init.clone()) {
          v.push(0x01);
          self.typ.push_to(v);
          encode_vec(is, v);
        }
        else {
          v.push(0x05);
          self.typ.push_to(v);
          encode_vec(self.init, v);
        }
      }
      ElemMode::ActiveElem { table: TableIndex(t), offset } => {
        if let Some(is) = get_func_indices(self.init.clone()) {
          if t == 0 {
            v.push(0x00);
          }
          else {
            v.push(0x02);
            leb_u32(t, v);
          }
          offset.push_to(v);
          encode_vec(is, v);
        }
        else if t == 0 {
          v.push(0x04);
          offset.push_to(v);
          encode_vec(self.init, v);
        }
        else {
          v.push(0x06);
          leb_u32(t, v);
          offset.push_to(v);
          encode_vec(self.init, v);
        }
      }
    }
  }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Local(u32, ValueType);

impl ToBinary for Local {
  fn push_to(self, v: &mut Vec<u8>) {
    leb_u32(self.0, v);
    self.1.push_to(v);
  }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Code {
  locals: Vec<Local>,
  expr: Expression,
}

impl ToBinary for Code {
  fn push_to(self, v: &mut Vec<u8>) {
    let mut inner = Vec::new();
    encode_vec(self.locals, &mut inner);
    self.expr.push_to(&mut inner);
    leb_u32(inner.len() as u32, v);
    v.extend(inner);
  }
}

pub fn value_types_to_locals(mut vts: Vec<ValueType>) -> Vec<Local> {
  vts.sort();
  let mut locals = Vec::new();
  let mut last = vts[0].clone();
  let mut count = 0;
  for vt in &vts[1..] {
    if *vt == last {
      count += 1;
    } else {
      locals.push(Local(count, last));
      last = vt.clone();
    }
  }
  locals
}

impl ToBinary for u8 {
  fn push_to(self, v: &mut Vec<u8>) {
    v.push(self);
  }
}

impl ToBinary for DataSegment {
    fn push_to(self, v: &mut Vec<u8>) {
      match self.mode {
        DataMode::PassiveData => {
          v.push(0x01);
          encode_vec(self.init, v);
        },
        DataMode::ActiveData { memory: MemIndex(0), offset } => {
          v.push(0x00); 
          offset.push_to(v);
          encode_vec(self.init, v);
        }
        DataMode::ActiveData { memory, offset } => {
          v.push(0x02); 
          memory.push_to(v);
          offset.push_to(v);
          encode_vec(self.init, v);
        },
    }
    }
}

impl ToBinary for Module {
  fn push_to(self, v: &mut Vec<u8>) {
    v.append(&mut vec![0x00, 0x61, 0x73, 0x6D]); // magic
    v.append(&mut vec![0x01, 0x00, 0x00, 0x00]); // version
    if !self.types.is_empty() {
      v.push(0x01);
      let mut inner = Vec::new();
      encode_vec(self.types, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    if !self.imports.is_empty() {
      v.push(0x02);
      let mut inner = Vec::new();
      encode_vec(self.imports, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    let type_indices = self.funcs.iter().map(|f| f.typ).collect::<Vec<_>>();
    if !type_indices.is_empty() {
      v.push(0x03);
      let mut inner = Vec::new();
      encode_vec(type_indices, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    if !self.tables.is_empty() {
      v.push(0x04);
      let mut inner = Vec::new();
      encode_vec(self.tables, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    if !self.memories.is_empty() {
      v.push(0x05);
      let mut inner = Vec::new();
      encode_vec(self.memories, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    if !self.globals.is_empty() {
      v.push(0x06);
      let mut inner = Vec::new();
      encode_vec(self.globals, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    if !self.exports.is_empty() {
      v.push(0x07);
      let mut inner = Vec::new();
      encode_vec(self.exports, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    if let Some(StartFunction(i)) = self.entrypoint {
      v.push(0x08);
      let mut inner = Vec::new();
      i.push_to(&mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    if !self.elems.is_empty() {
      v.push(0x09);
      let mut inner = Vec::new();
      encode_vec(self.elems, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    let codes = self
      .funcs
      .iter()
      .map(|f| Code {
        locals: value_types_to_locals(f.locals.clone()),
        expr: f.body.clone(),
      })
      .collect::<Vec<_>>();
    if !codes.is_empty() {
      v.push(0x0A);
      let mut inner = Vec::new();
      encode_vec(codes, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }
    if !self.data.is_empty() {
      v.push(0x0B);
      let mut inner = Vec::new();
      encode_vec(self.data, &mut inner);
      leb_u32(inner.len() as u32, v);
      v.extend(inner);
    }

  }
}

impl Module {
  pub fn to_bytes(self) -> Vec<u8> {
    let mut v = Vec::new();
    self.push_to(&mut v);
    v
  }
}
