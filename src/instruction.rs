use std::{ops, mem, fmt};

#[derive(Debug, Clone)]
struct Register(usize);

#[derive(Debug, Clone, Copy)]
struct RawByte(u64);
#[derive(Debug, Clone, Copy)]
enum TaggedByte {
	Empty,
	U8(u8),
	U64(u64),
	F64(f64),
}

trait Byte: Clone + Copy + fmt::Debug {
	fn initial() -> Self;
	fn from_u8(u8) -> Self;
	fn to_u8(self) -> u8;
	fn from_u64(u64) -> Self;
	fn to_u64(self) -> u64;
	fn from_f64(f64) -> Self;
	fn to_f64(self) -> f64;
}

impl Byte for RawByte {
	fn initial() -> RawByte {
		RawByte(0)
	}
	fn from_u8(v: u8) -> RawByte {
		RawByte(v as u64)
	}
	fn to_u8(self) -> u8 {
		self.0 as u8
	}
	fn from_u64(v: u64) -> RawByte {
		RawByte(v)
	}
	fn to_u64(self) -> u64 {
		self.0
	}
	fn from_f64(v: f64) -> RawByte {
		RawByte(unsafe { mem::transmute(v) })
	}
	fn to_f64(self) -> f64 {
		unsafe { mem::transmute(self.0) }
	}
}

impl Byte for TaggedByte {
	fn initial() -> TaggedByte {
		TaggedByte::Empty
	}
	fn from_u8(v: u8) -> TaggedByte {
		TaggedByte::U8(v)
	}
	fn to_u8(self) -> u8 {
		match self {
			TaggedByte::U8(x) => x,
			_ => unimplemented!(),
		}
	}
	fn from_u64(v: u64) -> TaggedByte {
		TaggedByte::U64(v)
	}
	fn to_u64(self) -> u64 {
		match self {
			TaggedByte::U64(x) => x,
			_ => unimplemented!(),
		}
	}
	fn from_f64(v: f64) -> TaggedByte {
		TaggedByte::F64(v)
	}
	fn to_f64(self) -> f64 {
		match self {
			TaggedByte::F64(x) => x,
			_ => unimplemented!(),
		}
	}
}

impl Into<RawByte> for TaggedByte {
	fn into(self) -> RawByte {
		let v = match self {
			TaggedByte::Empty => 0,
			TaggedByte::U8(v) => v as u64,
			TaggedByte::U64(v) => v,
			TaggedByte::F64(v) => unsafe { mem::transmute(v) },
		};
		RawByte(v)
	}
}

impl Into<Instruction<RawByte>> for Instruction<TaggedByte> {
	fn into(self) -> Instruction<RawByte> {
		use self::Instruction::*;
		match self {
			Store(r, v) => Store(r, v.into()),
			Add8(a, b) => Add8(a, b),
			AddF2(a, b) => AddF2(a, b),
			Save(a) => Save(a),
			Jump(a) => Jump(a),
			Print(a) => Print(a),
		}
	}
}

const STACK_SIZE: usize = 512;

#[derive(Clone)]
struct Bytes<B: Byte>([B; STACK_SIZE]);

impl<B: Byte> Bytes<B> {
	fn new() -> Bytes<B> {
		Bytes([B::initial(); STACK_SIZE])
	}
}

impl<B: Byte> ops::Index<Register> for Bytes<B> {
	type Output = B;

	fn index(&self, reg: Register) -> &B {
		&self.0[reg.0]
	}
}

impl<B: Byte> ops::IndexMut<Register> for Bytes<B> {
	fn index_mut(&mut self, reg: Register) -> &mut B {
		&mut self.0[reg.0]
	}
}

#[derive(Debug, Clone)]
enum Instruction<B: Byte> {
	Store(Register, B),
	Add8(Register, Register),
	AddF2(Register, Register),
	Save(Register),
	Jump(Register),
	Print(Register),
}

fn run<B: Byte>(instructions: &[Instruction<B>]) -> Bytes<B> {
	let mut result = B::initial();
	let mut bytes = Bytes::new();

	use self::Instruction::*;

	let mut ptr = 0;

	let mut loops = 0;

	loop {
		loops += 1;
		if loops > 100 {
			panic!("over 100 loops");
		}

		if ptr >= instructions.len() {
			break;
		}
		let i = instructions[ptr].clone();
		println!("{:?}", i);
		ptr += 1;

		match i {
			Store(r, v) => {
				bytes[r] = v;
			}
			Add8(ar, br) => {
				let a = bytes[ar].to_u8();
				let b = bytes[br].to_u8();
				result = B::from_u8(a + b);
			}
			AddF2(ar, br) => {
				let a = bytes[ar].to_f64();
				let b = bytes[br].to_f64();
				result = B::from_f64(a + b);
			}
			Save(r) => {
				bytes[r] = result;
			}
			Print(r) => {
				println!("{:?}", bytes[r]);
			}
			Jump(i) => {
				ptr = bytes[i].to_u64() as usize;
			}
		}
	}

	bytes
}

#[test]
fn initial() {
	use self::Instruction::*;

	type Byte = TaggedByte;

	let instructions = vec![
		Store(Register(0), TaggedByte::F64(4.0)),
		Store(Register(3), TaggedByte::U64(2)),
		AddF2(Register(0), Register(0)),
		Save(Register(0)),
		Print(Register(0)),
		Jump(Register(3)),
	].into_iter().map(|x| x.into()).collect::<Vec<Instruction<Byte>>>();

	let res = run(&instructions);

	for b in res.0.iter().enumerate().rev() {;
		println!("{:?}", b);
	}

	assert!(false);
}
