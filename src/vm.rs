use crate::builtins::{self, Builtin};
use crate::compiler::{Bytecode, OpCode};
use crate::gc::GC;
use crate::object::{Error, FromString, FromVec, Object, Type};

#[cfg(feature = "debug")]
use std::io::Write;
use std::ptr;

#[cfg(feature = "debug")]
use crate::compiler::bytecode_to_human;

#[derive(Copy, Clone, Debug)]
struct Frame {
    /// Index of the current instruction
    ip: usize,

    /// Pointer to the index of the stack before function call started
    /// This is where the VM returns its stack to after the function returns
    base_pointer: u16,
}

impl Frame {
    #[inline(always)]
    fn new(ip: usize, base_pointer: u16) -> Self {
        Frame { ip, base_pointer }
    }
}

pub struct VM {
    stack: Vec<Object>,
    globals: Vec<Object>,
    frames: Vec<Frame>,
    instructions: Vec<u8>,
    ip: usize,
    bp: u16,

    constants: Vec<Object>,
    final_result: Object,
    gc: GC,
}

impl VM {
    /// Creates a new VM with an empty stack and callframes vector
    pub fn new() -> Self {
        let mut frames = Vec::with_capacity(32);
        frames.push(Frame::new(0, 0));

        Self {
            stack: Vec::with_capacity(32),
            globals: Vec::with_capacity(8),
            frames,
            instructions: Vec::new(),
            ip: 0,
            bp: 0,

            constants: Vec::new(),
            gc: GC::new(),
            final_result: Object::null(),
        }
    }

    /// Get a local variable (stored on the stack)
    /// The passed index is the relative position to the base pointer of the current callframe
    /// Performance: Skipping the bounds check here does not yield any significant performance improvement
    #[inline(always)]
    fn get_local(&self, rel_idx: u16) -> Object {
        self.stack[self.bp as usize + rel_idx as usize]
    }

    /// Store a local variable (on the stack)
    /// The passed index is the relative position to the base pointer of the current callframe
    #[inline(always)]
    fn set_local(&mut self, rel_idx: u16, value: Object) {
        self.stack[self.bp as usize + rel_idx as usize] = value;
    }

    /// Reads a u16 value from the current position in the instructions array
    #[inline(always)]
    fn read_u8(&mut self) -> u8 {
        let v = unsafe { *self.instructions.get_unchecked(self.ip) };
        self.ip += 1;
        v
    }

    /// Reads a u16 value from the current position in the instructions array
    #[inline(always)]
    fn read_u16(&mut self) -> u16 {
        let start = self.ip;
        self.ip += 2;
        let bytes = unsafe { self.instructions.get_unchecked(start..self.ip) };
        bytes[0] as u16 | (bytes[1] as u16) << 8
    }

    /// Sets the instruction pointer to the given value
    #[inline]
    fn jump(&mut self, ip: u16) {
        self.ip = ip as usize;
    }

    /// Reads the next OpCode from the instructions vector
    /// This function still accounts for 25-35% of runtime right now...
    #[inline(always)]
    fn next(&mut self) -> OpCode {
        // Safety: if compiler did its job correctly, IP will always be in bounds
        // Performance: skipping the bounds check yields a 22% performance improvement
        let byte = unsafe { *self.instructions.get_unchecked(self.ip) };
        self.ip += 1;
        OpCode::from(byte)
    }

    /// Pop an object off the stack
    /// This is like `Vec::pop`, but without checking if it's empty first.
    /// Performance: -25% over a regular call to `Vec::pop()`
    #[inline(always)]
    fn pop(&mut self) -> Object {
        debug_assert!(!self.stack.is_empty());

        // Safety: if the compiler and VM are implemented correctly, the stack will never be empty
        unsafe {
            let new_len = self.stack.len() - 1;
            self.stack.set_len(new_len);
            ptr::read(self.stack.as_ptr().add(new_len))
        }
    }

    /// Push a new object on the stack
    #[inline(always)]
    fn push(&mut self, obj: Object) {
        self.stack.push(obj)
    }

    /// Pop a callframe and return IP to the IP of the last callframe
    /// This also truncates the stack back to SP from when this frame was pushed
    #[inline(always)]
    fn popframe(&mut self) {
        // pop frame and return stack to frame's base pointer
        let frame = self.frames.pop().unwrap();
        self.stack.truncate(frame.base_pointer as usize);

        // copy base pointer and instruction pointer out of new current frame
        // this yields an enormous performance improvement
        let frame = self.frames.last().unwrap();
        self.ip = frame.ip;
        self.bp = frame.base_pointer;
    }

    /// Push new callframe with the given IP and Base Pointer
    #[inline(always)]
    fn pushframe(&mut self, ip: u32, base_pointer: u16) {
        // store current IP into the frame that we're leaving
        // so we can return to it later
        let frame = self.frames.last_mut().unwrap();
        frame.ip = self.ip;

        // push new frame and copy over IP and BP
        // this somehow yields an enormous performance improvent
        self.frames.push(Frame::new(ip as usize, base_pointer));
        self.ip = ip as usize;
        self.bp = base_pointer;
    }

    /// Executes the given Bytecode inside the context of this VM
    pub fn run(&mut self, code: Bytecode) -> Result<Object, Error> {
        #[cfg(feature = "debug")]
        {
            println!("Bytecode (raw)= \n{:?}", &code.instructions);
            print!(
                "Bytecode (human)= {}\n",
                bytecode_to_human(&code.instructions, true)
            );
            println!("{:16}= {:?}", "Constants", code.constants);
            println!("{:16}= {:?}", "Frames", self.frames);
        }

        // reset some state
        self.instructions = code.instructions;
        self.ip = 0;
        self.bp = 0;
        self.frames[0].ip = 0;
        self.frames[0].base_pointer = 0;

        // Keep your friends close
        self.constants = code.constants;

        // Construct a new garbage collector
        // And allow to manage memory for constants
        for c in &self.constants {
            self.gc.maybe_trace(*c)
        }

        #[cfg(feature = "debug")]
        let mut debug_pause = 0;

        #[cfg(feature = "debug")]
        // Buffer used to capture input from stdin during stepped debugging
        let mut buffer = String::new();

        loop {
            #[cfg(feature = "debug")]
            {
                println!(
                    "{:16}= {}/{}: {}",
                    "Instruction",
                    self.ip,
                    self.instructions.len() - 1,
                    // This prints the OpCode along with all of its operand values (in decimal form)
                    bytecode_to_human(&self.instructions[self.ip..], false)
                        .split(" ")
                        .next()
                        .unwrap()
                );
                print!("{:16}= [", "Globals");
                for (i, v) in self.globals.iter().enumerate() {
                    print!("{}{}: {:?}", if i > 0 { ", " } else { "" }, i, v)
                }
                println!("]");
                print!("{:16}= [", "Stack");
                for (i, v) in self.stack.iter().enumerate() {
                    print!("{}{}: {:?}", if i > 0 { ", " } else { "" }, i, v)
                }
                println!("]");

                if debug_pause == 0 {
                    print!("{} ", ">".repeat(40));
                    std::io::stdout().flush().unwrap();
                    buffer.clear();
                    std::io::stdin().read_line(&mut buffer).unwrap();
                    debug_pause = buffer.trim().parse().unwrap_or(1) - 1;
                } else {
                    println!("{} ", ">".repeat(40));
                    debug_pause -= 1;
                }
            }

            match self.next() {
                OpCode::Const => self.op_const()?,
                OpCode::Pop => self.op_pop()?,
                OpCode::True => self.op_true()?,
                OpCode::False => self.op_false()?,
                OpCode::Add => self.op_add()?,
                OpCode::Subtract => self.op_sub()?,
                OpCode::Divide => self.op_div()?,
                OpCode::Multiply => self.op_mul()?,
                OpCode::Gt => self.op_gt()?,
                OpCode::Gte => self.op_gte()?,
                OpCode::Lt => self.op_lt()?,
                OpCode::Lte => self.op_lte()?,
                OpCode::Eq => self.op_eq()?,
                OpCode::Neq => self.op_neq()?,
                OpCode::And => self.op_and()?,
                OpCode::Or => self.op_or()?,
                OpCode::Not => self.op_not()?,
                OpCode::Modulo => self.op_rem()?,
                OpCode::Negate => self.op_negate()?,
                OpCode::Jump => self.op_jump()?,
                OpCode::JumpIfFalse => self.op_jumpiffalse()?,
                OpCode::Null => self.op_null()?,
                OpCode::Return => self.op_return()?,
                OpCode::ReturnValue => self.op_returnvalue()?,
                OpCode::Call => self.op_call()?,
                OpCode::CallBuiltin => self.op_callbuiltin()?,
                OpCode::GetLocal => self.op_getlocal()?,
                OpCode::SetLocal => self.op_setlocal()?,
                OpCode::GetGlobal => self.op_getglobal()?,
                OpCode::SetGlobal => self.op_setglobal()?,
                OpCode::GtLocalConst => self.op_gtlocalconst()?,
                OpCode::GteLocalConst => self.op_gtelocalconst()?,
                OpCode::LtLocalConst => self.op_ltlocalconst()?,
                OpCode::LteLocalConst => self.op_ltelocalconst()?,
                OpCode::EqLocalConst => self.op_eqlocalconst()?,
                OpCode::NeqLocalConst => self.op_neqlocalconst()?,
                OpCode::AddLocalConst => self.op_addlocalconst()?,
                OpCode::SubtractLocalConst => self.op_subtractlocalconst()?,
                OpCode::MultiplyLocalConst => self.op_multiplylocalconst()?,
                OpCode::DivideLocalConst => self.op_dividelocalconst()?,
                OpCode::ModuloLocalConst => self.op_modulolocalconst()?,
                OpCode::Array => self.op_array()?,
                OpCode::IndexGet => self.op_indexget()?,
                OpCode::IndexSet => self.op_indexset()?,
                OpCode::Halt => return self.op_halt(),
            }
        }
    }
}

macro_rules! impl_binary_op_method {
    ($name:tt, $op:tt) => {
        #[inline(always)]
        fn $name(&mut self) -> Result<(), Error> {
            let right = self.pop();
            let left = self.pop();
            let result = left.$op(right, &mut self.gc)?;
            self.push(result);
            Ok(())
        }
    };
}
macro_rules! impl_binary_const_local_op_method {
    ($self:expr, $op:tt) => {{
        let local_idx = $self.read_u16();
        let left = $self.get_local(local_idx);
        let constant_idx = $self.read_u16();
        let right = $self.constants[constant_idx as usize];
        let result = left.$op(right, &mut $self.gc).unwrap();
        $self.push(result);
        Ok(())
    }};
}

impl VM {
    #[inline(always)]
    fn op_const(&mut self) -> Result<(), Error> {
        let idx = self.read_u16();
        let value = self.constants[idx as usize];
        self.push(value);
        Ok(())
    }

    #[inline(always)]
    fn op_setglobal(&mut self) -> Result<(), Error> {
        let idx = self.read_u16() as usize;
        let value = self.pop();
        while self.globals.len() <= idx {
            self.globals.push(Object::null());
        }
        self.globals[idx] = value;
        Ok(())
    }

    #[inline(always)]
    fn op_getglobal(&mut self) -> Result<(), Error> {
        let idx = self.read_u16();
        let value = self.globals[idx as usize];
        self.push(value);
        Ok(())
    }
    #[inline(always)]
    fn op_setlocal(&mut self) -> Result<(), Error> {
        let idx = self.read_u16();
        let value = self.pop();
        self.set_local(idx, value);
        Ok(())
    }
    #[inline(always)]
    fn op_getlocal(&mut self) -> Result<(), Error> {
        let idx = self.read_u16();
        let value = self.get_local(idx);
        self.push(value);
        Ok(())
    }
    // TODO: Make JUMP* opcodes relative?
    #[inline(always)]
    fn op_jump(&mut self) -> Result<(), Error> {
        let pos = self.read_u16();
        self.jump(pos);

        // collect garbage on every jump instruction
        // gc.run(&[&self.stack, &constants, &self.globals, &[final_result]]);
        Ok(())
    }
    #[inline(always)]
    fn op_jumpiffalse(&mut self) -> Result<(), Error> {
        let condition = self.pop();
        let evaluation = match condition.tag() {
            Type::Bool => Ok(condition.as_bool()),
            _ => Err(Error::TypeError(format!("kan object met type {} niet gebruiken als voorwaarde. Gebruik evt. bool() om te type casten naar boolean.", condition.tag()))),
        }?;

        let pos = self.read_u16();
        if !evaluation {
            self.jump(pos);
        }
        Ok(())
    }
    #[inline(always)]
    fn op_pop(&mut self) -> Result<(), Error> {
        self.final_result = self.pop();
        Ok(())
    }
    #[inline(always)]
    fn op_null(&mut self) -> Result<(), Error> {
        self.push(Object::null());
        Ok(())
    }
    #[inline(always)]
    fn op_true(&mut self) -> Result<(), Error> {
        self.push(Object::bool(true));
        Ok(())
    }
    #[inline(always)]
    fn op_false(&mut self) -> Result<(), Error> {
        self.push(Object::bool(false));
        Ok(())
    }

    impl_binary_op_method!(op_add, add);
    impl_binary_op_method!(op_sub, sub);
    impl_binary_op_method!(op_div, div);
    impl_binary_op_method!(op_mul, mul);
    impl_binary_op_method!(op_gt, gt);
    impl_binary_op_method!(op_gte, gte);
    impl_binary_op_method!(op_lt, lt);
    impl_binary_op_method!(op_lte, lte);
    impl_binary_op_method!(op_eq, eq);
    impl_binary_op_method!(op_neq, neq);
    impl_binary_op_method!(op_rem, rem);
    impl_binary_op_method!(op_and, and);
    impl_binary_op_method!(op_or, or);

    #[inline(always)]
    fn op_not(&mut self) -> Result<(), Error> {
        let left = self.pop();
        if left.tag() != Type::Bool {
            return Err(Error::TypeError(format!(
                "kan ! (niet) niet toepassen op objecten van type {}",
                left.tag()
            )));
        }
        let result = Object::bool(!left.as_bool());
        self.push(result);
        Ok(())
    }

    #[inline(always)]
    fn op_negate(&mut self) -> Result<(), Error> {
        let left = self.pop();
        let result = match left.tag() {
            Type::Float => unsafe { Object::float(-left.as_f64_unchecked(), &mut self.gc) },
            Type::Int => Object::int(-left.as_int()),
            _ => {
                return Err(Error::TypeError(format!(
                    "kan objecten met type {} niet omdraaien",
                    left.tag()
                )))
            }
        };
        self.push(result);
        Ok(())
    }

    #[inline(always)]
    fn op_call(&mut self) -> Result<(), Error> {
        let num_args = self.read_u8();
        let base_pointer = self.stack.len() as u16 - 1 - num_args as u16;
        let obj = self.pop();
        if obj.tag() != Type::Function {
            return Err(Error::TypeError(format!(
                "object van type {} is not aanroepbaar",
                obj.tag()
            )));
        }
        let [ip, num_locals] = obj.as_function();

        // Make room on the stack for any local variables defined inside this function
        for _ in 0..num_locals - num_args as u32 {
            self.push(Object::null());
        }

        self.pushframe(ip, base_pointer);
        Ok(())
    }

    #[inline(always)]
    fn op_callbuiltin(&mut self) -> Result<(), Error> {
        let builtin = self.read_u8();
        let num_args = self.read_u8() as usize;
        let mut args = Vec::with_capacity(num_args);
        for _ in 0..num_args {
            args.push(self.pop());
        }
        args.reverse();
        let builtin = unsafe { std::mem::transmute::<u8, Builtin>(builtin) };
        let result = builtins::call(builtin, &args, &mut self.gc).unwrap();
        self.push(result);
        Ok(())
    }

    #[inline(always)]
    fn op_returnvalue(&mut self) -> Result<(), Error> {
        let result = self.pop();
        self.popframe();
        self.push(result);
        Ok(())
    }

    #[inline(always)]
    fn op_return(&mut self) -> Result<(), Error> {
        self.popframe();
        self.push(Object::null());
        Ok(())
    }

    #[inline(always)]
    fn op_gtlocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, gt)
    }
    #[inline(always)]
    fn op_gtelocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, gte)
    }
    #[inline(always)]
    fn op_ltlocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, lt)
    }
    #[inline(always)]
    fn op_ltelocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, lte)
    }
    #[inline(always)]
    fn op_eqlocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, eq)
    }
    #[inline(always)]
    fn op_neqlocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, neq)
    }
    #[inline(always)]
    fn op_addlocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, add)
    }
    #[inline(always)]
    fn op_subtractlocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, sub)
    }
    #[inline(always)]
    fn op_multiplylocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, mul)
    }
    #[inline(always)]
    fn op_dividelocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, div)
    }
    #[inline(always)]
    fn op_modulolocalconst(&mut self) -> Result<(), Error> {
        impl_binary_const_local_op_method!(self, rem)
    }

    #[inline(always)]
    fn op_array(&mut self) -> Result<(), Error> {
        let length = self.read_u16();
        let mut vec = Vec::with_capacity(length as usize);
        for _ in 0..length {
            vec.push(self.pop());
        }
        vec.reverse();
        let obj = Object::array(vec, &mut self.gc);
        self.push(obj);
        Ok(())
    }
    #[inline(always)]
    fn op_indexget(&mut self) -> Result<(), Error> {
        let index = self.pop();
        let left = self.pop();
        let result = index_get(left, index, &mut self.gc)?;
        self.push(result);
        Ok(())
    }
    #[inline(always)]
    fn op_indexset(&mut self) -> Result<(), Error> {
        let value = self.pop();
        let index = self.pop();
        let left = self.pop();
        let value = index_set(left, index, value)?;
        self.push(value);
        Ok(())
    }
    #[inline(always)]
    fn op_halt(&mut self) -> Result<Object, Error> {
        self.gc.untrace(self.final_result);
        Ok(self.final_result)
    }
}

fn index_get(left: Object, index: Object, gc: &mut GC) -> Result<Object, Error> {
    if index.tag() != Type::Int {
        return Err(Error::TypeError(format!(
            "lijst index moet een integer zijn, geen {}",
            index.tag()
        )));
    }

    let result = match left.tag() {
        Type::Array => index_get_array(left, index.as_int()),
        Type::String => index_get_string(left, index.as_int(), gc),
        _ => {
            return Err(Error::TypeError(format!(
                "kan niet indexeren in objecten van type {}",
                left.tag()
            )))
        }
    }?;

    Ok(result)
}

fn index_get_array(obj: Object, mut index: isize) -> Result<Object, Error> {
    let array = obj.as_vec();
    if index < 0 {
        index += array.len() as isize;
    }
    let index = index as usize;
    if index >= array.len() {
        return Err(Error::IndexError(
            "lijst index valt buiten de lijst".to_string(),
        ));
    }

    Ok(array[index])
}

fn index_get_string(obj: Object, mut index: isize, gc: &mut GC) -> Result<Object, Error> {
    let str = obj.as_str();
    if index < 0 {
        index += str.chars().count() as isize;
    }
    let index = index as usize;
    if index >= str.len() {
        return Err(Error::IndexError(
            "lijst index valt buiten de lijst".to_string(),
        ));
    }

    let ch = str.chars().nth(index).unwrap();
    let result = Object::string(ch.to_string(), gc);
    Ok(result)
}

fn index_set(mut left: Object, index: Object, value: Object) -> Result<Object, Error> {
    if index.tag() != Type::Int {
        return Err(Error::TypeError(format!(
            "lijst index moet een integer zijn, geen {}",
            index.tag()
        )));
    }
    match left.tag() {
        Type::Array => index_set_array(left.as_vec_mut(), index.as_int(), value)?,
        Type::String => index_set_string(left.as_string_mut(), index.as_int(), value)?,
        _ => {
            return Err(Error::TypeError(format!(
                "kan niet indexeren in objecten van type {}",
                left.tag()
            )))
        }
    }

    Ok(value)
}

fn index_set_array(array: &mut Vec<Object>, mut index: isize, value: Object) -> Result<(), Error> {
    if index < 0 {
        index += array.len() as isize;
    }
    let index = index as usize;
    if index >= array.len() {
        return Err(Error::IndexError(
            "lijst index valt buiten de lijst".to_string(),
        ));
    }
    array[index] = value;
    Ok(())
}

fn index_set_string(string: &mut String, mut index: isize, value: Object) -> Result<(), Error> {
    let strlen = string.chars().count();
    if index < 0 {
        index += strlen as isize;
    }
    let index = index as usize;
    if index >= strlen {
        return Err(Error::IndexError(
            "lijst index valt buiten de lijst".to_string(),
        ));
    }

    if value.tag() != Type::String {
        return Err(Error::TypeError(
            "kan geen niet-string invoegen op string object".to_string(),
        ));
    }

    string.replace_range(
        string
            .char_indices()
            .nth(index)
            .map(|(pos, ch)| (pos..pos + ch.len_utf8()))
            .unwrap(),
        value.as_str(),
    );

    Ok(())
}
