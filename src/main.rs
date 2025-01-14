use regalloc2::{
    Algorithm, Allocation, Block, Edit, Inst, InstOrEdit, InstRange, MachineEnv, Operand, PReg,
    PRegSet, RegClass, RegallocOptions, VReg,
};
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;

fn setup_logger() -> Result<(), fern::InitError> {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{} {}] {}",
                record.level(),
                record.target(),
                message
            ))
        })
        .level(log::LevelFilter::Trace)
        .chain(std::io::stdout())
        .chain(
            fern::Dispatch::new()
                .level(log::LevelFilter::Error)
                .chain(fern::Panic),
        )
        .apply()?;
    Ok(())
}

fn main() {
    setup_logger().unwrap();
    let (input, algo, num_gp_regs, num_vec_regs) = get_input();
    let builder = IrBuilder::new();
    let ir = builder.build(input.lines().collect());
    println!("{:#?}", ir);
    let output_res = regalloc2::run(
        &ir,
        &MachineEnv {
            preferred_regs_by_class: [
                (0..num_gp_regs)
                    .into_iter()
                    .map(|num| PReg::new(num, RegClass::Int))
                    .collect(),
                vec![],
                (0..num_vec_regs)
                    .into_iter()
                    .map(|num| PReg::new(num, RegClass::Vector))
                    .collect(),
            ],
            non_preferred_regs_by_class: [vec![], vec![], vec![]],
            scratch_by_class: [None, None, None],
            fixed_stack_slots: vec![],
        },
        &RegallocOptions {
            verbose_log: true,
            validate_ssa: true,
            algorithm: algo,
        },
    );
    // let output = output_res.unwrap_or_else(|e| -> {panic!("bad error sorry {:?}", e);});
    let output = match output_res {
        Ok(r) => r,
        Err(e) => panic!("bad error sorry: {:?}", e),
    };
    println!("{:#?}\n", output);

    let mut finalcode = String::new();
    for blocknum in 0..ir.blocks.len() {
        finalcode.push_str(&format!("block {blocknum}\n"));
        for out in output.block_insts_and_edits(&ir, Block::new(blocknum)) {
            match out {
                InstOrEdit::Inst(inst) => {
                    let instn = ir.find_instn(inst).unwrap();
                    let allocs = output.inst_allocs(inst);
                    finalcode.push_str(&format!("\t{}", instn.rewrite(allocs)));
                }
                InstOrEdit::Edit(edit) => match edit {
                    Edit::Move { from, to } => {
                        finalcode.push_str(&format!(
                            "\tmove {} to {}",
                            from.to_string(),
                            to.to_string()
                        ));
                    }
                },
            }
            finalcode.push('\n');
        }
        finalcode.push('\n');
    }
    println!("{}", finalcode);
}

static MAX_VREG_GEN_NO: AtomicUsize = AtomicUsize::new(0);
static MAX_VREG_VEC_NO: AtomicUsize = AtomicUsize::new(0);

fn get_reg_class(name: &str) -> RegClass {
    match &name[..2] {
        "vg" => RegClass::Int,
        "vv" => RegClass::Vector,
        _ => panic!("unhandled regclass: {:?}", name),
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct TypedVReg {
    idx: usize,
    class: RegClass,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct TypedVRegName {
    name: String,
    class: RegClass,
}
impl TypedVRegName {
    fn from_str(name: &str) -> Self {
        Self {
            name: name[2..].to_string(),
            class: get_reg_class(name),
        }
    }
}

#[derive(Clone, Debug)]
struct VRegIdx {
    name2idx: HashMap<TypedVRegName, TypedVReg>,
    idx: usize,
}
impl VRegIdx {
    fn new() -> Self {
        Self {
            name2idx: HashMap::new(),
            idx: 0,
        }
    }
    fn add_idx(&mut self, name: &TypedVRegName) -> usize {
        assert!(!self.name2idx.contains_key(&name));
        let this_idx = match name.class {
            RegClass::Int | RegClass::Vector => self.idx,
            _ => panic!("bad class: {:?}", name.class),
        };
        let tvreg = TypedVReg {
            idx: this_idx,
            class: name.class,
        };
        self.name2idx.insert(name.clone(), tvreg);
        self.idx += 1;
        match name.class {
            RegClass::Int | RegClass::Vector => self.idx += 1,
            _ => panic!("bad class: {:?}", name.class),
        }
        return this_idx;
    }
    fn get_idx(&self, name: &TypedVRegName) -> usize {
        match self.name2idx.contains_key(&name) {
            true => (),
            false => panic!("name2idx doesn't have key {:?}", name),
        }
        return self.name2idx.get(name).unwrap().idx;
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct Ir {
    blocks: Vec<BasicBlock>,
    vidx: VRegIdx,
}

impl Ir {
    fn find_instn(&self, insn: Inst) -> Option<&Instn> {
        for bb in self.blocks.iter() {
            for instn in bb.instns.iter() {
                if instn.index == insn {
                    return Some(instn);
                }
            }
        }
        None
    }
}

impl regalloc2::Function for Ir {
    fn num_insts(&self) -> usize {
        self.blocks.iter().map(|block| block.instns.len()).sum()
    }

    fn entry_block(&self) -> Block {
        self.blocks[0].index
    }

    fn num_blocks(&self) -> usize {
        self.blocks.len()
    }

    fn block_insns(&self, block: Block) -> regalloc2::InstRange {
        let bb = &self.blocks[block.index()];
        let first_inst_index = bb.instns[0].index;
        let last_inst_index = bb.instns.last().unwrap().index;
        InstRange::new(first_inst_index, last_inst_index.next())
    }

    fn block_succs(&self, block: Block) -> &[Block] {
        &self.blocks[block.index()].succs
    }

    fn block_preds(&self, block: Block) -> &[Block] {
        &self.blocks[block.index()].preds
    }

    fn block_params(&self, block: Block) -> &[VReg] {
        &self.blocks[block.index()].params
    }

    fn is_ret(&self, insn: Inst) -> bool {
        let instn = self
            .find_instn(insn)
            .expect(&format!("Failed to find instruction with index {:?}", insn));
        instn.is_ret()
    }

    fn is_branch(&self, insn: Inst) -> bool {
        let instn = self
            .find_instn(insn)
            .expect(&format!("Failed to find instruction with index {:?}", insn));
        instn.is_branch()
    }

    fn branch_blockparams(&self, block: Block, insn: Inst, succ_idx: usize) -> &[VReg] {
        let bb = &self.blocks[block.index()];
        for instn in bb.instns.iter() {
            if instn.index == insn {
                return instn.blockparams_to(succ_idx);
            }
        }
        eprintln!("The instruction doesnt exist in the block");
        &[]
    }

    fn inst_operands(&self, insn: Inst) -> &[Operand] {
        let instn = self
            .find_instn(insn)
            .expect(&format!("Failed to find instruction with index {:?}", insn));
        instn.operands()
    }

    fn num_vregs(&self) -> usize {
        get_max_vreg(RegClass::Int) + get_max_vreg(RegClass::Vector) + 1
    }

    fn inst_clobbers(&self, _insn: Inst) -> regalloc2::PRegSet {
        PRegSet::empty()
    }

    fn spillslot_size(&self, _regclass: RegClass) -> usize {
        1
    }
}

#[derive(Clone, Debug)]
struct IrBuilder {
    blocks: Vec<BasicBlock>,
    blockindex: usize,
    blockparams: Vec<VReg>,
    instns: Vec<Instn>,
    next_instn_index: usize,
    succs: Vec<Block>,
    vidx: VRegIdx,
}

impl IrBuilder {
    fn new() -> Self {
        Self {
            blocks: vec![],
            blockindex: 0,
            blockparams: vec![],
            instns: vec![],
            next_instn_index: 0,
            succs: vec![],
            vidx: VRegIdx::new(),
        }
    }

    fn build(mut self, lines: Vec<&str>) -> Ir {
        for line in lines {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            let words: Vec<&str> = line.split(" ").collect();
            match words[0] {
                "block" => {
                    if !self.instns.is_empty() {
                        self.end_bb();
                    }
                    if words.len() < 2 {
                        panic!("Expected the block definition to have a block number");
                    }
                    self.blockindex = words[1]
                        .parse::<usize>()
                        .expect("Block numbers should be valid `usize`s");
                    for word in &words[2..] {
                        assert!(
                            word.starts_with("vg") || word.starts_with("vv"),
                            "Block params must have valid virtual register names"
                        );
                        assert!(word.len() > 2, "Virtual registers should have a number");
                        let vr = parse_vreg(&word[2..], &self.vidx);
                        self.blockparams.push(vr);
                    }
                }
                "if" => {
                    let instn = Instn::cond_branch(words, self.next_instn_index, &mut self.vidx);
                    self.succs.push(instn.to());
                    self.succs.push(instn.elseto());
                    self.instns.push(instn);
                    println!("{:?} {:?}", self.next_instn_index, line);
                    self.next_instn_index += 1;
                }
                "goto" => {
                    let instn = Instn::uncond_branch(words, self.next_instn_index, &mut self.vidx);
                    self.succs.push(instn.to());
                    self.instns.push(instn);
                    println!("{:?} {:?}", self.next_instn_index, line);
                    self.next_instn_index += 1;
                }
                "output" => {
                    assert_eq!(words.len(), 2);
                    let reg = parse_vreg(words[1], &self.vidx);
                    (&mut self.instns).push(Instn {
                        index: Inst::new(self.next_instn_index),
                        instntype: InstnType::Output([Operand::reg_use(reg)]),
                    });
                    println!("{:?} {:?}", self.next_instn_index, line);
                    self.next_instn_index += 1;
                }
                "return" => {
                    assert_eq!(words.len(), 1);
                    self.instns.push(Instn {
                        index: Inst::new(self.next_instn_index),
                        instntype: InstnType::Return,
                    });
                    println!("{:?} {:?}", self.next_instn_index, line);
                    self.next_instn_index += 1;
                }
                v => {
                    if v.starts_with("#") {
                        continue;
                    } else if v.starts_with("vg") || v.starts_with("vv") {
                        let parts: Vec<&str> = line.split("=").collect();
                        assert_eq!(parts.len(), 2);
                        // let class: RegClass = get_reg_class(v);
                        let outreg = parse_vreg_def(parts[0].trim(), &mut self.vidx);
                        let input: Vec<&str> = parts[1].trim().split(" ").collect();
                        assert!(!input.is_empty());
                        let cmd = input[0];
                        if cmd == "input" {
                            self.instns
                                .push(Instn::input(outreg, self.next_instn_index));
                        } else {
                            self.instns.push(Instn::new(
                                cmd,
                                outreg,
                                &input[1..],
                                self.next_instn_index,
                                &mut self.vidx,
                                false,
                            ));
                        }
                        println!("{:?} {:?}", self.next_instn_index, line);
                        self.next_instn_index += 1;
                    } else {
                        panic!("Unrecognized word: {}", v);
                    }
                }
            };
        }
        self.end_bb();
        self.compute_preds();
        Ir {
            blocks: self.blocks,
            vidx: self.vidx,
        }
    }

    fn end_bb(&mut self) {
        self.blocks.push(BasicBlock {
            index: Block::new(self.blockindex),
            succs: self.succs.clone(),
            preds: vec![],
            params: self.blockparams.clone(),
            instns: self.instns.clone(),
        });
        self.succs.clear();
        self.blockparams.clear();
        self.instns.clear();
    }

    fn compute_preds(&mut self) {
        for i in 0..self.blocks.len() {
            let bb = self.blocks[i].clone();
            for succindex in bb.succs.iter() {
                self.blocks[succindex.index()].preds.push(bb.index);
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Instn {
    index: Inst,
    instntype: InstnType,
}

impl Instn {
    fn to(&self) -> Block {
        match &self.instntype {
            InstnType::CondBranch { to, .. } => to.0,
            InstnType::Branch { to, .. } => to.clone(),
            _ => panic!("Attempting to find a `to` on a non-branch instruction"),
        }
    }

    fn elseto(&self) -> Block {
        match &self.instntype {
            InstnType::CondBranch { elseto, .. } => elseto.0,
            _ => panic!("Attempting to find an `elseto` on a non-cond-branch instruction"),
        }
    }

    fn is_ret(&self) -> bool {
        match &self.instntype {
            InstnType::Return => true,
            _ => false,
        }
    }

    fn is_branch(&self) -> bool {
        match &self.instntype {
            InstnType::CondBranch { .. } => true,
            InstnType::Branch { .. } => true,
            _ => false,
        }
    }

    fn blockparams_to(&self, succidx: usize) -> &[VReg] {
        match &self.instntype {
            InstnType::CondBranch { to, elseto, .. } => {
                if succidx == 0 {
                    &to.1
                } else if succidx == 1 {
                    &elseto.1
                } else {
                    eprintln!(
                        "There is no {succidx} successor for instruction {:?}",
                        self.index
                    );
                    &[]
                }
            }
            InstnType::Branch { branchargs, .. } => {
                if succidx == 0 {
                    &branchargs
                } else {
                    eprintln!(
                        "There is no {succidx} successor for instruction {:?}",
                        self.index
                    );
                    &[]
                }
            }
            _ => {
                eprintln!("The instn isnt a branch");
                &[]
            }
        }
    }

    fn operands(&self) -> &[Operand] {
        match &self.instntype {
            InstnType::Normal { operands, .. } => &operands,
            InstnType::CondBranch { args, .. } => args.as_slice(),
            InstnType::Input(op) => op.as_slice(),
            InstnType::Output(op) => op.as_slice(),
            _ => {
                //println!("No operands for instruction {:?}", self.index);
                &[]
            }
        }
    }
}

#[derive(Clone, Debug)]
enum InstnOperand {
    /// An index into a vector of operands
    Operand(usize),
    Constant(String),
}

#[derive(Clone, Debug)]
enum InstnType {
    Normal {
        cmd: String,
        /// First operand is the output operand
        args: Vec<InstnOperand>,
        operands: Vec<Operand>,
    },
    CondBranch {
        cmp: String,
        args: [Operand; 2],
        to: (Block, Vec<VReg>),
        elseto: (Block, Vec<VReg>),
    },
    Branch {
        to: Block,
        branchargs: Vec<VReg>,
    },
    Output([Operand; 1]),
    Input([Operand; 1]),
    Return,
}

impl Instn {
    fn new(
        cmd: &str,
        outreg: VReg,
        args: &[&str],
        index: usize,
        vidx: &mut VRegIdx,
        is_def: bool,
    ) -> Self {
        let mut operands = vec![Operand::reg_def(outreg.clone())];
        let mut parsedargs = vec![];
        for arg in args {
            if arg.starts_with("vg") || arg.starts_with("vv") {
                if is_def {
                    operands.push(Operand::reg_use(parse_vreg_def(arg, vidx)));
                } else {
                    operands.push(Operand::reg_use(parse_vreg(arg, vidx)));
                }
                parsedargs.push(InstnOperand::Operand(operands.len() - 1));
            } else {
                parsedargs.push(InstnOperand::Constant(arg.to_string()));
            }
        }
        Self {
            index: Inst::new(index),
            instntype: InstnType::Normal {
                cmd: cmd.to_string(),
                args: parsedargs,
                operands,
            },
        }
    }

    fn cond_branch(words: Vec<&str>, index: usize, vidx: &mut VRegIdx) -> Self {
        assert!(words.len() >= 9);
        let firstarg = Operand::reg_use(parse_vreg(words[1], vidx));
        let cmp = words[2];
        let secondarg = Operand::reg_use(parse_vreg(words[3], vidx));
        assert_eq!(words[4], "goto");
        let to = words[5].parse::<usize>().unwrap();
        let mut toargs = vec![];
        let mut i = 6;
        while i < words.len() && words[i] != "else" {
            toargs.push(parse_vreg_def(words[i], vidx));
            i += 1;
        }
        assert!(i < words.len(), "There should be an else branch");
        // After else, there should at least be a "goto [branch num]"
        assert!(words[i + 1..].len() >= 2);
        assert_eq!(words[i + 1], "goto");
        let elseto = words[i + 2].parse::<usize>().unwrap();
        let mut elsetoargs = vec![];
        let mut j = i + 2 + 1;
        while j < words.len() {
            elsetoargs.push(parse_vreg_def(words[j], vidx));
            j += 1;
        }
        Self {
            index: Inst::new(index),
            instntype: InstnType::CondBranch {
                cmp: cmp.to_string(),
                args: [firstarg, secondarg],
                to: (Block::new(to), toargs),
                elseto: (Block::new(elseto), elsetoargs),
            },
        }
    }

    fn uncond_branch(words: Vec<&str>, index: usize, vidx: &VRegIdx) -> Self {
        assert!(words.len() >= 2);
        let to = Block::new(words[1].parse::<usize>().unwrap());
        let mut branchargs = vec![];
        if words.len() > 2 {
            for reg in &words[2..] {
                branchargs.push(parse_vreg(reg, vidx));
            }
        }
        Self {
            index: Inst::new(index),
            instntype: InstnType::Branch { to, branchargs },
        }
    }

    fn input(out: VReg, index: usize) -> Self {
        Self {
            index: Inst::new(index),
            instntype: InstnType::Input([Operand::reg_def(out)]),
        }
    }

    fn rewrite(&self, allocs: &[Allocation]) -> String {
        match &self.instntype {
            InstnType::Normal {
                cmd,
                args,
                operands,
            } => {
                assert_eq!(allocs.len(), operands.len());
                let mut code = format!(
                    "{} = {} ",
                    if allocs[0].is_reg() {
                        allocs[0].as_reg().unwrap().to_string()
                    } else {
                        allocs[0].as_stack().unwrap().to_string()
                    },
                    cmd
                );
                for operand in args.iter() {
                    match operand {
                        InstnOperand::Operand(op) => {
                            let op = *op;
                            if allocs[op].is_reg() {
                                code.push_str(&allocs[op].as_reg().unwrap().to_string());
                            } else {
                                code.push_str(&allocs[op].as_stack().unwrap().to_string());
                            }
                            code.push(' ');
                        }
                        InstnOperand::Constant(c) => {
                            code.push_str(c);
                        }
                    }
                }
                code
            }
            InstnType::CondBranch {
                cmp,
                args: _,
                to,
                elseto,
            } => {
                format!(
                    "if {} {} {} goto {:?} else goto {:?}",
                    allocs[0], cmp, allocs[1], to.0, elseto.0
                )
            }
            InstnType::Branch { to, branchargs: _ } => {
                format!("goto {:?} ", to)
            }
            InstnType::Output(_ops) => {
                if allocs[0].is_reg() {
                    format!("output {}", allocs[0].as_reg().unwrap().to_string())
                } else {
                    format!("output {}", allocs[0].as_stack().unwrap().to_string())
                }
            }
            InstnType::Input(_ops) => {
                if allocs[0].is_reg() {
                    format!("{} = input", allocs[0].as_reg().unwrap().to_string())
                } else {
                    format!("{} = input", allocs[0].as_stack().unwrap().to_string())
                }
            }
            InstnType::Return => {
                format!("return")
            }
        }
    }
}

#[derive(Clone, Debug)]
struct BasicBlock {
    index: Block,
    succs: Vec<Block>,
    preds: Vec<Block>,
    params: Vec<VReg>,
    instns: Vec<Instn>,
}

fn parse_vreg_def(reg: &str, vidx: &mut VRegIdx) -> VReg {
    assert!(reg.len() >= 3);
    assert!(reg.starts_with("vg") || reg.starts_with("vv"));
    println!("parse_vreg_def: {}", reg);
    let vname = TypedVRegName::from_str(reg);
    let new_idx = vidx.add_idx(&vname);
    update_max_vreg(new_idx, vname.class);
    VReg::new(new_idx, vname.class)
}

fn parse_vreg(reg: &str, vidx: &VRegIdx) -> VReg {
    assert!(reg.len() >= 3);
    assert!(reg.starts_with("vg") || reg.starts_with("vv"));
    println!("parse_vreg: {}", reg);
    let vname = TypedVRegName::from_str(reg);
    let idx = vidx.get_idx(&vname);
    VReg::new(idx, vname.class)
}

fn update_max_vreg(regnum: usize, regclass: RegClass) {
    use std::sync::atomic::Ordering;
    match regclass {
        RegClass::Int => MAX_VREG_GEN_NO.store(
            MAX_VREG_GEN_NO.load(Ordering::Relaxed).max(regnum),
            Ordering::Relaxed,
        ),
        RegClass::Vector => MAX_VREG_VEC_NO.store(
            MAX_VREG_VEC_NO.load(Ordering::Relaxed).max(regnum),
            Ordering::Relaxed,
        ),
        _ => panic!("unhandled reg class {:?}", regclass),
    }
}

fn get_max_vreg(regclass: RegClass) -> usize {
    use std::sync::atomic::Ordering;
    match regclass {
        RegClass::Int => MAX_VREG_GEN_NO.load(Ordering::Relaxed),
        RegClass::Vector => MAX_VREG_VEC_NO.load(Ordering::Relaxed),
        _ => panic!("unhandled reg class {:?}", regclass),
    }
}

fn get_input() -> (String, Algorithm, usize, usize) {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 5 {
        fail("USAGE: prog [input filepath] [algo] [# physical gen reg] [# physical vec reg]");
    }
    let algo: Algorithm = match args[2].as_str() {
        "ion" => Algorithm::Ion,
        "fastalloc" => Algorithm::Fastalloc,
        _ => panic!("unknown algo: {}", args[2]),
    };
    let filepath = args[1].clone();
    (
        std::fs::read_to_string(filepath)
            .unwrap_or_else(|_| {
                fail("Failed to read the file");
            })
            .trim()
            .into(),
        algo,
        args[3].parse().unwrap_or_else(|_| {
            fail("Failed to parse the number of general physical registers");
        }),
        args[4].parse().unwrap_or_else(|_| {
            fail("Failed to parse the number of vector physical registers");
        }),
    )
}

fn fail(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}
