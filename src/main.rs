use std::sync::atomic::AtomicUsize;

use regalloc2::{Block, VReg, Inst, Operand, OperandConstraint, OperandKind, OperandPos, RegClass, InstRange, PRegSet};

fn main() {
    let (input, no_of_regs) = get_input();
    let builder = IrBuilder::new();
    let ir = builder.build(input.lines().collect());
    println!("{:?}", ir);
}

static MAX_VREG_NO: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
struct Ir {
    blocks: Vec<BasicBlock>,
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
        self.blocks.iter()
            .map(|block| block.instns.len())
            .sum()
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
        let instn = self.find_instn(insn)
            .expect(&format!("Failed to find instruction with index {:?}", insn));
        instn.is_ret()
    }

    fn is_branch(&self, insn: Inst) -> bool {
        let instn = self.find_instn(insn)
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
        let instn = self.find_instn(insn)
            .expect(&format!("Failed to find instruction with index {:?}", insn));
        instn.operands()
    }

    fn num_vregs(&self) -> usize {
        get_max_vreg() + 1
    }

    fn inst_clobbers(&self, insn: Inst) -> regalloc2::PRegSet {
        PRegSet::empty()
    }

    fn spillslot_size(&self, regclass: RegClass) -> usize {
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
                    self.blockindex = words[1].parse::<usize>().expect("Block numbers should be valid `usize`s");
                    for word in &words[2..] {
                        assert!(word.starts_with("v"), "Block params must have valid virtual register names");
                        assert!(word.len() > 1, "Virtual registers should have a number");
                        let regnum = word[1..].parse::<usize>().unwrap();
                        self.blockparams.push(VReg::new(regnum, RegClass::Int));
                    }
                }
                "if" => {
                    let instn = Instn::cond_branch(words, self.next_instn_index);
                    self.succs.push(instn.to());
                    self.instns.push(instn);
                }
                "goto" => {
                    let instn = Instn::uncond_branch(words, self.next_instn_index);
                    self.succs.push(instn.to());
                    self.instns.push(instn);
                }
                "output" => {
                    assert_eq!(words.len(), 2);
                    let reg = parse_vreg(words[1]);
                    self.instns.push(Instn {
                        index: Inst::new(self.next_instn_index),
                        instntype: InstnType::Output([Operand::new(
                            reg,
                            OperandConstraint::Any,
                            OperandKind::Use,
                            OperandPos::Late
                        )])
                    });
                }
                "return" => {
                    assert_eq!(words.len(), 1);
                    self.instns.push(Instn {
                        index: Inst::new(self.next_instn_index),
                        instntype: InstnType::Return,
                    })
                }
                v => {
                    if v.starts_with("v") {
                        let parts: Vec<&str> = line.split("=").collect();
                        assert_eq!(parts.len(), 2);
                        let outreg = parse_vreg(parts[0].trim());
                        let input: Vec<&str> = parts[1].trim().split(" ").collect();
                        assert!(!input.is_empty());
                        let cmd = input[0];
                        if cmd == "input" {
                            self.instns.push(Instn::input(outreg, self.next_instn_index));
                        } else {
                            self.instns.push(Instn::new(cmd, outreg, &input[1..], self.next_instn_index));
                        }
                    } else {
                        panic!("Unrecognized word: {}", v);
                    }
                }
            };
            self.next_instn_index += 1;
        }
        self.end_bb();
        self.compute_preds();
        Ir { blocks: self.blocks }
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
    instntype: InstnType
}

impl Instn {
    fn to(&self) -> Block {
        match self.instntype {
            InstnType::CondBranch { to, .. } => to,
            InstnType::Branch { to, .. } => to,
            _ => panic!("Attempting to find a `to` on a non-branch instruction")
        }
    }

    fn is_ret(&self) -> bool {
        match &self.instntype {
            InstnType::Return => true,
            _ => false
        }
    }

    fn is_branch(&self) -> bool {
        match &self.instntype {
            InstnType::CondBranch { .. } => true,
            InstnType::Branch { .. } => true,
            _ => false
        }
    }

    fn blockparams_to(&self, succidx: usize) -> &[VReg] {
        match &self.instntype {
            InstnType::CondBranch { to, branchargs, .. } => {
                if to.index() == succidx {
                    &branchargs
                } else {
                    eprintln!("The branch doesnt go to succidx");
                    &[]
                }
            }
            InstnType::Branch { to, branchargs, .. } => {
                if to.index() == succidx {
                    eprintln!("The branch doesnt go to succidx");
                    &branchargs
                } else {
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
            InstnType::Normal { operands, .. } => {
                &operands
            }
            InstnType::CondBranch { args, .. } => {
                args.as_slice()
            }
            InstnType::Input(op) => {
                op.as_slice()
            }
            InstnType::Output(op) => {
                op.as_slice()
            }
            _ => {
                println!("No operands");
                &[]
            }
        }
    }
}

#[derive(Clone, Debug)]
enum InstnOperand {
    /// An index into a vector of operands
    Operand(usize),
    Constant(String)
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
        to: Block,
        branchargs: Vec<VReg>,
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
    fn new(cmd: &str, outreg: VReg, args: &[&str], index: usize) -> Self {
        let mut operands = vec![Operand::new(
            outreg.clone(),
            OperandConstraint::Any,
            OperandKind::Def,
            OperandPos::Late
        )];
        let mut parsedargs = vec![];
        for arg in args {
            if arg.starts_with("v") {
                operands.push(Operand::new(
                    parse_vreg(arg),
                    OperandConstraint::Any,
                    OperandKind::Use,
                    OperandPos::Early
                ));
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
            }
        }
    }

    fn cond_branch(words: Vec<&str>, index: usize) -> Self {
        assert!(words.len() >= 6);
        let firstarg = Operand::new(
            parse_vreg(words[1]),
            OperandConstraint::Any,
            OperandKind::Use,
            OperandPos::Early,
        );
        let cmp = words[2];
        let secondarg = Operand::new(
            parse_vreg(words[3]),
            OperandConstraint::Any,
            OperandKind::Use,
            OperandPos::Early
        );
        assert_eq!(words[4], "goto");
        let to = words[5].parse::<usize>().unwrap();
        let mut branchargs = vec![];
        for reg in &words[6..] {
            branchargs.push(parse_vreg(reg));
        }
        Self {
            index: Inst::new(index),
            instntype: InstnType::CondBranch {
                cmp: cmp.to_string(),
                args: [firstarg, secondarg],
                to: Block::new(to),
                branchargs
            }
        }
    }

    fn uncond_branch(words: Vec<&str>, index: usize) -> Self {
        assert!(words.len() >= 2);
        let to = Block::new(words[1].parse::<usize>().unwrap());
        let mut branchargs = vec![];
        if words.len() >= 2 {
            for reg in &words[2..] {
                branchargs.push(parse_vreg(reg));
            }
        }
        Self {
            index: Inst::new(index),
            instntype: InstnType::Branch { to, branchargs }
        }
    }

    fn input(out: VReg, index: usize) -> Self {
        Self {
            index: Inst::new(index),
            instntype: InstnType::Input([Operand::new(
                out,
                OperandConstraint::Any,
                OperandKind::Def,
                OperandPos::Late,
            )])
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

fn parse_vreg(reg: &str) -> VReg {
    assert!(reg.len() >= 2);
    assert!(reg.starts_with("v"));
    let regnum = reg[1..].parse::<usize>().unwrap();
    update_max_vreg(regnum);
    VReg::new(regnum, RegClass::Int)
}

fn update_max_vreg(regnum: usize) {
    use std::sync::atomic::Ordering;
    MAX_VREG_NO.store(MAX_VREG_NO.load(Ordering::Relaxed).max(regnum), Ordering::Relaxed);
}

fn get_max_vreg() -> usize {
    use std::sync::atomic::Ordering;
    MAX_VREG_NO.load(Ordering::Relaxed)
}

fn get_input() -> (String, i32) {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        fail("USAGE: prog [input filepath] [num of physical registers]");
    }
    let filepath = args[1].clone();
    (
        std::fs::read_to_string(filepath)
            .unwrap_or_else(|_| {
                fail("Failed to read the file");
            })
            .trim()
            .into(),
        args[2].parse().unwrap_or_else(|_| {
            fail("Failed to parse the number of registers");
        }),
    )
}

fn fail(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}
