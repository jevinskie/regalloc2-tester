use regalloc2::{Block, VReg, Inst, Operand, OperandConstraint, OperandKind, OperandPos, RegClass, InstRange};

fn main() {
    let (input, no_of_regs) = get_input();
    let builder = IrBuilder::new();
    let ir = builder.build(input.lines().collect());
    println!("{:?}", ir);
}

#[derive(Debug)]
struct Ir {
    blocks: Vec<BasicBlock>,
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
                        instntype: InstnType::Output(Operand::new(
                        reg,
                        OperandConstraint::Any,
                        OperandKind::Use,
                        OperandPos::Late
                    ))});
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
                            let mut args = vec![];
                            for arg in &input[1..] {
                                if arg.starts_with("v") {
                                    args.push(InstnOperand::Operand(Operand::new(
                                        parse_vreg(arg),
                                        OperandConstraint::Any,
                                        OperandKind::Use,
                                        OperandPos::Early
                                    )));
                                } else {
                                    args.push(InstnOperand::Constant(arg.to_string()));
                                }
                            }
                            self.instns.push(Instn::new(cmd, outreg, args, self.next_instn_index));
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
}

#[derive(Clone, Debug)]
enum InstnOperand {
    Operand(Operand),
    Constant(String)
}

#[derive(Clone, Debug)]
enum InstnType {
    Normal {
        out: Operand,
        cmd: String,
        args: Vec<InstnOperand>,
    },
    CondBranch {
        cmp: String,
        firstarg: Operand,
        secondarg: Operand,
        to: Block,
        branchargs: Vec<VReg>,
    },
    Branch {
        to: Block,
        branchargs: Vec<VReg>,
    },
    Output(Operand),
    Input(Operand),
}

impl Instn {
    fn new(cmd: &str, outreg: VReg, args: Vec<InstnOperand>, index: usize) -> Self {
        Self {
            index: Inst::new(index),
            instntype: InstnType::Normal {
                out: Operand::new(
                    outreg.clone(),
                    OperandConstraint::Any,
                    OperandKind::Def,
                    OperandPos::Late
                ),
                cmd: cmd.to_string(),
                args,
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
                firstarg,
                secondarg,
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
            instntype: InstnType::Input(Operand::new(
                out,
                OperandConstraint::Any,
                OperandKind::Def,
                OperandPos::Late,
            ))
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
    VReg::new(regnum, RegClass::Int)
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
