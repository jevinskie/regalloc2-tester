# Reverse Linear Scan Regalloc Concept 2

A continuation of my exploration of the reverse linear scan regalloc algorithm (SSRA).

This project parses a program, uses my sample implementation of SSRA in the 
`regalloc2` (https://github.com/d-sonuga/regalloc2) crate for register allocation,
and outputs the program rewritten to use the physical registers.

The input program must be in the form of the sample programs in `sample/`.

Accepts a program:
* `block` to indicate blocks and block parameters.
* Virtual registers of form `vn` where `n` is the register number.
* `goto` for unconditional branches, with the block parameters listed in front.
* `if ... goto ... else goto ...` for conditional branches with 2 targets.
* Outputs `output [virtual register]` (Meaning read virtual register and output its value,
so the operand is an early use).
* Inputs: `[virtual register] = input` (Meaning accept input and store it in virtual register,
so the operand is a late def).
* Operations: `[virtual-register] = [command] [virtual-register | constant] [virtual-register | constant]`,
where `command` is a string, the virtual registers/constants following are early use operands
and the virtual register on the left side of the equals sign is 
* Each instruction is either a `return` with no arguments, an unconditional branch,
a conditional branch, an output, an input, or an operation.
* Each block must end in a branch or return.
* Branches and returns must not be in the middle of a block.
* There must be no critical edges.

All values are assumed to be integers.

For example, a sample program with 8 virtual registers and 2 blocks:

```
block 0
    v0 = input
    v1 = input
    v2 = add v0 v1
    v3 = add v2 v1
    v4 = add v0 v3
    goto 1 v0

block 1 v5
    v6 = input
    v7 = add v5 v6
    return
```

A sample output with only 2 physical registers:

```
block 0
    p0i = input
    p1i = input
    move p0i to stack1
    p0i = add p0i p1i 
    p0i = add p0i p1i 
    move stack1 to p1i
    move p1i to stack0
    p1i = add p1i p0i 
    goto Block(1) 

block 1
    move stack0 to p1i
    p0i = input
    p1i = add p1i p0i 
    return
```

To run:

```
cargo run [input path] [no of registers]
```

With a sample input:

```
cargo run sample/input3.txt 2
```
