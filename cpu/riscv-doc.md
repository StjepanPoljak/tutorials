# Introduction

You can find [ISA specification](https://riscv.org/technical/specifications/) on the [official RISC-V](https://riscv.org) web site.

## RV32I

RV32I is intended to be sufficient to form compiler target and to support most modern operating systems. RV32E is the embedded version of RV32I.

### Registers

|   register   |    ABI    |            purpose              |
|--------------|-----------|---------------------------------|
|     `x0`     |  `zero`   | zero register                   |
|     `x1`     |   `ra`    | return address                  |
|     `x2`     |   `sp`    | stack pointer / callee saved    |
|     `x3`     |   `gp`    | global pointer                  |
|     `x4`     |   `tp`    | thread pointer                  |
|     `x5`     |   `t0`    | link register / temporary       |
|  `x6 - x7`   | `t1 - t2` | temporary                       |
|     `x8`     | `s0 / fp` | callee saved / frame pointer    |
|     `x9`     |   `s1`    | callee saved / temporary        |
| `x10 - x11`  | `a0 - a1` | parameter / result              |
| `x12 - x17`  | `a2 - a7` | parameter                       |
| `x18 - x27`  |`s2 - s11` | callee saved / temporary        |
| `x28 - x31`  | `t3 - t6` | temporary registers             |


### Move instruction

The move instruction `mv <Rd>, <Rs>` is just an alias for `addi <Rd>, <Rs>, 0`.

### Memory operations

#### Load

The main syntax to load data to `<Rd>` from address stored in `<Rs>` is: 

```
l[b|h|w|d] <Rd>, <offset>(<Rs>)
```

For example, to load a byte into `t0` from address in `sp` with offset `16`:

```
lb to, 16(sp)
```

#### Store

To store data from `<Rs>` to address stored in `<Rd>`:

```
s[b|h|w|d] <Rs>, <offset>(<Rd>)
```

For example, to store a byte from `t0` to address in `sp` with offset `16`:

```
sb t0, 16(sp)
```

Note: The `[b|h|w|d]` options refers to the size of memory being loaded (or stored), and is explained in the table below:

| code |     size    | bits |
|------|-------------|------|
| `b`  |     byte    | `8`  |
| `h`  |  half word  | `16` |
| `w`  |     word    | `32` |
| `d`  | double word | `64` |

### Arithmetic operations

|       instruction      |        formula        |
|------------------------|-----------------------|
| `add <Rd>, <Rm>, <Rn>` |    `Rd = Rm + Rn`     |
|`addi <Rd>, <Rm>, #imm` |    `Rd = Rm + imm`    |
| `sub <Rd>, <Rm>, <Rn>` |    `Rd = Rm - Rn`     |
| `mul <Rd>, <Rm>, <Rn>` |    `Rd = Rm * Rn`     |
| `div <Rd>, <Rm>, <Rn>` |    `Rd = Rm / Rn`     |
| `rem <Rd>, <Rm>, <Rn>` |    `Rd = Rm % Rn`     |

### Logical bitwise operations

|       instruction      |        formula        |
|------------------------|-----------------------|
| `and <Rd>, <Rm>, <Rn>` |    `Rd = Rm & Rn`     |
| `or <Rd>, <Rm>, <Rn>`  |    `Rd = Rm | imm`    |
| `xor <Rd>, <Rm>, <Rn>` |   `Rd = Rm XOR Rn`    |
