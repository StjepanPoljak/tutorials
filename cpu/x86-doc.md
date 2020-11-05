# Terminology and History

## Early CPUs

 * **4004** (1971): 4-bit CPU, fourth in series (hence the "004" suffix)
 * **8008** (1972): 8-bit version of 4004 (named by doubling 4004's digits)
 * **4040** (1974): extra instructions to 4004 (4004's last two digits reversed)
 * **8080** (1974): enhanced version of 8008 (named by doubling 4040's digits)
 * **8085** (1976): the 5 in 8085 signifies use of single +5V-volt power supply
 * **8086** (1979): next in series after 8085, accidentally the 6 correspods to 16-bit
 * **8088** (1979): x86-16, variant of 8086 with 8-bit data bus
 * **80186** (1982): x86-16, based on 8086, 16-bit data bus, 20-bit address bus
 * **80188** (1982): variant of 80186 with 8-bit data bus
 * **80286** (1982): x86-16, MMU, based on 8086
 * **80386** (1985): x86-32, 32-bit extension of 80286 architecture
 * **80486** (1989): x86 including x87 (except SX models), over million transistors

Note: The x87 is a separate architecture for the FPU accompanying x86.

## Relevant microarchitectures

 * **P5** (1993): IA-32, MMX ISA, Pentium series
 * **P6** (1995): P6 x86, MMX ISA, Celeron, Pentium series
 * **NetBurst** (2000): NetBurst x86, MMX ISA, Celeron, Pentium, Xeon
 * **Intel Itanium (IA-64)** (2001): 64-bit, ISA originated at Hewlett-Packard
 * **Intel Core** (2006): Intel Core x86, MMX ISA, Celeron, Pentium, Xeon

Note: [MMX](#MMX) is an ISA extension.

## Tick-Tock production model era

 * **Penryn** (2008): "tick", shrink of Core to 45nm, Celeron, Pentium, Xeon
 * **Nehalem** (2008): "tock", 45nm, differs from NetBurst, MMX ISA, Pentium, Core, Xeon
 * **Westmere** (2011): "tick", 32nm shrink of Nehalem, Core i3, i5, i7, Pentium, Celeron, Xeon
 * **Sandy Bridge** (2011): "tock", successor to Nehalem, 32nm, Celeron, Pentium, Core, Xeon
 * **Ivy Bridge** (2012): "tick", 22nm shrink of Sandy Bridge, Celeron G, Pentium G, Core, Xeon
 * **Haswell** (2013): "tock", successor to Ivy bridge, 22nm, Core, Xeon, Pentium, Celeron
 * **Broadwell** (2014): "tick", 14nm shrink of Haswell, Core, Celeron, Pentium, Xeon
 * **Skylake** (2015): "tock", successor to Broadwell, 14nm, Core, Xeon, Pentium, Celeron

Note: Usually, "tick" refers to an improvement in production process (i.e. nm shrink), and "tick" to a change in (micro)architecture.

## Skylake successors

 * **Kaby Lake** (2016) - microarchitecture, Skylake improvement, 14nm, Core, Celeron, Xeon, Pentium
 * **Coffee Lake** (2017) - for desktop use, 14nm, Celeron, Pentium Gold, Core, Xeon
 * **Whiskey Lake** (2018) - for mobile use, 14nm, Celeron, Pentium, Core
 * **Cascade Lake** (2019) - for servers, 14nm, Core, Xeon

# x86

## Registers

The x86 architecture has 8 GPRs, 6 Segment Registers, 1 Flags Register and an Instruction Pointer.

### General Purpose Registers

The table for 16-bit GPRs:

|  register  |       meaning      |                 purpose                |
|------------|--------------------|----------------------------------------|
|     AX     | arithmetic         | for arithmetic operations              |
|     CX     | counter            | used in loops and shift operations     |
|     DX     | data               | for I/O and arithmetic operations      |
|     BX     | base               | pointer to data                        |
|     SP     | stack pointer      | points to top of the stack             |
|     BP     | stack base pointer | points to base of the stack            |
|     SI     | source index       | pointer to source in stream operations |
|     DI     | destination index  | pointer to destination in stream ops.  |

Registers AX, CX, DX and BX are divided in two registers each, e.g.:

<table>
	<tbody>
		<tr>
			<td colspan="2" align="center">AX</td>
		</tr>
		<tr>
			<td align="center">AH</td>
			<td align="center">AL</td>
		</tr>
	</tbody>
</table>

The register AH represents higher 8-bit part of AX, and AL lower 8-bit part of AX. Registers SP, BP, SI, and DI don't have only lower 8-bit counterparts called SPL, BPL, SIL and DIL, respectively.

Note: In IA-32, AX, CX, DX, BX, SP, BP, SI and DI are lower-half 16-bit registers of their 32-bit counterparts, where they are prefixed with an "E" (short for "Extended"), so they are called EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI. In x86-64 registers EAX, ECX, EDX, EBX, ESP, EBP, ESI and EDI are lower 32-bit registers of their 64-bit counterparts, called RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI. The "R" prefix stands for "Register".

### Segment Registers

The table for segment registers:

|  register  |       meaning     |                 purpose                |
|------------|-------------------|----------------------------------------|
|     SS     | stack segment     | pointer to the stack                   |
|     CS     | code segment      | pointer to the code                    |
|     DS     | data segment      | pointer to the data                    |
|     ES     | extra segment     | pointer to extra data                  |
|     FS     | F segment (extra) | pointer to extra data (after E)        |
|     GS     | G segment (extra) | pointer to extra data (after F)        |

Note: On most modern operating systems, the use of these registers is deprecated (except FS and GS which are used to point to thread-specific data).

### MMX

MMX is an ISA extension to x86, introduced with the P5 Pentium series. It's basically a set of macros that make 64-bit aliases out of x87's 80-bit registers. The "new" registers are:

```
%mmx0, %mmx1, ..., %mmx7
```

This way, things stayed backward compatible. To find out if you have MMX, you can see on Linux with:

```
cat /proc/cpuinfo | grep mmx
```

To check for existence of MMX in assembly, use the `cpuid` instruction (x86-64 example):

```
_is_mmx_available:
	pushq   %rbx

	movq    $1, %rax
	cpuid
	movq    %rdx, %rax
	shrq    $23, %rax
	andq    $1, %rax

	popq    %rbx
	ret
```

## Descriptor Tables

### Global Descriptor Table (GDT)

One table entry in GDT contains following two 32-bit parts:

|   bits   | size |        usage        |
|----------|------|---------------------|
|   0:15   |  16  |    segment limit    |
|  16:31   |  16  |     base address    |

|   bits   | size |        usage        |
|----------|------|---------------------|
|   0:7    |  8   |        base         |
|   8:11   |  4   |    segment type     |
|    12    |  1   |   descriptor type   |
|  13:14   |  2   |   privilege level   |
|    15    |  1   |   segment present   |
|  16:19   |  4   |        limit        |
|    20    |  1   |         AVL         |
|    21    |  1   | 64-bit code segment |
|    22    |  1   |         D/B         |
|    23    |  1   |     granularity     |
|  24-31   |  8   |         base        |

Note: If descriptor type is set to 1 (code or data), then first bit of descriptor type is "accessed flag". The value 0 for descriptor type implies system.

Note: AVL means "available for use by system programmers".

Note: 64-bit code segment is for IA-32 only, and D/B is default operation size (0 implies 16-bit segment, and 0 a 32-bit segment).

# Notes

You can find extensive documentation on x86 architecture [here](https://software.intel.com/content/www/us/en/develop/articles/intel-sdm.html).

