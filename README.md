# GAD - Galaksija Debugger

GAD is a debugging application for retro computer [Galaksija](https://en.wikipedia.org/wiki/Galaksija_(computer)) written in Z80 assembly language. It is aimed to help with developing other assembly language programs.

Minimum system requirements are Galaksija with built-in ROM A and ROM B (sometimes referred also as ROM 1 and ROM 2) and some RAM memory expansion. Classical 6 KB RAM Galaksija is technically capable of loading GAD but doesn't have any memory left for debugged program.

# Features
- Up to four software breakpoints set to any assembly instruction
- Single stepping
- Stepping into or over a subroutine
- Inspection and editing of all register values and individual flags at any time
- Fast single key commands where ever is possible
- Command line input for commands with parameters
- Implemented most of the machine code monitor functionality
- Keeps backup of system screen memory
- Private stack and keyboard input buffer
- Fairly good error handling for such a simple platform
- Modest memory footprint (about 4.5 KBytes)

# Description

Next image depicts typical GAD screen appearance. GAD's screen is divided into six sections located at fixed screen positions.

![GAD screen sections.](/images/dbg-sections.png)

First row of a _Registers view_ shows main register set values, as well as values of IX and SP registers. Second row shows alternate register set values, IY and two bytes from memory location pointed by SP register, respectively.

_Individual flags view_ is an another representation for a F register value. Every flag from the F register is displayed as a dash character (-) if reset and as a letter if set, beginning from the lowest bit on right to highest bit on the left side. For example, `SZ H VNC` is displayed when all bits are set.

_Disassembler window_ shows memory contents in assembly code form. The contents of a window can be changed with `D` command and some sort of scrolling can be achieved with skip key shortcut `/` for forward scrolling and `B` for backward scrolling (one byte at the time).

Program counter (PC) always points to first disassembled line marked by `>` character placed in front of instruction mnemonic. PC value can be set with `D from` or `R PC value` commands, or implicitly with `G addr` command.

_Memory dump window_ shows hexadecimal memory view with eight bytes in a row. For changing dump address use `M` command and for scrolling use up (`↑`) and down (`↓`) arrows.

_Command line_ is used to submit commands with parameters. It's hidden while key shortcut command mode is active. Switching to command line input mode can be done by pressing `I` key. Exiting the command line input mode is done by executing `X` command (type X and press ENTER).

Bottom screen line is _Messages area_. This is the line where some commands display their output information and where error messages are displayed.

## Program messages

GAD has few commonly used messages which have the same meaning where ever they may be issued. Those are:
- `WHAT?` message is displayed when number of command parameters is wrong.
- `HOW?` message is displayed when one of the parameters has wrong value.
- `SORRY` message is displayed when command cannot be executed. Reason depends on the specific command but usually it is because chosen memory address value is in restricted area.
- `OK` message is displayed when command does not alter data on the screen but want to inform user that it ended successfully.

There are other messages specific to certain commands which will be explained as part of command description.

# Commands

## Commands Issued by Keyboard Shortcut

> If working in emulator, note that keyboard shortcut commands correspond to Galaksija's keyboard layout, not the PC's one.

### Basic Keyboard Shortcuts

| Key | Description
|------|---------------
| `I`  | Enter command line input mode
| `ENTER` | Step over (single step)
| `.`  | Step into
| `;`  | Continue execution to a breakpoint
| `/`  | Skip current instruction
| `B`  | Decrease PC value by one (Back)
| `SPACE` | Toggle between system and GAD screens
| `F`  | Search forward
| `X`  | Exit to BASIC prompt

### Flag Toggle Commands

| Key | Description
|------|---------------
| `C` | Toggle Carry flag
| `N` | Toggle Negative flag
| `V` | Toggle parity/oVerflow flag
| `H` | Toggle Half carry flag
| `Z` | Toggle Zero flag
| `S` | Toggle Sign flag

### Memory Dump Window Navigation

| Key | Description
|------|---------------
| `↑` | Scroll memory dump window up
| `↓` | Scroll memory dump window down

## Command Line Commands

Most of the command parameters represent memory address and are defined as up to four digit hexadecimal or decimal number. Hexadecimal numbers must have even number of digits and be prefixed by ampersand (&). Note that decimal numbers are also long only up to four digits, therefore maximum allowed decimal value is 9999. One exception to previous definition is _hex_string_ parameter which is defined as even number of hexadecimal digits optionally prefixed with `&` sign (eg. &114AB7).

| Command | Syntax | Description
|-----|--------|------
| `D` | _from_ | Disassemble memory and set PC register address
| `M` | _from_ | Memory dump
| `E` | _from_ _hex_string_ | Edit memory
| `B` | _no_ _addr_ | Breakpoint set/remove/show
| `R` | _reg_ _value_ | Set register or register-pair value
| `C` | _from_ _to_ _length_ | Copy memory
| `S` | _start_ _end_ _hex_string_ | Search memory
| `F` | _start_ _end_ _byte_ | Fill memory
| `P` | _addr_ | Proceed execution until RET instruction
| `G` | _addr_ | Go execute to a breakpoint
| `V` | | Print program version
| `X` | | Exit command line mode

# Command Reference Manual

## Commands Issued by Keyboard Shortcut

### `ENTER` -	Step over (single step)

Pressing the `ENTER` key will trigger the execution of the instruction pointed by program counter register. In case that current instruction is a subroutine call instruction, complete subroutine will be executed and new program counter value will be set to the next instruction after the call (hence the name `step over`).

PC value can be set at any time before or between two single step commands with `D from` or `R PC value` commands.

> After every instruction executed by single step command, screen blinks once. You may ask why is it necessary? Well, it is unavoidable cause for every executed instruction, GAD copies system screen to video memory, executes (or emulates if necessary) the instruction pointed by PC, copies back system screen from video memory to the buffer and regenerates its own screen contents.

### `;` - Continue execution to a breakpoint

This command will continue execution from the current PC value until it reaches a breakpoint or the end of the program.

> If there are no breakpoints set, or non of the breakpoints has not been hit during the execution, and end of the debugged program is reached, then execution will jump out of GAD to the BASIC prompt. To avoid this behavior consider using proceed command (`P addr`) or use single stepping instead.

### `F` - Search forward

Search forward command is integral part of the `S` command previously issued from the command line. As its name says, it searches forward the next occurrence of the searched string from the address where it has been previously found.

## Command Line Commands

### `B` -	Breakpoint set/remove/show

This command has multiple functions. It sets a new breakpoint, removes an existing breakpoint (only one at a time) or shows all set breakpoint addresses. Up to four breakpoints can be set simultaneously. Upon a successful completion, all command variations print breakpoint all addresses in the status line.

Setting a breakpoint has following format:

```
B <bp number> <address>
```
where _bp number_ is number of a breakpoint (from 1 to 4), and _address_ is an address where breakpoint has to be set.

Removing the breakpoint is essentially equal to setting breakpoint address to zero. For removing multiple breakpoints, command has to be repeated for every breakpoint number.

```
B <bp number> 0
```

EXAMPLES:

This example sets breakpoint number two value to hexadecimal address 3200. Other three breakpoints are not set.
```
B 2 &3200
1:---- 2:3200 3:---- 4:----
```
Next example shows how to check which breakpoints are set and their values. After issuing `B` command without any parameters, GAD will print all breakpoint values in the status line. Note that if breakpoint is not set, its value is displayed with dashes (`----`).

```
B
1:2F00 2:3200 3:---- 4:----
```

> Addresses below `&2C00` are not allowed for breakpoints (ROM, video RAM and system variables space). If user try to set a breakpoint in that range, message `SORRY` will be displayed.

### `F` - Fill memory

This command fills memory section with a specified byte. General format is as follows:

```
F <start address> <end address> <optional byte value>
```
where: _start address_ is first memory location to fill with _byte value_, _end address_ is last location to fill with _byte value_, and _byte value_ is optional byte value to be written. If _byte value_ is not specified, memory will be filled with zeros.

In case that command ends successfully, an `OK` message is displayed in the message line.

EXAMPLES:

```
F &3000 &30FF &FF
```
This command fills memory locations from address &3000 to &30FF (including start and end addresses) with value &FF.
```
F &4000 &47FF
```
This command example fills memory locations &4000 to &47FF with zeros.

### `G` - Go execute to a breakpoint

Starts execution at the specified address. Command format is:

```
G <address>
```
where _address_ is address where execution is to start.

> If there are no breakpoints set, or non of the breakpoints has not been hit during the execution, and end of the debugged program is reached, then execution will jump out of GAD to the BASIC prompt. To avoid this behavior consider using proceed command (`P addr`) or use single stepping instead.

### `R` - Set register or register-pair value

This command sets any register (8-bit) or register-pair (16-bit) value, except `I` and `R` registers. Format of the command is:

```
R <register> <value>
```
where _register_ is register name (eg. `A` or `BC`), and value is new one or two byte register value. After the execution, register view will immediately update specified register value.

> Due to limitations imposed by Galaksija's architecture for use of IY and (to some lesser extent) IX registers from user code, value of IY and IX registers cannot be changed by `R` command! Changes issued to IY and IX registers will be shown in _register view_ but values will not be transferred to registers itself.

EXAMPLES:

Next example shows how `A` register is set to hex `FF` value.
```
R A &FF
```
In the next example `HL` register gets decimal value `1234`.
```
R HL 1234
```
This example shows that program counter can be set with `R` command, as well.
```
R PC &4000
```

### `S` - Search Memory

Search command is searching through specified memory range for a set of bytes. If found, memory dump window is set to start address of a first found occurrence. Next occurrences can be searched by pressing `F` key (search forward). If searched bytes string has not been found, or there are no more subsequent occurrences, `END` message is printed into the message line.

Syntax for the search command is:

```
S <start address> <end address> <hex string>
```
where: _start address_ is beginning address for search procedure, _end address_ is ending address for search procedure and _hex string_ is a searched set of bytes written as a hexadecimal string with optional `&` character at the beginning, and without spaces between hexadecimal byte values.

Searched string length is limited only by size of input buffer. Currently, maximum command length is 30 characters.

> Between subsequent searches with `S` and `F` commands, searched string is kept in the command line input buffer. Thus, subsequent searches will work correctly only if no other command line commands are issued in meantime, because they would overwrite the input buffer contents.

# Limitations

Co-executing two programs on a platform without memory protection certainly imposes some rules of good behavior for both of these programs. But there are some not so obvious limitations which will be listed here as well.

GAD's breakpoint is implemented as simple three bytes `CALL` instruction into a breakpoint handling routine. When inserted into the target code, this CALL instruction may overlap from one (three byte long) to three (one byte long) other instructions. Depending on a order of execution, in same rare occasions, when execution misses the breakpoint but jumps to second or third byte overlapped by `CALL` instruction, this may lead to unpredictable behavior (if there would exist one user defined `RST` vector, this could be avoided, but there is no such possibility on Galaksija).

# License

The MIT License (MIT)

Copyright (c) 2024 Vitomir Spasojević <vitomir.spasojevic@gmail.com> (https://github.com/DigitalVS/GAD). All rights reserved.














