;--------------------------------------------------------------------------------------------------
;
; GAD - Galaksija Debugger
;
; Copyright (c) 2024 by Vitomir Spasojević. All rights reserved.
;
;--------------------------------------------------------------------------------------------------

INBUFFERSIZE  = 30          ; Input buffer size
NUMBRKS       = 4           ; Breakpoints number
START_ADDR    = $2C3A       ; Initial dump and disassembler start address is set at the beginning of free memory
VERSION       = 3

  STRUCT brk
status          BYTE        ; 1 = active, 0 = inactive
addr            WORD        ; Breakpoint address
tmp             BLOCK 3     ; Original three bytes from target address
  ENDS

  STRUCT v                  ; Start of variables declaration
_PREFIX         BYTE
_TABEL1_INDEX   BYTE        ; Opcode index in TABEL1
_OPCODE         BYTE
_CODE_LEFT      BYTE
_CODE_RIGHT     BYTE
_VALUE_1        BYTE        ; Instruction 8-bit data value or low byte from 16-bit value
_VALUE_2        BYTE
_DISASS_ADDR    WORD        ; Current disassembling address
_DISASM_CNT     BYTE        ; How many lines to disassemble
_INST_LEN       BYTE        ; Instruction length in bytes
_BRK_ADDR       WORD        ; Address of current breakpoint or next instruction address for single stepping
_SEARCH_START   WORD        ; Search start address
_SEARCH_END     WORD
_AF_ALT         WORD        ; AF'
_BC_ALT         WORD        ; BC'
_DE_ALT         WORD        ; DE'
_HL_ALT         WORD        ; HL'
_AF_REG         WORD        ; AF
_BC_REG         WORD        ; BC
_DE_REG         WORD        ; DE
_HL_REG         WORD        ; HL
_IX_REG         WORD        ; IX
_IY_REG         WORD        ; IY
_SP_REG         WORD        ; SP
_PC_REG         WORD
_SP_SYS         WORD        ; System stack pointer backup value
_DUMP_ADDR      WORD        ; Current memory dump address
_KEY_CODE       BYTE
_TRACE_INSTR    BLOCK 4     ; Temporary space for traced instruction ('sandbox')
_TRACE_BRK      BLOCK 3     ; Call instruction at end of sandbox
_BREAKPOINTS    BLOCK brk * NUMBRKS ; Breakpoints array
_CURSOR_SAVE    WORD        ; System cursor position
_SCREEN_SAVE    BLOCK VRAMLEN ; System screen buffer
_IN_BUFFER      BLOCK INBUFFERSIZE ; Private keyboard input buffer
_DBG_STACK      BLOCK 64    ; Debugger private stack
  ENDS

  include "galaksija.inc"

; ROM B routines
DumpMemory = $19DF          ; Print memory as HEX values, from HL address, A rows
; ROM B table addresses used by disassembler
TABEL1 = $1EE4              ; TABEL1 pointer to first mnemonic string (skipped other command strings at the beginning)
TABEL2 = $1FB4              ; Register and flag labels table

; Constants
REGS_STRING = $1FD8
REGS_CURSOR_POS = VIDEORAM + $20
DISASM_CURSOR_POS1 = VIDEORAM + $80 ; First disassembler line
DISASM_CURSOR_POS2 = VIDEORAM + $100
DISASM_CURSOR_POS6 = VIDEORAM + $120 ; Last (6th) disassembler line
DISASM_POINTER_POS = DISASM_CURSOR_POS1 + 13 ; Character position before instruction mnemonic
DUMP_CURSOR_POS = VIDEORAM + $140
INPUT_CURSOR_POS = VIDEORAM + $1C0
MSG_LINE_CPOS = VIDEORAM + $1E0

  .org $4800

Start:
  ld (SP_SYS), sp           ; Save system stack pointer
  ld (SP_REG), sp           ; Copy to SP_REG
  ld sp, STACK_TOP
  call SaveScreen           ; Save system screen
  call RemoveBreaks         ; Clear all breakpoints
  ld de, START_ADDR         ; Disassembler and memory dump start address
  ld (PC_REG), de
  ld hl, DUMP_ADDR
  ld (hl), e                ; ld (hl), de
  inc hl
  ld (hl), d
  ; Next two flags have to have value 1 because otherwise PrintHex16 call won't display anything!
  ld a, $01
  ld (ASMOPTFLAG), a
  ld (ASMPASSNO), a
UpdateScreen:
  ld a, FF                  ; Form feed character
  rst $20                   ; Clears screen
  ld de, REGS_STRING        ; Print registers
  call PrintString
  call ShowRegisters
  ld hl, (PC_REG)
  call DisassemblerInit
  call ShowPointer
  call MemoryDump
KeyLoop:
  call ReadKey
  ld (KEY_CODE), a          ; This is needed at one place in code
  cp 'I'                    ; Enter command line input mode
  jp z, GetInputCmd
  cp CR                     ; Trace over (single step)
  jp z, TraceNext
  cp '.'                    ; Trace into CALL
  jp z, TraceNext
  ; cp ','                    ; Trace out of CALL (until RET)
  ; jp z, TraceOut
  cp ';'                    ; Trace to breakpoint
  jp z, TraceGo
  cp '/'                    ; Skip instruction
  call z, Skip
  cp 'B'                    ; PC = PC - 1
  jp z, TraceBack
  ld b, $01
  cp 'C'                    ; C = toggle Carry flag
  jr z, ToggleFlags
  ld b, $02
  cp 'N'                    ; N = toggle Negative flag
  jr z, ToggleFlags
  ld b, $04
  cp 'V'                    ; V = toggle parity/oVerflow flag
  jr z, ToggleFlags
  ld b, $10
  cp 'H'                    ; H = toggle Half carry flag
  jr z, ToggleFlags
  ld b, $40
  cp 'Z'                    ; Z = toggle Zero flag
  jr z, ToggleFlags
  ld b, $80
  cp 'S'                    ; S = toggle Sign flag
  jr z, ToggleFlags
  cp ' '
  jp z, ToggleScreen
  cp 'F'
  jp z, SearchMemCmd.SearchNext
  cp 'X'
  jp z, Exit
  cp KEY_DOWN
  jr nz, .CheckDown
  ld hl, (DUMP_ADDR)
  ld de, 8                  ; 8 bytes are in one row
  add hl, de
  jr .Dump
.CheckDown:
  cp KEY_UP
  jr nz, KeyLoop
  ld hl, (DUMP_ADDR)
  ld de, 8
  or a                      ; Next two instructions: sub hl, de
  sbc hl, de
.Dump:
  ld (DUMP_ADDR), hl
  call MemoryDump
  jr KeyLoop

ToggleFlags:
  ld hl, AF_REG
  ld a, b
  xor (hl)                  ; Toggle flag bit
  ld (hl), a
  call ShowRegistersPos
  jp KeyLoop

;--------------------------------------------------------------------------------------------------
; Adopted subroutine from ROM A at address $7BB to allow input in one line only into its own buffer
;
GetInputLine:
  ld a, '>'
  ld de, IN_BUFFER          ; Load INPUT_BUFFER address into DE
  rst $20                   ; Display prompt '>'.
.LoopCurs:
  push hl
  ld hl, (CURSORPOS)
  ld (hl), '_'
  pop hl
.Loop:
  call ReadKey              ; Read a key and print it
  rst $20
  push hl
  ld hl, (CURSORPOS)
  ld (hl), '_'              ; Print cursor
  pop hl
  cp CR
  jr z, .Add                ; ENTER pressed, jump to ADD
  cp KEY_LEFT
  jr z, .Backspace      	  ; Left arrow pressed, jump to BACKSPACE
  cp $0C
  jr z, GetInputLine        ; SHIFT-DELETE pressed - begin from the start
  cp $20
  jr c, .Loop               ; Ignore if key code is less than $20
.Add:
  ld (de), a                ; Load ASCII code of the pressed key into the buffer
  inc de
  cp CR                     ; If ENTER was pressed, return.
  ret z
  ld a, e                   ; Compare lower byte of DE with end of buffer
  ; TODO Implement longer line input, with horizontal scrolling.
  cp (IN_BUFFER + INBUFFERSIZE) & $FF ; Input buffer start address plus its size. This is allowed command input length.
  jr nz, .Loop              ; If end of buffer not reached,	then loop...
  ld a, KEY_LEFT            ; ...else delete last pressed character from the screen and buffer
  rst $20
.Backspace:
  ld a, e
  cp IN_BUFFER & $FF        ; Compare lower byte of DE with buffer start address
  jr z, GetInputLine        ; If not at start of buffer decrement DE
  dec de
  jr .LoopCurs

GetInputCmd:
  ld de, INPUT_CURSOR_POS
  call SetCursorPos
  call GetInputLine
  ld de, IN_BUFFER          ; DE = Input buffer address
  ld a, (de)                ; Command character is in A
  inc de
  ld hl, CMD_TABLE-2        ; HL = command table address - 2
.FindCmd:
  inc hl
  inc hl                    ; HL + 2 (skip command address)
  bit 7, (hl)               ; Check for table end (marked as value $80)?
  jr nz, ShowWhatMsg        ; If yes, command not found
  cp (hl)                   ; Is it correct command?
  inc hl                    ; HL = command address
  jr nz, .FindCmd           ; If not correct command, search further
  ld b, a                   ; Save A temporary
  ld a, (de)                ; Check second command character
  cp ' '
  jr z, .OneChrCmd
  cp CR
  jr nz, ShowWhatMsg        ; If second character is not SPACE nor CR
.OneChrCmd:
  ld a, b
  ld c, (hl)                ; Command has been found!
  inc hl
  ld b, (hl)                ; BC = command handler address
  push bc                   ; Command address to stack
  cp 'R'                    ; For R, E, S commands don't read parameters because they have different parameters format
  jr z, .Execute
  cp 'E'
  jr z, .Execute
  cp 'S'
  jr z, .Execute
  call GetParameter         ; Get 1st command parameter and put it to stack
  jr c, ShowHowMsg
  rst $28                   ; Clear HL
  jr z, .NoParameter        ; If there are no parameters, jump
  call GetParameter         ; Read 2nd parameter and push it to stack
  jr c, ShowHowMsg          ; If number conversion fails
  jr z, .Param1             ; If there are no more parameters, jump
  call GetParameter         ; Read 3rd parameter and push it to stack
  jr c, ShowHowMsg
  ld bc, $0000
  jr z, .Param2             ; If there are no more parameters, jump
  call GetParameter
  jr nz, ShowWhatMsg        ; If there is 4th parameter
                            ; At this point command has three parameters
  pop bc                    ; 3 parameters: BC = third parameter, A will be 3 at the end
  inc a                     ; A = 1 (previous A value is always 0 because SUB $0D if there are no more parameters)
.Param2:
  pop de                    ; Command has two parameters: DE = second parameter, A = 2
  inc a
  inc a                     ; A += 2 (2 or 3)
  pop hl                    ; HL = first parameter
.Execute:
  cp $03                    ; Set Zf = 1 if command has three parameters
  push af
  push de
  push hl
  ld de, INPUT_CURSOR_POS
  call SetCursorPos
  ld h, d                   ; HL = DE
  ld l, e
  call ClearLine            ; Clear cmd line before executing the command
  call ClearMsgLine         ; Clear bottom line
  pop hl
  pop de
  pop af
  ret                       ; Jump to command execution (command address is on the stack top)
.Param1:
  pop hl                    ; Command has one parameter: HL = parameter
  inc a                     ; A = 1
.NoParameter:               ; Command has no parameters: BC = 0 (HL = 0)
  ld bc, $0000
  ld de, $FFFF
  jr .Execute               ; Set Z flag and jump to command execution

ShowHowMsg:
  ld bc, HowMsg
  jr ShowErrMsg
ShowSorryMsg:
  ld bc, SorryMsg
  jr ShowErrMsg
ShowWhatMsg:
  ld bc, WhatMsg
ShowErrMsg:                 ; BC = message address
  ld hl, INPUT_CURSOR_POS
  call ClearLine            ; Clear cmd line before executing the command
  call PrintMsg
  jp GetInputCmd

DisassembleCmd:
  dec a
  jr nz, ShowWhatMsg        ; If not one parameter
  ld (PC_REG), hl           ; Set PC_REG to new address
DisassembleFromHL:
  call DisassemblerInit     ; HL = starting address provided by command parameter
  call ShowPointer
  jp KeyLoop

DisassembleNext:
  ld a, 2                   ; One line to disassemble
  ld (DISASM_CNT), a
  call DisassemblerInit.Disassemble
  call ShowPointer
  ret

ShowPointer:                ; Print '>' sign as pointer in front of the next instruction to be executed
  ld de, DISASM_POINTER_POS
  call SetCursorPos
  ld a, '>'
  rst $20
  ret

EditMemCmd:
  call GetParameter         ; Read first command parameter
  jr z, ShowWhatMsg
  jr c, ShowHowMsg
  call SkipSpaces
  rst $18
  db ','
  db .Space-$-1
.Space:
  rst $18
  db '&'
  db .Ampersand-$-1
.Ampersand:
  cp CR
  jr z, ShowWhatMsg         ; Missing third command parameter
  ; Convert ASCII to HEX for third parameter and write that at the beginning of input buffer
  pop hl                    ; HL = first parameter
.StoreHex:
  call CheckLatch
  call HexToNumCR
  jp z, ShowOKMsg
  jr c, ShowHowMsg
  ld (hl), a
  inc hl
  jr .StoreHex

FillMemCmd:
  cp $2
  jr c, ShowWhatMsg         ; If not at least two parameters. Third is 0 if not provided.
  push de
  ex de, hl
  or a                      ; Reset Carry flag
  sbc hl, de                ; HL = length
  ex de, hl
  pop de
  jr c, ShowHowMsg          ; Quit if end < start
.Loop:
  ld a, c                   ; B = fill byte value
  call Poke
  rst $10                   ; Does whole section has been filled (HL > DE)?
  inc hl
  jr c, .Loop               ; If not, continue
  jp ShowOKMsg

DumpMemCmd:
  dec a
  jp nz, ShowWhatMsg        ; If not one parameter
  ld (DUMP_ADDR), hl
  call MemoryDump
  jp KeyLoop

CMD_TABLE:
        BYTE "D"            ; Command list
        WORD DisassembleCmd
        BYTE "M"
        WORD DumpMemCmd
        BYTE "E"
        WORD EditMemCmd
        BYTE "B"
        WORD BreakCmd
        BYTE "R"
        WORD RegisterCmd
        BYTE "C"
        WORD CopyMemCmd
        BYTE "S"
        WORD SearchMemCmd
        BYTE "F"
        WORD FillMemCmd
        BYTE "P"
        WORD ProceedCmd
        BYTE "G"
        WORD GoCmd
        BYTE "V"
        WORD VersionCmd
        BYTE "X"
        WORD ExitCmd
        BYTE $80            ; End list byte
; TODO Possible additional commands: swap two blocks, compare two blocks, ASCII search and edit

WhatMsg:
  BYTE "WHAT?", $00
HowMsg:
  BYTE "HOW?", $00
SorryMsg:
  BYTE "SORRY", $00
EndMsg:
  BYTE "END", $00
OKMsg:
  BYTE "OK", $00
FlagNames:
  BYTE "SZ H VNC"
RegNames:
  BYTE "AFBCDEHLIXIYSPPC"

MemoryDump:
  ld de, DUMP_CURSOR_POS    ; Set cursor position
  call SetCursorPos
  ld d, 4                   ; Dump 4 lines
  ld hl, (DUMP_ADDR)
PrintMem:                   ; Print memory dump starting from address in HL
  call PrintHex16
  ld a, ':'
  rst $20
  ld b, 8                   ; Print 8 bytes in one row
.Row:
  ld a, ' '
  rst $20
  call Peek
  call PrintAHex8
  inc hl
  djnz .Row
  ld a, CR
  rst $20
  dec d
  jr nz, PrintMem
  ret

VersionCmd:                 ; Number of parameters is not checked, they are not important here!
  ld de, MSG_LINE_CPOS
  call SetCursorPos
  ld a, VERSION
  call PrintAHex8
  jp KeyLoop

CopyMemCmd:
  cp 3
  jp nz, ShowWhatMsg        ; If not three parameters
  ex de, hl                 ; DE = start address, HL = end address
  or a
  sbc hl, de                ; HL = section to be copied length
  jp c, ShowHowMsg          ; If end is before start
  inc hl                    ; Length + 1 (+1 will not be copied)
  push hl                   ; Length to stack
  push de                   ; Start address to stack
  ld d, b
  ld e, c                   ; DE = target section address
  pop hl                    ; HL = source section start address
  pop bc                    ; BC = section length
  rst $10                   ; Is target section start address in source section?
  jr c, .Backword           ; If yes, copy backward
  ldir                      ; If not, copy forward
  jp ShowOKMsg
.Backword:
  add hl, bc                ; HL = start + length + 1
  dec hl                    ; HL = source section end address
  ex de, hl                 ; DE = source section end address, HL = target section start address
  add hl, bc                ; HL = end + length + 1
  dec hl                    ; HL = target section end address
  ex de, hl                 ; DE = target section end address, HL = source section end address
  lddr                      ; Copy section backwards
  jp ShowOKMsg

SearchMemCmd:
  call GetParameter         ; Get first parameter
  jp z, ShowWhatMsg
  jp c, ShowHowMsg
  call GetParameter         ; Get second parameter
  jp z, ShowWhatMsg
  jp z, ShowHowMsg
  call SkipSpaces
  rst $18                   ; Skip not mandatory ','
  db ','
  db .Space-$-1
.Space:
  rst $18                   ; Skip not mandatory '&'
  db '&'
  db .Ampersand-$-1
.Ampersand:
  cp CR
  jp z, ShowWhatMsg         ; Missing third parameter
  ; Convert third parameter ASCII string to HEX and write that at the beginning of input buffer
  ld hl, IN_BUFFER
.ConvertHex
  call HexToNumCR
  jr z, .ConversionEnd
  jp c, ShowHowMsg
  ld (hl), a
  inc hl
  jr .ConvertHex
.ConversionEnd:
  ld (hl), CR               ; CR marks the end of searched string
  ; Move start and end address from stack to corresponding variables
  pop de
  ld (SEARCH_END), de       ; Second command parameter
  pop hl
  ld (SEARCH_START), hl     ; First command parameter
  jr .Loop
  ; Actual search starts here
.SearchNext:                ; HL = start address, DE = end address
  ld hl, (SEARCH_START)
  ld de, (SEARCH_END)
.Loop
  ld bc, IN_BUFFER
.NextByte:
  rst $10
  jr nc, ShowEndMsg         ; End is before or equal to start
  call CheckLatch
  ld a, (bc)                ; A = byte to search for
  cp CR
  jr z, .Found              ; Reached the end of searched string - string has been found
  cp (hl)                   ; Zf = 1 if byte at HL address is equal
  inc hl                    ; Next address
  jr nz, .Loop              ; Jump if not found
  inc bc
  jr .NextByte
.Found:
  ld (SEARCH_START), hl
  push hl                   ; HL = end of found string address
  ld h, b
  ld l, c
  ld bc, IN_BUFFER
  or a                      ; Clear carry flag
  sbc hl, bc                ; HL = searched string length
  ld b, h
  ld c, l                   ; BC = searched string length
  pop hl
  or a
  sbc hl, bc
  ld (DUMP_ADDR), hl        ; HL now to points to start of the found string
  call MemoryDump
  jp KeyLoop

ShowEndMsg:
  ld bc, EndMsg             ; BC = message address
  jr ShowMsg
ShowOKMsg:
  ld bc, OKMsg
ShowMsg:
  call PrintMsg
  jp KeyLoop

PrintMsg:
  call ClearMsgLine
  ld de, MSG_LINE_CPOS
  call SetCursorPos
  ld d, b                   ; DE = BC
  ld e, c
  call PrintString
  ret

SetCursorPos:
  ld hl, CURSORPOS
  ld (hl), e                ; ld (hl), de
  inc hl
  ld (hl), d
  ret

ClearMsgLine:
  ld hl, MSG_LINE_CPOS
  call ClearLine
  ret

ExitCmd:
  call ClearMsgLine
  jp KeyLoop

ShowRegistersPos:
  ld de, REGS_CURSOR_POS
  call SetCursorPos
ShowRegisters:
  ld hl, (AF_REG)
  call PrintReg
  ld hl, (BC_REG)
  call PrintReg
  ld hl, (DE_REG)
  call PrintReg
  ld hl, (HL_REG)
  call PrintReg
  ld hl, (IX_REG)
  call PrintReg
  ld hl, (SP_REG)
  call PrintReg
  ld a, CR
  rst $20
  ; Print alternate registers, IY and (SP)
  ld hl, (AF_ALT)
  call PrintReg
  ld hl, (BC_ALT)
  call PrintReg
  ld hl, (DE_ALT)
  call PrintReg
  ld hl, (HL_ALT)
  call PrintReg
  ld hl, (IY_REG)
  call PrintReg
  ld hl, (SP_REG)
  ld e, (hl)                ; Print two bytes at stack top
  inc hl
  ld d, (hl)
  ld h, d
  ld l, e
  call PrintReg
  ld a, CR                  ; Print final CR
  rst $20
  ; Print "F:"
  ld a, 'F'
  rst $20
  ld a, ':'
  rst $20
  ; Print flags values
  ld hl, (AF_REG)
  ld a, l                   ; A = flags register
  ld de, FlagNames          ; DE = points to flag names
  ld b, 8                   ; 8 flag bits
.NextFlag:
  rla                       ; Carry = flag bit
  push af
  ld a, (de)                ; Get flag name
  jr c, .ShowFlag           ; Flag bit set?
  ld a, '-'                 ; If not, show '-'
.ShowFlag:
  rst $20                   ; Print flag name or space
  pop af
  inc de
  djnz .NextFlag            ; Show next flag bit
  ret

PrintReg:                   ; Prints HL register hex value
  ld a, ' '
  rst $20
  call PrintHex16
  ret

CodeScroll:                 ; Scroll section of five rows by one row up
  ld b, 5
  ld de, DISASM_CURSOR_POS1 ; Start destination address
.Row:
  push bc
  ld bc, 32                 ; Size (row width)
  ld h, b
  ld l, c                   ; HL = Source address
  add hl, de
  ldir
  pop bc
  djnz .Row
  ret

;----------------------------------------------------------
;
;   Convert two HEX ASCII digits to one byte value
;
;  in: DE = pointer to string in input buffer
; out:  A = converted number
;       Cf = set on conversion error or number is just one byte long
;
HexToNum:
  ld a, (de)
  call HexCharToBin         ; Increments DE internally
  ret c
  rlca
  rlca
  rlca
  rlca
  ld c, a
  ld a, (de)
  call HexCharToBin
  ret c
  or c
  ret

;----------------------------------------------------------
;
; Two HEX ASCII digits conversion with checking if it ends with CR
;
HexToNumCR:
  call HexToNum
  ret nc                    ; Conversion OK
  cp CR
  ret z                     ; CR character found (Zf = 1)
  scf
  ret                       ; Conversion error (Cf = 1)

;----------------------------------------------------------
;
; Convert two or four long HEX ASCII digits to 16 bit value in HL
;
HexToNum16:
  rst $28                   ; HL = 0
  call HexToNum
  ret c
  ld l, a
  call HexToNum
  ret c
  ld h, l
  ld l, a
  ret

;----------------------------------------------------------
;
; Convert decimal or Hex ASCII string to 16 bit number at stack top.
; For decimal numbers max 4 decimal digits are allowed (value up to 9999).
;
;  in:   DE = pointer to string in input buffer
; out: (SP) = parameter value
;       Zf = 1 if parameter is missing
;       Cf = 1 on conversion error
;
GetParameter:
  call SkipSpaces           ; Skip SPACE characters, A points to first non blank character
  sub CR                    ; Is that character a CR?
  ret z                     ; No more parameters (A = 0, Zf = 1)
  rst $18                   ; Skip not mandatory comma sign
  db ','
  db .Same-$-1
.Same:
  rst $18
  db '&'
  db .Integer-$-1
  ; Hex number
  call HexToNum16
  jr .Converted
.Integer:
  call Dec2Num16            ; HL = converted number
  ld a, b
  cp 5
  jr nc, .Error             ; For 5+ digits number
.Converted:
  ld a, (de)
  cp ' '                    ; If number ends with SPACE, CR or ',' it's fine
  jr z, .Ok
  cp CR
  jr z, .Ok
  cp ','
  jr z, .Ok
.Error:
  xor a
  inc a                     ; Clear zero flag
  scf
  ret                       ; Error (Cf = 1)
.Ok:
  ex (sp), hl               ; Put parameter to stack, HL = return address
  xor a
  inc a                     ; Clear zero flag
  jp (hl)

;----------------------------------------------------------
;   Convert decimal string to number
;
;  in: DE = decimal string
; out: HL = converted 16 bit number
;       B = no of digits
;
Dec2Num16:
  ld b, 0                   ; B is digit counter
  rst $28                   ; HL = 0
.Loop:
  ld a, (de)                ; Get char
  call IntCharToBin         ; A = digit numeric value
  ret c                     ; Cannot convert character
  inc b
  push bc
  add hl, hl
  ld b, h                   ; ld bc, hl
  ld c ,l
  add hl, hl
  add hl, hl
  add hl, bc                ; HL = HL * 10
  ld c, a
  ld b, 0
  add hl, bc                ; Add converted digit
  pop bc
  jr .Loop

ParamLength:                ; Find string parameter length. Parameter is pointed by DE, length is in C.
  ld c, 0
.Next:
  ld a, (de)                ; A = next parameter character
  cp ' '                    ; Parameter ends with space, comma or CR.
  ret z
  cp ','
  ret z
  cp CR
  ret z
  inc de
  inc c
  jr .Next

CheckLatch:                 ; Check if address is in LATCH memory ($2000-$27FF)
  ld a, h
  and $F8                   ; Mask lower 3 bits of higher address byte ($20-$27)
  cp $20                    ; Is HL address in LATCH?
  ret nz                    ; Go back if address is not in LATCH
  jp ShowSorryMsg           ; If it is in LATCH, display SORRY message and stop

PrintHexWord:               ; Subroutine to print HEX word with '&' at the beginning
  ld a, '&'
  rst $20                   ; Print character
  jp PrintHex16             ; Print HEX value from HL (This is the end of *H command)

;--------------------------------------------------------------------------------------------------

Decode:                     ; DE = DISASM_CURSOR_POS1, DISASM_CNT = 1, DISASS_ADDR temporary set to PC_REG and restored afterwards
  push hl
  ld a, 1                   ; One line to disassemble
  ld (DISASM_CNT), a
  ld de, DISASM_CURSOR_POS1
  ld hl, (DISASS_ADDR)
  push hl
  ld hl, (PC_REG)
  ld (DISASS_ADDR), hl
  call DisassTrace
  pop hl
  ld (DISASS_ADDR), hl
  pop hl
  ret

ToggleScreen:
  call RestoreScreen        ; Show system screen
  call ReadKey              ; Wait for any key
  jp UpdateScreen

Skip:                       ; Skip over current instruction
  ld hl, (PC_REG)           ; HL = target code address
  call Decode
  ld a, (INST_LEN)
  ld c, a                   ; BC = instruction length
  ld b, 0
  add hl, bc
  ld (PC_REG), hl
  call DisassembleNext
  ret

TraceBack:
  ld hl, (PC_REG)
  dec hl                    ; PC = PC - 1
  ld (PC_REG), hl
  jp DisassembleFromHL

;-----------------------------------------------------------
;   Single step (trace next instruction)
;
; Copies instruction to sandbox, appends a CALL Break, then executes the instruction.
; Instructions that may change order of execution (eg. JR, JP, CALL) are emulated.
;
TraceNext:
  ld hl, TraceCode
  ld de, TRACE_INSTR
  ld bc, 7                  ; Initialize trace code buffer to four NOPs and CALL Break
  ldir  ; TODO Copy CALL Break only once at start, it's not changing with new breakpoint address!
  ld hl, (PC_REG)           ; HL = target code address
  call Decode               ; Get opcode attributes
  call RestoreScreen        ; This is after Decode call because Decode writes disassembled line to screen
  ld a, (INST_LEN)
  ld b, $0
  ld c, a                   ; BC = instruction length
  ld de, TRACE_INSTR
  ldir                      ; Copy instruction to trace code buffer
  ld (BRK_ADDR), hl         ; Set current breakpoint address, hl = (PC_REG) + (INST_LEN)
  call Trap                 ; Emulate unsafe instructions
  jr z, TraceAddr           ; If emulated then don't execute
  ld hl, TRACE_INSTR
  ld (PC_REG), hl           ; PC = code in trace buffer
  jr TraceCont              ; Execute instruction, then break

; Initial trace code (NOPs) executed during tracing. 7 bytes copied to RAM at TRACE_INSTR
TraceCode:
  DB 0, 0, 0, 0, $CD, Break & $FF, Break >> 8

;-----------------------------------------------------------
;   G - Go
;-----------------------------------------------------------
;
; Execute code without RET (until breakpoint or forever!)
;
; equivalent to JP xxxx
;
; input: HL = PC_REG contents
;
GoCmd:                      ; HL = execution start address
  dec a
  jp nz, ShowWhatMsg        ; If not one parameter
  jr GoFromHL

;-----------------------------------------------------------
;   Trace out - execute until RET
;-----------------------------------------------------------

; TraceOut:
;   ld hl, (PC_REG)
;   jr TraceCall

;-----------------------------------------------------------
;   P - Proceed
;
; Execute code as subroutine
;
; Equivalent to CALL xxxx, break
;
; input: HL = address to be called
;
ProceedCmd:
  dec	a
  jp nz, ShowWhatMsg        ; If not one parameter
TraceCall:
  ex de, hl
  ld hl, TRACE_INSTR
  ld (hl), $CD              ; CALL instruction operand
  inc hl
  ld (hl), e
  inc hl
  ld (hl), d
  inc hl
  ld (hl), 0                ; Insert one NOP at the end because TRACE_INSTR size is four bytes
  inc hl
  call SetBrakeJump
  ld hl, TRACE_INSTR
;-----------------------------------------------------------
;   Trace instruction(s)
;-----------------------------------------------------------
;
; Restore all registers, then jump into target code.
; Returns to Break through CALL Break.
;
GoFromHL:                   ; From address in HL
  ld (PC_REG), hl           ; Set PC
TraceGo:                    ; From PC_REG
  call RestoreScreen        ; Show system screen
  call SetBreaks            ; Set all breakpoints to it's memory addresses
TraceCont:                  ; Continue tracing
  ld sp, AF_ALT             ; SP points to alternate registers
  pop af                    ; Load AF'
  ex af, af'
  pop bc
  pop de                    ; Load alternate registers
  pop hl
  exx
  pop af
  pop bc
  pop de                    ; Load base registers
  pop hl
  pop ix
  ; Don't restore IY, just update SP
  ld sp, (SP_REG)           ; SP = SP_REG
  push hl                   ; Push HL_REG onto stack to be restored by EX
  ld hl, (PC_REG)           ; HL = PC_REG
  ex (sp), hl               ; PC_REG on stack, HL = HL_REG
  ret                       ; Jump to code at PC_REG

TraceAddr:
  ld (PC_REG), hl           ; Set execution address
  jp UpdateScreen           ; Update screen and enter trace loop

;-----------------------------------------------------------
;  Breakpoint hit, save all registers and enter trace loop.
;
Break:
  ex (sp), hl               ; Get return address
  ld (PC_REG), hl           ; Save return address (PC after breakpoint)
  ex (sp), hl               ; Restore HL
  inc sp
  inc sp                    ; SP + 2 = value before execution of CALL Break
  ld (SP_REG), sp           ; Save SP
  ld sp, SP_REG             ; Switch stack to register save area
  push iy
  push ix
  push hl
  push de                   ; Save base registers
  push bc
  push af
  exx
  push hl
  push de                   ; Save alternate registers
  push bc
  exx
  ex af, af'
  push af                   ; Save AF'
  ex af, af'
  ld sp, STACK_TOP          ; Switch SP to debug stack
  call SaveScreen           ; Save system screen
  ld hl, (PC_REG)
  ld bc, TRACE_BRK + 3      ; BC = after CALL Break at end of sandbox
  or a
  sbc hl, bc                ; Is it equal to PC? (single step)
  jr nz, .NotSinglestep
  ld hl, (BRK_ADDR)         ; Yes, PC = address of next instruction
  jr .InSandbox
.NotSinglestep:
  add hl, bc
  dec bc                    ; BC = after CALL nnnn, points to CALL Break in sandbox
  dec bc
  dec bc
  or a
  sbc hl, bc                ; Is it equal to PC ? (stepped into subroutine)
  jr nz,.NotSandbox
  ld hl, (SP_REG)           ; Yes, HL = target stack
  ld e, (hl)
  inc hl
  ld d, (hl)                ; Pop return address off target stack to DE
  inc hl
  ld (SP_REG), hl
  ex de, hl                 ; Tracing from return address
.InSandbox:
  jr TraceAddr
.NotSandbox:                ; Not single stepping, came here because execution hit the breakpoint or CALL Break in user code
  ld de, (PC_REG)
  dec de
  dec de
  dec de                    ; PC - 3 = address of CALL Break
  call FindBreak            ; Hit one of breakpoints?
  jr nz, .NoBrk
  ld (PC_REG), de           ; Yes, PC = breakpoint address
  call RemoveBreaks         ; Restore original code at all breakpoints
.NoBrk:
  jp UpdateScreen           ; Update screen and enter trace loop from breakpoint address

;----------------------------------------------------------
;   R - Set register value
;----------------------------------------------------------

RegisterCmd:                ; DE points to next character after 'R' in input buffer
  call SkipSpaces           ; Skip SPACE characters, A points to first non blank character
  push de
.Count:
  call ParamLength
  ld a, c
  and a                     ; Shorter version of: cp 0
  jp z, ShowWhatMsg
  cp 3
  jp nc, ShowHowMsg         ; If register length >= 3
  pop de                    ; DE = points back to first register name character
  ld hl, RegNames
  ld b, 0                   ; B = index in RegNames list
.Find:
  push bc
  push de
  push hl
  call StrCmp               ; Compare input pointed by DE to register name in the list, pointed by HL, for string length in C
  pop hl
  pop de
  pop bc
  jr z, .RegFound
  inc hl                    ; Next register name
  bit 1, c                  ; Is it register pair?
  jr z,.Next
  inc hl                    ; Yes, next pair name
.Next:
  inc b
  ld a, 8                   ; 8 register names (single or pairs)
  cp b
  jp z, ShowHowMsg          ; Can't find register name
  jr .Find
.RegFound:
  ld a, b
  bit 1, c                  ; Register pair?
  jr z, .Reverse
  add a                     ; Yes, index * 2
  jr .Index
.Reverse:
  xor 1                     ; No, reverse register order
.Index:
  push bc
  ld c, a                   ; C = register list index
  ld b, 0
  ld hl, AF_REG
  add hl, bc                ; HL = register address
  pop bc
  push hl                   ; Save register address
  ld b, c                   ; B = register name length
.Inc:
  inc de
  djnz .Inc                 ; DE = DE + C to skip first parameter
  push bc
  call GetParameter         ; Read second command parameter
  jp z, ShowWhatMsg         ; If missing second parameter
  jp c, ShowHowMsg          ; If number conversion fails
  pop de                    ; DE = second parameter pushed to stack by GetParameter
  pop bc
  pop hl                    ; Restore register address
  ld (hl), e                ; Store register value
  bit 1, c                  ; Register pair?
  jr z, .Done
  inc hl                    ; Yes, store upper register value
  ld (hl), d
  ; If changed register is PC then show disassembled code from that address!
  dec hl
  ld de, PC_REG
  rst $10
  jr nz, .Done              ; If HL != DE then register is not PC
  ; Show disassembled code from newly entered address
  ld hl, (PC_REG)
  call DisassembleFromHL
.Done:
  call ShowRegistersPos
  jp KeyLoop

;----------------------------------------------------------
;  Compare memory to string of specified length
;----------------------------------------------------------
;  in: HL = string
;      DE = memory pointer
;       C = length of string
;
; out: Z = found
;
StrCmp: ; TODO This could be inlined where it is called from!
  ld a, (de)                ; Read byte to A register
  cp (hl)                   ; Compare values
  ret nz
  inc de
  inc hl                    ; Next byte
  dec c                     ; Decrement string length
  jr nz, StrCmp
  ret

;----------------------------------------------------------
;   B - Set/Remove/Show a Breakpoint
;----------------------------------------------------------

BreakCmd:
  or a
  jr z, ShowBreakpoints     ; If zero parameters
  dec a
  jp z, ShowWhatMsg         ; If one parameter
  dec a
  jp nz, ShowWhatMsg        ; If more then two parameters
  ld a, d
  or a
  jr z, .Zero               ; Zero address is allowed for removing breakpoints
  cp $2C                    ; Check if address is less then $2C00, this area is not allowed for breakpoints
  jp c, ShowSorryMsg
.Zero:
  xor a
  ld b, l                   ; Move breakpoint number from HL to B register
.Mult:
  dec b
  jr z, .Index
  add brk                   ; A = number * brk size (A is 0 at the beginning)
  jr .Mult
.Index:
  ld hl, BREAKPOINTS
  ld c, a
  add hl, bc                ; HL = points to breakpoint in array
  ld a, d                   ; DE = breakpoint address
  or e
  jr z, .SetBrk             ; if address = 0 then status = 0 (inactive)
  ld a, 1                   ; else status = 1 (active)
.SetBrk:
  ld (hl), a                ; brk.status = status
  inc hl
  ld (hl), e                ; brk.addr = address
  inc hl
  ld (hl), d
  jr ShowBreakpoints        ; Show all breakpoints at the end

;----------------------------------------------------------
;        Remove All Breakpoints
;----------------------------------------------------------

; ClearBreaks:
;   call RemoveBreaks         ; First remove breakpoints from memory, for any case
;   ld hl, BREAKPOINTS
;   ld b, brk * NUMBRKS
;   ld c, 0
; .Next:
;   ld (hl), c
;   inc hl
;   djnz .Next
;   ret

;----------------------------------------------------------
;        Show All Breakpoints
;
; Display format: 1:xxxx 2:xxxx 3:xxxx 4:xxxx
;
ShowBreakpoints:
  ld de, MSG_LINE_CPOS
  call SetCursorPos         ; Set cursor to beginning of message line
  ld hl, BREAKPOINTS
  ld b, NUMBRKS             ; B = number of breakpoints
  ld c, '1'                 ; C = breakpoint number '1'
.BpLoop:
  push bc
  ld a, c
  rst $20
  ld a, ':'
  rst $20
  ld a, (hl)                ; A = breakpoint status
  or a
  jr nz, .BpActive          ; 1  = active
  ld de, EmptyBP            ; 0 = inactive
  CALL PrintString
  jr .BpNext
.BpActive:
  push hl
  inc hl
  ld e, (hl)
  inc hl
  ld d, (hl)                ; DE = breakpoint address
  ex de, hl
  call PrintHex16           ; Print breakpoint address
  ld a, ' '
  rst $20
  pop hl
.BpNext:
  ld de, brk                ; DE = breakpoint size
  add hl, de                ; Advance to next breakpoint
  pop bc
  inc c                     ; Next breakpoint number
  djnz .BpLoop
  jp KeyLoop

EmptyBP:
  db "---- ", $00

;----------------------------------------------------------
;         Remove breakpoint from target code
;
;  in: HL = points to breakpoint address in array
;
RemoveBrk:
  ld a, (hl)
  or a
  ret z                     ; Return if breakpoint not active
  inc hl
  ld e, (hl)
  inc hl
  ld d, (hl)                ; DE = target address
  inc hl                    ; HL = address of temporary breakpoint space where are three bytes from breakpoint target address
  ; Check if at target address is call to breakpoint handler routine
  inc de
  ld a, (de)
  cp Break & $FF            ; Check if at BP + 1 is breakpoint handler lower address byte?
  ret nz                    ; If not then skip coping back operation
  inc de
  ld a, (de)
  cp Break >> 8             ; Check if at BP + 2 is breakpoint handler higher address byte?
  ret nz                    ; If not then skip coping back operation
  dec de                    ; Restore DE to breakpoint address value with two DEC instructions
  dec de
CopyPtrMem:
  ld bc, $0003
  ldir                      ; Copy 3 bytes to or from temporary space
  ret

;----------------------------------------------------------
;     Remove all breakpoints from target code
;----------------------------------------------------------

RemoveBreaks:
  ld b, NUMBRKS             ; B = number of breakpoints
  ld hl, BREAKPOINTS        ; HL = breakpoint array
.Next:
  push hl
  call RemoveBrk            ; Remove the breakpoint
  pop hl
  ld de, brk                ; Size of the breakpoint
  add hl, de
  djnz .Next                ; Next breakpoint
  ret

;----------------------------------------------------------
;        Check if breakpoint exists at PC address
;
;  in: DE = PC
;       z = breakpoint found
;      nz = not found
;
FindBreak:
  ld b, NUMBRKS             ; B = number of breakpoints
  ld hl, BREAKPOINTS        ; HL = breakpoint array
.Next:
  inc hl                    ; Skip brk.status
  ld c, (hl)
  inc hl
  ld a, (hl)                ; A,C = brk.address
  inc hl
  inc hl                    ; Skip brk.tmp (3 bytes)
  inc hl
  inc hl
  cp d
  jr nz, .notFound
  ld a, c                   ; DE = our address?
  cp e
  ret z                     ; Breakpoint found
.notFound:
  djnz .Next                ; Next breakpoint
  ret

;----------------------------------------------------------
;                  Set Breakpoint
;----------------------------------------------------------
; Replaces target opcode with CALL Break.
; Saves original chunk of code so that breakpoint can be removed later.
;
;  in: HL = breakpoint address in array
;
SetBreakpoint:
  ld a, (hl)                ; A = status
  or a
  ret z                     ; Return if inactive
  push hl
  inc hl
  ld e, (hl)
  inc hl
  ld d, (hl)                ; DE = breakpoint address
  call CmpBPandPC
  jr z, .End                ; Don't set breakpoint if it's at current PC or next two bytes (to avoid forever loop)
  push de
  inc hl                    ; HL points to temporary breakpoint area
  ex de, hl
  call CopyPtrMem           ; Copy 3 bytes from breakpoint address to brk.tmp
  pop hl                    ; HL = breakpoint address
  call SetBrakeJump
.End:
  pop hl
  ret

CmpBPandPC:                 ; Check if breakpoint address is equal to PC, PC + 1, PC + 2 (because CALL Break is three bytes long). Z flag is set if it's true.
  push hl
  ld b, 3
.Loop:
  ld hl, (PC_REG)
  rst $10
  jr z, .End
  inc hl
  djnz .Loop
.End:
  pop hl
  ret

SetBrakeJump:
  ld (hl), $CD
  inc hl
  ld (hl), Break & $FF
  inc hl
  ld (hl), Break >> 8
  ret

;----------------------------------------------------------
;             Set all breakpoints
;----------------------------------------------------------

SetBreaks:
  ld b, NUMBRKS             ; B = number of breakpoints
  ld hl, BREAKPOINTS        ; HL = points to 1st breakpoint
.Next:
  push bc
  call SetBreakpoint        ; Set breakpoint
  pop bc
  ld de, brk                ; DE = size of space for one breakpoint
  add hl, de                ; HL = next breakpoint in array
  djnz .Next                ; Next breakpoint
  ret

;-------------------------------------------------------------
;            Trap unsafe instructions
;
; Instructions such as JR, JP, CALL are simulated to maintain
; control of code execution. Others such as HALT are simply ignored.
;
;  in: HL = target PC
; out:  Z = instruction was simulated, HL = new PC
;      NZ = instruction is safe to execute
;
Trap:
  ld de, TRACE_INSTR        ; DE points to copy of instruction in sand box
  ld a, (de)                ; Get opcode
  cp $76                    ; 76 = HALT
  dec hl                    ; If HALT then PC - 1 (this is controlled loop forever from PC-1 to PC and back)
  ret z
  inc hl
  cp $FB                    ; FB = EI
  ret z                     ; Ignore EI
  cp $ED                    ; ED prefix?
  inc de                    ; Next addr
  jr nz, .doJphl            ; If not ED prefix
  ld a, (de)                ; Yes, get next opcode byte
  cp $46                    ; ED46 = IM0
  ret z
  cp $5E                    ; ED5E = IM2
  ret z
  cp $4D
  jp z, .retn               ; ED4D = RETI
  cp $45
  jp z, .retn               ; ED45 = RETN
  ret
.doJphl:
  cp $E9                    ; E9 = JP (HL)
  jr nz, .doDD
  ld hl, (HL_REG)           ; PC = HL
  ret                       ; Return simulated
.doDD:
  cp $DD                    ; DD instruction?
  jr nz, .doFD              ; If not DD prefix
  ld a, (de)                ; Yes, get next opcode byte
  cp $E9                    ; DDE9 = JP (IX)
  jr nz, .doRst             ; No, absorb DD
  ld hl, (IX_REG)           ; Yes, PC = IX
  ret
.doFD:
  cp $FD                    ; FD instruction?
  jr nz, .doRst             ; If not FD prefix
  ld a, (de)                ; Yes, get next opcode byte
  cp $E9                    ; FDE9 = JP (IY)
  jr nz, .doRst             ; No, absorb FD
  ld hl, (IY_REG)           ; Yes, PC = IY
  ret
.doRst:
  ld c, a
  and $C7                   ; Mask three bits for RST address
  cp $C7                    ; RST xx?
  ld a, c
  jr nz, .doRet             ; If not RST instruction
  ; Handling RST $nn instructions
  and $38                   ; Mask all other then bits 3-5 for RST address
  ld (de), a                ; Low target address byte
  inc de
  xor a                     ; Target address = $0008~$0030, $38 is video interrupt, not called directly
  ld (de), a                ; Zero to higher address byte
  jp .traceCall             ; Treat as CALL
.doRet:
  cp $C9                    ; RET
  jp z, .retn
  cp $C3                    ; JP
  jr z, .jump
  cp $CD                    ; CALL
  jp z,.traceCall
  cp $18                    ; JR
  jr z, .jpr
  ld b, 3                   ; B value represents condition code: 3-C, 2-NC, 1-Z, 0-NZ
  cp $38                    ; JR C
  jr z, .jrCC
  dec b
  cp $30                    ; JR NC
  jr z, .jrCC
  dec b
  cp $28                    ; JR Z
  jr z, .jrCC
  dec b
  cp $20                    ; JR NZ
  jr z, .jrCC
  cp $10                    ; DJNZ
  jr z, .decjnz
  rla                       ; Bit 7 to C flag
  ret nc
  rla
  ret nc
  dec de
  ld a, (de)                ; Get opcode
  rra
  ret c                     ; If bit 0 = 1 then return c
  rra
  jr c, .bit1               ; If bit 1 = 1 then return c
  rra
  jp c, .callCC
  jr .retCC                 ; RET cc
.bit1:
  rra
  ret c                     ; If bit 2 = 1 then return
  ; JP CC
  call TestCC               ; Is condition met?
  jr nc, .sim1              ; No, it's not
.jump:
  ld hl, (TRACE_INSTR + 1)  ; Yes, HL = dest address
.sim1:
  xor a                     ; Return simulated
  ret
  ; JR CC
.jrCC:
  ld a, b
  call TestCC               ; Is condition met?
  jr nc, .sim2              ; No, it's not
.jpr:
  ld a, (de)
  ld c, a
  rla                       ; Calculate destination address, C flag set if jump is negative
  sbc a, a
  ld b, a                   ; BC is sign extended A
  add hl, bc                ; HL = destination address
.sim2:
  xor a
  ret                       ; Return simulated
  ; DJNZ
.decjnz:
  ld bc, (BC_REG)
  dec b
  ld (BC_REG), bc
  jr nz, .jpr               ; Decrement B register, the rest is the same as for JR
  ret                       ; If B = 0, return simulated
  ; RET CC
.retCC:
  call TestCC               ; Is condition met?
  jr nc, .sim2              ; No, it's not
  ; RET
.retn:
  ld hl, (SP_REG)
  ld e, (hl)
  inc hl                    ; Pop address from trace stack to DE
  ld d, (hl)
  inc hl
  ld (SP_REG), hl
  ex de, hl                 ; HL = return address
  xor a
  ret                       ; Return simulated
  ; CALL CC
.callCC:
  call TestCC               ; Conditon met?
  jr c, .traceCall          ; Yes
  xor a
  ret                       ; No, return simulated
.traceCall:
  ld a, (KEY_CODE)          ; Last pressed key
  cp '.'                    ; Is it stepping into key?
  ret nz                    ; If not, return nz = trace over
  ; Trace into
  ex de, hl                 ; DE = return address
  ld hl, (SP_REG)           ; HL = trace SP
  dec hl
  ld (hl), d
  dec hl                    ; Push return address onto trace stack
  ld (hl), e
  ld (SP_REG), hl           ; Update trace SP
  jp .jump                  ; Return Z = trace into

;----------------------------------------------------------
;   Process Condition Code
;
;   in: A = Condition Code
;  out: Carry set = Condition met
;
TestCC:
  ld bc, (AF_REG)           ; C = Flags register value
  and 7                     ; Mask low 3 bits in CC
  ld b, a                   ; B = CC
  ld a, c                   ; A = Flags
  ld c, b
  srl c                     ; CC first two bits = 00?
  jr z, .L8CD4              ; Yes
  dec c                     ; C = 01?
  jr z, .L8CDA              ; Yes
  dec c                     ; C = 10?
  jr z, .L8CD8              ; Yes
  rrca                      ; CC = 11x, test S flag (bit 7)
.L8CD4:
  rrca                      ; CC = 00x, test Z flag (bit 6)
  rrca
  rrca
  rrca
.L8CD8:
  rrca                      ; CC = 10x, test P/V flag (bit 2)
  rrca
.L8CDA:
  rrca                      ; CC = 01x, test C flag (bit 0)
  bit 0, b                  ; CC bit 0 = 1?
  ret nz                    ; Yes, CC (eg. Z)
  ccf                       ; Else NOT CC (eg. NZ)
  ret

;----------------------------------------------------------
;      Is address inside visible screen area?
;----------------------------------------------------------
;   in: HL = address
;  out:  C = in screen
;       NC = not in screen
;
; uses: A
;
IsInScreen:
  ld a, h
  cp $2A                    ; Equal or above $2A00?
  ret nc
  cp $28                    ; Below $2800?
  ret nc
  ccf                       ; NC = not in screen
  ret

;-------------------------------------------
;        Read byte from memory
;-------------------------------------------
;
; in: HL = address
; out: A = byte
;
; If in screen then read from saved system screen.
;
Peek:
  call IsInScreen
  jr c, .InScreen
  ld a, (hl)                ; Load directly from memory
  ret
.InScreen:
  push hl
  push de
  ld de, VIDEO_OFFSET
  add hl, de                ; Add offset to peek address
  pop de
  ld a, (hl)                ; Peek from screen buffer
  pop hl
  ret

;-------------------------------------------
;        Write byte to memory
;-------------------------------------------
;
; in: HL = address
;      A = byte
;
; If in screen then write to saved system screen.
;
Poke:
  push af
  call IsInScreen
  jr c, .InScreen
  pop af
  ld (hl), a                ; Store directly to memory
  ret
.InScreen:
  pop af
  push hl
  push de
  ld de, VIDEO_OFFSET
  add hl, de
  pop de
  ld (hl), a                ; Store to screen buffer
  pop hl
  ret

SaveScreen:
  push hl
  push de
  push bc
  ld hl, VIDEORAM
  ld de, SCREEN_SAVE
  ld bc, VRAMLEN
  ldir                      ; Save chars
  ld hl, (CURSORPOS)
  ld (CURSOR_SAVE), hl      ; Save cursor position
  pop bc
  pop de
  pop hl
  ret

RestoreScreen:
  push hl
  push de
  push bc
  ld hl, SCREEN_SAVE
  ld de, VIDEORAM
  ld bc, VRAMLEN
  ldir                      ; Restore chars
  ld hl, (CURSOR_SAVE)
  ld (CURSORPOS), hl        ; Restore cursor position
  pop bc
  pop de
  pop hl
  ret

Exit:
  call RestoreScreen        ; Restore system screen
  ld sp, (SP_SYS)           ; Restore system stack pointer
  call ReadKey              ; Read 'X' key to not be shown at the restored screen
  ret

;--------------------------------------------------------------------------------------------------

DisassemblerInit:
  ld (DISASS_ADDR), hl      ; Save start address
  ld a, 7                   ; Six lines to disassemble
  ld (DISASM_CNT), a
.Disassemble:
  ld hl, DISASM_CNT
  dec (hl)
  ret z
  ld bc, .Disassemble       ; Return address after one chunk has been dissasembled...
  push bc                   ; ... to stack
  ld hl, (DISASS_ADDR)      ; HL = start address
  ld de, $FFFF              ; DE = end address, fixed to $FFFF
  rst $10                   ; Compare HL with DE and and set C and Z flags
  ret nc                    ; If equal (or if 1st greater then 2nd) then end of command
  call CodeScroll
  ld de, DISASM_CURSOR_POS6
  ld h, d                   ; HL = DE
  ld l, e
  call ClearLine
DisassTrace:                ; DE = DISASM_CURSOR_POS1, DISASM_CNT = 1, DISASS_ADDR temporary set to PC_REG and restored afterwards
  call SetCursorPos
  ld a, 0
  ld (INST_LEN), a          ; Clear instruction length
  ld hl, (DISASS_ADDR)      ; HL = dissasembling start address
  call PrintHex16           ; Print HEX address value from HL (four digit address at the beginning of the line)
  ld a, ' '
  rst $20                   ; Print space
  call CheckLatch           ; If address is in latch area then SORRY
  ld hl, PrintInstruction   ; Next address...
  push hl                   ; ...to stack
  rst $28
  ld (CODE_LEFT), hl        ; Clear CODE_LEFT and CODE_RIGHT locations
  call HexCode              ; Read next byte to be decoded to A register and print it as HEX value
  ld c, a                   ; C = byte to be decoded
  cp $ED                    ; Is byte ED prefix?
  jr nz, .CheckDD           ; If not, continue
  call SavePrefixNext       ; If it is ED, $2BE5 = $ED; address + 1; Print next HEX byte
  jr ED_Prefix
.CheckDD:
  cp $DD                    ; Is byte DD prefix? (IX instruction)
  jr nz, .CheckFD           ; If not, continue
  call SavePrefixNext
  jr .CheckCB               ; Check CB after DD as a second opcode byte
.CheckFD:
  cp $FD                    ; Is byte FD prefix? (IY instruction)
  call z, SavePrefixNext
.CheckCB:
  cp $CB                    ; Is byte the CB prefix?
  jr nz, .FindOpcode        ; If not, continue
  call SavePrefixNext
  jr CB_Prefix
.FindOpcode:                ; C = PREFIX, A = opcode
  ld b, a
  ld a, c
  cp $DD
  jr z, DDFD_Prefix
  ld a, c
  cp $FD
  jr z, DDFD_Prefix

  ld c, a
  ld hl, SIMPLE_TABLE
  call SearchSimpleTable    ; Recognize instruction opcode
  call c, CheckGeneralTable
  ret nc                    ; If found skip further searching
  ld a, c
  ld hl, BLOCK_TABLE
  call SearchBlockTable
  jr c, Unknown             ; Opcode not found
  ret

DDFD_Prefix:
  ld c, b                   ; B = opcode
  ld hl, DD_FD_TABLE
  call SearchTable
  jr c, Unknown
  ret

ED_Prefix:
  ld c, a
  ld hl, ED2_TABLE          ; First, check simple instructions
  call SearchSimpleTable    ; Recognize instruction opcode
  ret nc
  ld hl, ED_TABLE           ; Check ED_TABLE
  call SearchTable
  jr c, Unknown             ; Opcode not found
  ret

CB_Prefix:
  ; Check DD/FD prefix (C register) before CB prefix
  ld b, a                   ; B = opcode value
  ld a, c
  cp $DD
  jr z, .DDFD_CB_Prefix
  cp $FD
  jr z, .DDFD_CB_Prefix
  ; CB prefix w/o DD or FD
  ld c, b
  ld hl, CB_BLOCK_TABLE
  call SearchBlockTable
  jr c, Unknown
  ret
.DDFD_CB_Prefix:            ; Special case!
  ld hl, VALUE_1            ; Save displacement value
  ld (hl), b
  call NextByte             ; Read next byte because it is the opcode byte
  ld c, a
  ld hl, DDFD_CB_TABLE
  call SearchTable
  jr c, Unknown             ; Opcode not found
  ret

CheckGeneralTable:
  ld a, c
  bit 7, a                  ; A = operand to find
  jr nz, .General2
  ld hl, GENERAL_TABLE      ; Check GENERAL_TABLE for operand value smaller then $80
  jr .SearchTable
.General2:
  ld hl, GENERAL2_TABLE     ; Check GENERAL2_TABLE for operand value larger then $80
.SearchTable:
  call SearchTable
  ret

SavePrefixNext:
  ld hl, PREFIX
  ld (hl), c
NextByte:
  call IncDisassStart
HexCode:
  ld hl, INST_LEN
  inc (hl)
  ld hl, (DISASS_ADDR)
  ld a, (hl)
HexA:
  push af
  push bc
  push de
  call PrintAHex8
  pop de
  pop bc
  pop af
  ret

Unknown:
  pop af                    ; Just remove address from stack to not call PrintInstruction
  call TextGap
  ld a, '?'                 ; '?' character
  rst $20
IncDisassStart:
  ld hl, (DISASS_ADDR)      ; Increment current disassembling address
  inc hl
  ld (DISASS_ADDR), hl
  ret

PrintFromTabel2:
  ld hl, TABEL2
PrintFromTable:             ; Print mnemonic
  dec a
  jr z, .Print
.Find:
  inc hl                    ; Before INC, HL is pointing to table beginning
  bit 7, (hl)               ; Test for string end (value $80)
  jr z, .Find               ; Skip A mnemonics
  jr PrintFromTable
.Print:
  ld a, (hl)                ; A contains character code from table (Eg. CC for L letter from LD instruction at $1F8F)
  and $7F                   ; Characters in the table have set most significant bit and this instruction will reset it
  rst $20                   ; Print ASCII character in A to the screen
  inc hl
  bit 7, (hl)               ; Test for string end (value $80)
  jr z, .Print              ; Repeat until the end of mnemonic
  ret

SearchSimpleTable:          ; C = byte to be decoded, IX = start of the opcodes table
  ld a, c
  xor (hl)                  ; Compare values
  inc hl                    ; INC two times because one table entry is two bytes, INC before jump to End becouse there will need that value
  jr z, IndexFound.End
  inc hl
  ld a, (hl)
  or a                      ; Is it the end of the table ($00 byte at the end)?
  jr nz, SearchSimpleTable
  scf                       ; C flag set if opcode is not found
  ret

SearchTable:                ; C = byte to be decoded, IX = start of the opcodes table
  ld de, $04
.Loop:
  ld a, c
  xor (hl)                  ; Compare values
  jr z, IndexFound
  add hl, de                ; Increment IX by 4 because table row is 4 bytes
  ld a, (hl)
  or a                      ; Is it the end of the table ($00 byte at the end)?
  jr nz, .Loop
  scf                       ; C flag set if opcode is not found
  ret

IndexFound:                 ; Read table data for index pointed by IX and read optional data operands, put these into temporary variables.
  ld a, c                   ; C = instruction opcode byte
  ld (OPCODE), a
  inc hl
  inc hl
  ld a, (hl)                ; +2
  ld (CODE_LEFT), a
  bit 7, a
  call nz, .ReadByte        ; Jump for codes $80 and bigger
  inc hl
  ld a, (hl)                ; +3
  ld (CODE_RIGHT), a
  bit 7, a
  call nz, .ReadByte        ; Jump for codes $80 and bigger (Only one of CODE_LEFT and CODE_RIGHT values need reading more data)
  dec hl
  dec hl
.End:
  ld a, (hl)                ; +1
  ld (TABEL1_INDEX), a
  ret
.ReadByte:
  ld e, a
  push hl
  call NextByte
  pop hl
  ld (VALUE_1), a           ; Save second instruction byte
  ld a, e
  bit 6, a
  ret z
  push hl
  call NextByte
  pop hl
  ld (VALUE_2), a           ; Save third instruction byte
  ret

SearchBlockTable:           ; C = byte to be decoded, IX = start of the opcodes table
  ld de, $04                ; Table row size in bytes
  ld a, (hl)                ; A has first byte from first table row
.TableLoop:
  cp c                      ; Next lines will check if A <= C < A+8
  jr z, IndexFound          ; Jump if A = C
  jr nc, .Next              ; Jump if A > C
  ; A < C
  add a, $07
  cp c
  jr nc, IndexFound         ; Jump if A >= C
.Next:
  add hl, de                ; Increment IX by 4
  ld a, (hl)
  or a                      ; Is it the end of the table ($00 byte at the end)?
  jr nz, .TableLoop
  scf                       ; C flag set if opcode is not found
  ret

TextGap:
  ld b, $0E                 ; Align cursor position to column 14
.GapLoop:
  ld a, (CURSORPOS)
  and $1F                   ; Take into account only 5 bits (there are 32 columns)
  cp b
  ret nc                    ; Return if column index in A >= $0E
  ld a, ' '
  rst $20                   ; Print space character
  jr .GapLoop

PrintInstruction:           ; Print mnemonic and operands
  call TextGap
  ld a, (TABEL1_INDEX)
  ld hl, TABEL1
  inc a                     ; Increment A because it will be decremented at the beginning subroutine call
  call PrintFromTable
  ld b, $13                 ; Align output to column 19 for printing operands
  call TextGap.GapLoop
  ld a, (CODE_LEFT)
  or a
  call nz, .PrtOperand
  ld a, (CODE_RIGHT)
  ld b, a
  or a
  ld a, ','                 ; ',' character
  call nz, $0020
  ld a, b
  call nz, .PrtOperand
  jp IncDisassStart         ; Go to process the next instruction
  ;call IncDisassStart         ; Go to process the next instruction
  ;ret
.PrtOperand:
  cp $1F
  jr c, .PrintReg           ; Jump if less then $1F, these values are in TABEL2
  cp CODE_REG
  ld b, $0                  ; Set displacement value in B to no displacement
  jp c, .IndirectRegsByCode ; If less then CODE_REG then these are indirect register codes
  jr nz, .NotCodeReg
  ; CODE_REG
  ld a, (OPCODE)            ; Code is CODE_REG, read register value from opcode byte
  and $7                    ; 3-bit masking. Operand lowest 3 bits are register code
  cp $6                     ; Test HL register index
  jp z, .IndirectRegs       ; For (HL)
  add $8                    ; Add 8 for TABLE2 B register index
  cp 15                     ; For register index 15 decimal (A register, code: 111) decrease by one
  jr nz, .PrintReg
  dec a                     ; For A register table index
.PrintReg:                  ; Print register label from TABEL2
  cp CODE_IX
  jr nz, .NotIXReg
  call ChangeIXToIY
.NotIXReg:
  call PrintFromTabel2
  ret
.NotCodeReg:
  bit 7, a
  jr z, .Numbers            ; Jump if code is less then $80 (bit 7 is 0)
  bit 6, a
  jr z, .OneByte            ; Jump if code is in range of $80-$BF (bit 7 is 1, bit 6 is 0)
  cp CODE_NN
  bit 0, a
  ld hl, (VALUE_1)          ; Before JR because it is used in both branches
  jr z, .NN_Indirect
  call PrintHexWord
  ret
.NN_Indirect:               ; CODE_NN_INDIRECT
  ld a, '('
  rst $20
  call PrintHexWord
  ld a, ')'
  rst $20
  ret
.OneByte:
  bit 0, a
  jr nz, .Op_IXD            ; CODE_IX_D ($81)
  bit 1, a
  jr nz, .Op_N              ; CODE_N ($82)
  bit 2, a
  jr nz, .Op_N_Indirect     ; CODE_N_INDIRECT ($84)
  bit 3, a
  jr nz, .Op_D_Abs          ; CODE_DISPLACEMENT_ABS ($88)
  ; CODE_DISPLACEMENT ($80)
  jr .Op_N                  ; The same as for CODE_N?
.Op_IXD:
  ld a, CODE_IX
  call ChangeIXToIY
  ld hl, VALUE_1
  ld b, (hl)
  jr .IndirectRegs
.Op_N:
  ld a, '&'
  rst $20                   ; Print '&' character
  ld a, (VALUE_1)
  call HexA
  ret
.Op_N_Indirect:
  ld a, '('
  rst $20
  ld a, '&'
  rst $20                   ; Print '&' character
  ld a, (VALUE_1)
  call HexA
  ld a, ')'
  rst $20
  ret
.Op_D_Abs:
  ld a, (VALUE_1)
  ld hl, (DISASS_ADDR)
  inc hl                    ; Because JR/DJNZ offset is relative to the next address after the instruction. DISASS_ADDR is current byte pointer.
  ld e, a                   ; Next instructions will sign extend 8-bit value to 16-bit
  add a, a                  ; Sign bit of A into carry
  sbc a, a                  ; A = 0 if carry == 0, $FF otherwise
  ld d, a                   ; DE is sign extended A
  add hl, de
  call PrintHexWord
  ret
.Numbers:
  cp CODE_IX_D2
  jr z, .Op_IXD             ; Same print as for CODE_IX_D
  cp CODE_VALUE_8_HEX
  jr c, .PrintDecimal
  cp CODE_VALUE_0_HEX
  jr nz, .Skip_0_Hex
  sub CODE_VALUE_0_HEX - $40 ; -$40 because it will be subtracted by next instruction
.Skip_0_Hex:
  sub $40
  ld b, a
  ld a, '&'
  rst $20                   ; Print '&' character
  ld a, b
  call HexA
  ret
.PrintDecimal:              ; Print decimal digits from 0 to 7
  sub $10                   ; Subtract $10 from CODE_VALUE_x to get ASCII digit (ASCII '0' has code $30)
  rst $20
  ret
.IndirectRegsByCode:
  sub $1F                   ; Subtract $1F from $2x codes to get register table index
.IndirectRegs:              ; B = displacement, 0 means no displacement; A = table index
  ld c, a
  ld a, '('
  rst $20
  ld a, c
  call PrintFromTabel2
  ; Print displacement
  ld a, b
  or a
  jr z, .Close              ; Jump if there is no displacement
  jp p, .Positive           ; Jump if A has positive value
  neg
.Positive:
  ld c, a
  ld a, '+'                 ; '+' character
  jr nc, .Plus              ; Jump if printing positive number
  ld a, '-'                 ; '-' character
.Plus:
  rst $20
  ld a, '&'
  rst $20                   ; Print '&' character
  ld a, c
  call HexA
.Close:
  ld a, ')'
  rst $20
  ret

ChangeIXToIY:               ; Change IX to IY if prefix is FD, C = table index
  ld c, a
  ld a, (PREFIX)
  cp $FD
  ld a, c
  jr nz, .No_IY             ; No need to change IX to IY
  inc a                     ; IY table index is next after IX
.No_IY:
  ret

SIMPLE_TABLE: ; Simple instructions (OPCODE, TABEL1 index)
  BYTE $00, $00 ; NOP
  BYTE $2F, $01 ; CPL
  BYTE $3F, $02 ; CCF
  BYTE $37, $03 ; SCF
  BYTE $76, $04 ; HALT
  BYTE $F3, $05 ; DI
  BYTE $FB, $06 ; EI
  BYTE $D9, $07 ; EXX
  BYTE $07, $08 ; RLCA
  BYTE $17, $09 ; RLA
  BYTE $0F, $0A ; RRCA
  BYTE $1F, $0B ; RRA
  BYTE $27, $0C ; DAA
  BYTE $00      ; Table terminator value

BLOCK_TABLE: ; (OPCODE start, TABEL1 index, CODE_LEFT, CODE_RIGHT) Register index order: B C D E H L (HL) A
  BYTE $40, $36, CODE_B, CODE_REG ; LD B,r - r value is derived from the opcode
  BYTE $48, $36, CODE_C, CODE_REG ; LD C,r
  BYTE $50, $36, CODE_D, CODE_REG ; LD D,r
  BYTE $58, $36, CODE_E, CODE_REG ; LD E,r
  BYTE $60, $36, CODE_H, CODE_REG ; LD H,r
  BYTE $68, $36, CODE_L, CODE_REG ; LD L,r
  BYTE $70, $36, CODE_HL_INDIRECT, CODE_REG ; LD (HL),r
  BYTE $78, $36, CODE_A, CODE_REG ; LD A,r
  BYTE $80, $2C, CODE_A, CODE_REG ; ADD A,r
  BYTE $88, $2D, CODE_A, CODE_REG ; ADC A,r
  BYTE $90, $2F, CODE_REG, $0 ; SUB r
  BYTE $98, $2E, CODE_A, CODE_REG ; SBC A,r
  BYTE $A0, $30, CODE_REG, $0 ; AND r
  BYTE $A8, $32, CODE_REG, $0 ; XOR r
  BYTE $B0, $31, CODE_REG, $0 ; OR r
  BYTE $B8, $33, CODE_REG, $0 ; CP r
  BYTE $00

GENERAL_TABLE: ; (OPCODE, TABEL1 index, CODE_LEFT, CODE_RIGHT), instructions with one byte opcode, opcodes $00-$3F
  BYTE $01, $36, CODE_BC, CODE_NN ; LD BC,nn
  BYTE $02, $36, CODE_BC_INDIRECT, CODE_A ; LD (BC),A
  BYTE $06, $36, CODE_B, CODE_N ; LD B,n
  BYTE $0A, $36, CODE_A, CODE_BC_INDIRECT ; LD A,(BC)
  BYTE $0E, $36, CODE_C, CODE_N ; LD C,n
  BYTE $11, $36, CODE_DE, CODE_NN ; LD DE,nn
  BYTE $12, $36, CODE_DE_INDIRECT, CODE_A ; LD (DE),A
  BYTE $16, $36, CODE_D, CODE_N ; LD D,n
  BYTE $1A, $36, CODE_A, CODE_DE_INDIRECT ; LD A,(DE)
  BYTE $1E, $36, CODE_E, CODE_N ; LD E,n
  BYTE $21, $36, CODE_HL, CODE_NN ; LD HL,nn
  BYTE $22, $36, CODE_NN_INDIRECT, CODE_HL ; LD (nn),HL
  BYTE $26, $36, CODE_H, CODE_N ; LD H,n
  BYTE $2A, $36, CODE_HL, CODE_NN_INDIRECT ; LD HL,(nn)
  BYTE $2E, $36, CODE_L, CODE_N ; LD L,n
  BYTE $31, $36, CODE_SP, CODE_NN ; LD SP,nn
  BYTE $32, $36, CODE_NN_INDIRECT, CODE_A ; LD (nn),A
  BYTE $36, $36, CODE_HL_INDIRECT, CODE_N ; LD (HL),n
  BYTE $3A, $36, CODE_A, CODE_NN_INDIRECT ; LD A,(nn)
  BYTE $3E, $36, CODE_A, CODE_N ; LD A,n

  BYTE $03, $34, CODE_BC, $0 ; INC BC
  BYTE $04, $34, CODE_B, $0 ; INC B
  BYTE $0C, $34, CODE_C, $0 ; INC C
  BYTE $13, $34, CODE_DE, $0 ; INC DE
  BYTE $14, $34, CODE_D, $0 ; INC D
  BYTE $1C, $34, CODE_E, $0 ; INC E
  BYTE $23, $34, CODE_HL, $0 ; INC HL
  BYTE $24, $34, CODE_H, $0 ; INC H
  BYTE $2C, $34, CODE_L, $0 ; INC L
  BYTE $33, $34, CODE_SP, $0 ; INC SP
  BYTE $34, $34, CODE_HL_INDIRECT, $0 ; INC (HL)
  BYTE $3C, $34, CODE_A, $0 ; INC A

  BYTE $05, $35, CODE_B, $0 ; DEC B
  BYTE $0B, $35, CODE_BC, $0 ; DEC BC
  BYTE $0D, $35, CODE_C, $0 ; DEC C
  BYTE $15, $35, CODE_D, $0 ; DEC D
  BYTE $1B, $35, CODE_DE, $0 ; DEC DE
  BYTE $1D, $35, CODE_E, $0 ; DEC E
  BYTE $25, $35, CODE_H, $0 ; DEC H
  BYTE $2B, $35, CODE_HL, $0 ; DEC HL
  BYTE $2D, $35, CODE_L, $0 ; DEC L
  BYTE $35, $35, CODE_HL_INDIRECT, $0 ; DEC (HL)
  BYTE $3B, $35, CODE_SP, $0 ; DEC SP
  BYTE $3D, $35, CODE_A, $0 ; DEC A

  BYTE $18, $3B, CODE_DISPLACEMENT_ABS, $0 ; JR d
  BYTE $20, $3B, CODE_F_NZ, CODE_DISPLACEMENT_ABS ; JR nz,d
  BYTE $28, $3B, CODE_F_Z, CODE_DISPLACEMENT_ABS ; JR z,d
  BYTE $30, $3B, CODE_F_NC, CODE_DISPLACEMENT_ABS ; JR nc,d
  BYTE $38, $3B, CODE_F_C, CODE_DISPLACEMENT_ABS ; JR c,d

  BYTE $09, $2C, CODE_HL, CODE_BC ; ADD HL,BC
  BYTE $19, $2C, CODE_HL, CODE_DE ; ADD HL,DE
  BYTE $29, $2C, CODE_HL, CODE_HL ; ADD HL,HL
  BYTE $39, $2C, CODE_HL, CODE_SP ; ADD HL,SP

  BYTE $10, $3A, CODE_DISPLACEMENT_ABS, $0 ; DJNZ d
  BYTE $08, $37, CODE_AF, CODE_AF_PRIM ; EX AF,AF'
  BYTE $00

GENERAL2_TABLE: ; Second half of the table, opcodes $C0-$FF, split into second table to improve search speed
  BYTE $C0, $3E, CODE_F_NZ, $0 ; RET nz
  BYTE $C8, $3E, CODE_F_Z, $0 ; RET z
  BYTE $C9, $3E, $0, $0 ; RET
  BYTE $D0, $3E, CODE_F_NC, $0 ; RET nc
  BYTE $D8, $3E, CODE_F_C, $0 ; RET c
  BYTE $E0, $3E, CODE_F_PO, $0 ; RET po
  BYTE $E8, $3E, CODE_F_PE, $0 ; RET pe
  BYTE $F0, $3E, CODE_F_P, $0 ; RET p
  BYTE $F8, $3E, CODE_F_M, $0 ; RET m

  BYTE $C2, $3C, CODE_F_NZ, CODE_NN ; JP nz,nn
  BYTE $C3, $3C, CODE_NN, $0 ; JP nn - Saved with low-high byte order (addr -> 'dr ad')
  BYTE $CA, $3C, CODE_F_Z, CODE_NN ; JP z,nn
  BYTE $D2, $3C, CODE_F_NC, CODE_NN ; JP nc,nn
  BYTE $DA, $3C, CODE_F_C, CODE_NN ; JP c,nn
  BYTE $E2, $3C, CODE_F_PO, CODE_NN ; JP po,nn
  BYTE $E9, $3C, CODE_HL_INDIRECT, $0 ; JP (HL)
  BYTE $EA, $3C, CODE_F_PE, CODE_NN ; JP pe,nn
  BYTE $F2, $3C, CODE_F_P, CODE_NN ; JP p,nn
  BYTE $FA, $3C, CODE_F_M, CODE_NN ; JP m,nn

  BYTE $C4, $3D, CODE_F_NZ, CODE_NN ; CALL nz,nn
  BYTE $CC, $3D, CODE_F_Z, CODE_NN ; CALL z,nn
  BYTE $CD, $3D, CODE_NN, $0 ; CALL nn
  BYTE $D4, $3D, CODE_F_NC, CODE_NN ; CALL nc,nn
  BYTE $DC, $3D, CODE_F_C, CODE_NN ; CALL c,nn
  BYTE $E4, $3D, CODE_F_PO, CODE_NN ; CALL po,nn
  BYTE $EC, $3D, CODE_F_PE, CODE_NN ; CALL pe,nn
  BYTE $F4, $3D, CODE_F_P, CODE_NN ; CALL p,nn
  BYTE $FC, $3D, CODE_F_M, CODE_NN ; CALL m,nn

  BYTE $C1, $41, CODE_BC, $0 ; POP BC
  BYTE $D1, $41, CODE_DE, $0 ; POP DE
  BYTE $E1, $41, CODE_HL, $0 ; POP HL
  BYTE $F1, $41, CODE_AF, $0 ; POP AF

  BYTE $C5, $42, CODE_BC, $0 ; PUSH BC
  BYTE $D5, $42, CODE_DE, $0 ; PUSH DE
  BYTE $E5, $42, CODE_HL, $0 ; PUSH HL
  BYTE $F5, $42, CODE_AF, $0 ; PUSH AF

  BYTE $C7, $3F, CODE_VALUE_0_HEX, $0 ; RST $00
  BYTE $CF, $3F, CODE_VALUE_8_HEX, $0 ; RST $08
  BYTE $D7, $3F, CODE_VALUE_16_HEX, $0 ; RST $10
  BYTE $DF, $3F, CODE_VALUE_24_HEX, $0 ; RST $18
  BYTE $E7, $3F, CODE_VALUE_32_HEX, $0 ; RST $20
  BYTE $EF, $3F, CODE_VALUE_40_HEX, $0 ; RST $28
  BYTE $F7, $3F, CODE_VALUE_48_HEX, $0 ; RST $30
  BYTE $FF, $3F, CODE_VALUE_56_HEX, $0 ; RST $38

  BYTE $C6, $2C, CODE_A, CODE_N ; ADD A,n
  BYTE $CE, $2D, CODE_A, CODE_N ; ADC A,n
  BYTE $D6, $2F, CODE_N, $0 ; SUB n
  BYTE $DE, $2E, CODE_A, CODE_N ; SBC A,n

  BYTE $E6, $30, CODE_N, $0 ; AND n
  BYTE $EE, $32, CODE_N, $0 ; XOR n
  BYTE $F6, $31, CODE_N, $0 ; OR n
  BYTE $FE, $33, CODE_N, $0 ; CP n

  BYTE $D3, $39, CODE_N_INDIRECT, CODE_A ; OUT (n),A
  BYTE $DB, $38, CODE_A, CODE_N_INDIRECT ; IN A,(N)
  BYTE $E3, $37, CODE_SP_INDIRECT, CODE_HL ; EX (SP),HL
  BYTE $EB, $37, CODE_DE, CODE_HL ; EX DE,HL
  BYTE $00

ED_TABLE:
  BYTE $40, $38, CODE_B, CODE_C_INDIRECT ; IN B,(C)
  BYTE $41, $39, CODE_C_INDIRECT, CODE_B ; OUT (C),B
  BYTE $42, $2E, CODE_HL, CODE_BC ; SBC HL,BC
  BYTE $43, $36, CODE_NN_INDIRECT, CODE_BC ; LD (nn),BC
  BYTE $44, $1D, $0, $0 ; NEG
  BYTE $45, $1F, $0, $0 ; RETN
  BYTE $46, $40, CODE_VALUE_0, $0 ; IM 0
  BYTE $47, $36, CODE_I, CODE_A ; LD I,A
  BYTE $48, $38, CODE_C, CODE_C_INDIRECT ; IN C,(C)
  BYTE $49, $39, CODE_C_INDIRECT, CODE_C ; OUT (C),C
  BYTE $4A, $2D, CODE_HL, CODE_BC ; ADC HL,BC
  BYTE $4B, $36, CODE_BC, CODE_NN_INDIRECT ; LD BC,(nn)
  BYTE $4D, $1E, $0, $0 ; RETI
  BYTE $4F, $36, CODE_R, CODE_A ; LD R,A
  BYTE $50, $38, CODE_D, CODE_C_INDIRECT ; IN D,(C)
  BYTE $51, $39, CODE_C_INDIRECT, CODE_D ; OUT (C),D
  BYTE $52, $2E, CODE_HL, CODE_DE ; SBC HL,DE
  BYTE $53, $36, CODE_NN_INDIRECT, CODE_DE ; LD (nn),DE
  BYTE $56, $40, CODE_VALUE_1, $0 ; IM 1
  BYTE $57, $36, CODE_A, CODE_I ; LD A,I
  BYTE $58, $38, CODE_E, CODE_C_INDIRECT ; IN E,(C)
  BYTE $59, $39, CODE_C_INDIRECT, CODE_E ; OUT (C),E
  BYTE $5A, $2D, CODE_HL, CODE_DE ; ADC HL,DE
  BYTE $5B, $36, CODE_DE, CODE_NN_INDIRECT ; LD DE,(nn)
  BYTE $5E, $40, CODE_VALUE_2, $0 ; IM 2
  BYTE $5F, $36, CODE_A, CODE_R ; LD A,R
  BYTE $60, $38, CODE_H, CODE_C_INDIRECT ; IN H,(C)
  BYTE $61, $39, CODE_C_INDIRECT, CODE_H ; OUT (C),H
  BYTE $62, $2E, CODE_HL, CODE_HL; SBC HL,HL
  BYTE $67, $21, $0, $0 ; RRD
  BYTE $68, $38, CODE_L, CODE_C_INDIRECT ; IN L,(C)
  BYTE $69, $39, CODE_C_INDIRECT, CODE_L ; OUT (C),L
  BYTE $6A, $2D, CODE_HL, CODE_HL ; ADC HL,HL
  BYTE $6F, $20, $0, $0 ; RLD
  BYTE $72, $2E, CODE_HL, CODE_SP ; SBC HL,SP
  BYTE $73, $36, CODE_NN_INDIRECT, CODE_SP ; LD (nn),SP
  BYTE $78, $38, CODE_A, CODE_C_INDIRECT ; IN A,(C)
  BYTE $79, $39, CODE_C_INDIRECT, CODE_A ; OUT (C),A
  BYTE $7A, $2D, CODE_HL, CODE_SP ; ADC HL,SP
  BYTE $7B, $36, CODE_SP, CODE_NN_INDIRECT ; LD SP,(nn)
  BYTE $00

ED2_TABLE:
  BYTE $A0, $0E ; LDI
  BYTE $A1, $12 ; CPI
  BYTE $A2, $16 ; INI
  BYTE $A3, $1A ; OUTI
  BYTE $A8, $10 ; LDD
  BYTE $A9, $14 ; CPD
  BYTE $AA, $18 ; IND
  BYTE $AB, $1C ; OUTD
  BYTE $B0, $0D ; LDIR
  BYTE $B1, $11 ; CPIR
  BYTE $B2, $15 ; INIR
  BYTE $B3, $19 ; OTIR
  BYTE $B8, $0F ; LDDR
  BYTE $B9, $13 ; CPDR
  BYTE $BA, $17 ; INDR
  BYTE $BB, $1B ; OTDR
  BYTE $00

DD_FD_TABLE: ; DD for IX, FD for IY
  BYTE $86, $2C, CODE_A, CODE_IX_D ; ADD A,(IX+d)
  BYTE $09, $2C, CODE_IX, CODE_BC ; ADD IX,BC
  BYTE $19, $2C, CODE_IX, CODE_DE ; ADD IX,DE
  BYTE $29, $2C, CODE_IX, CODE_IX ; ADD IX,IX
  BYTE $39, $2C, CODE_IX, CODE_SP ; ADD IX,SP
  BYTE $A6, $30, CODE_IX_D, $0 ; AND (IX+d)
  BYTE $77, $36, CODE_IX_D, CODE_A ; LD (IX+d),A
  BYTE $70, $36, CODE_IX_D, CODE_B ; LD (IX+d),B
  BYTE $71, $36, CODE_IX_D, CODE_C ; LD (IX+d),C
  BYTE $72, $36, CODE_IX_D, CODE_D ; LD (IX+d),D
  BYTE $73, $36, CODE_IX_D, CODE_E ; LD (IX+d),E
  BYTE $74, $36, CODE_IX_D, CODE_H ; LD (IX+d),H
  BYTE $75, $36, CODE_IX_D, CODE_L ; LD (IX+d),L
  BYTE $36, $36, CODE_IX_D, CODE_N ; LD (IX+d),n
  BYTE $7E, $36, CODE_A, CODE_IX_D ; LD A,(IX+d)
  BYTE $46, $36, CODE_B, CODE_IX_D ; LD B,(IX+d)
  BYTE $4E, $36, CODE_C, CODE_IX_D ; LD C,(IX+d)
  BYTE $56, $36, CODE_D, CODE_IX_D ; LD D,(IX+d)
  BYTE $5E, $36, CODE_E, CODE_IX_D ; LD E,(IX+d)
  BYTE $66, $36, CODE_H, CODE_IX_D ; LD H,(IX+d)
  BYTE $6E, $36, CODE_L, CODE_IX_D ; LD L,(IX+d)
  BYTE $2A, $36, CODE_IX, CODE_NN_INDIRECT; LD IX,(nn)
  BYTE $21, $36, CODE_IX, CODE_NN; LD IX,nn
  BYTE $9E, $2E, CODE_A, CODE_IX_D; SBC A,(IX+d)
  BYTE $BE, $33, CODE_IX_D, $0 ; CP (IX+d)
  BYTE $35, $35, CODE_IX_D, $0 ; DEC (IX+d)
  BYTE $2B, $35, CODE_IX, $0 ; DEC IX
  BYTE $E3, $37, CODE_SP_INDIRECT, CODE_IX ; EX (SP),IX
  BYTE $34, $34, CODE_IX_D, $0 ; INC (IX+d)
  BYTE $23, $34, CODE_IX, $0 ; INC IX
  BYTE $E9, $3C, CODE_IX_INDIRECT, $0 ; JP (IX)
  BYTE $F9, $36, CODE_SP, CODE_IX ; LD SP,IX
  BYTE $B6, $31, CODE_IX_D, $0 ; OR (IX+d)
  BYTE $E1, $41, CODE_IX, $0 ; POP IX
  BYTE $E5, $42, CODE_IX, $0 ; PUSH IX
  BYTE $96, $2F, CODE_IX_D, $0 ; SUB (IX+d)
  BYTE $AE, $32, CODE_IX_D, $0 ; XOR (IX+d)
  BYTE $00

CB_BLOCK_TABLE:
  BYTE $00, $22, CODE_REG, $0 ; RLC r
  BYTE $08, $24, CODE_REG, $0 ; RRC r
  BYTE $10, $23, CODE_REG, $0 ; RL r
  BYTE $18, $25, CODE_REG, $0 ; RR r
  BYTE $20, $26, CODE_REG, $0 ; SLA r
  BYTE $28, $27, CODE_REG, $0 ; SRA r
  BYTE $38, $28, CODE_REG, $0 ; SRL r
  BYTE $40, $29, CODE_VALUE_0, CODE_REG ; BIT 0,r
  BYTE $48, $29, CODE_VALUE_1, CODE_REG ; BIT 1,r
  BYTE $50, $29, CODE_VALUE_2, CODE_REG ; BIT 2,r
  BYTE $58, $29, CODE_VALUE_3, CODE_REG ; BIT 3,r
  BYTE $60, $29, CODE_VALUE_4, CODE_REG ; BIT 4,r
  BYTE $68, $29, CODE_VALUE_5, CODE_REG ; BIT 5,r
  BYTE $70, $29, CODE_VALUE_6, CODE_REG ; BIT 6,r
  BYTE $78, $29, CODE_VALUE_7, CODE_REG ; BIT 7,r
  BYTE $80, $2B, CODE_VALUE_0, CODE_REG ; RES 0,r
  BYTE $88, $2B, CODE_VALUE_1, CODE_REG ; RES 1,r
  BYTE $90, $2B, CODE_VALUE_2, CODE_REG ; RES 2,r
  BYTE $98, $2B, CODE_VALUE_3, CODE_REG ; RES 3,r
  BYTE $A0, $2B, CODE_VALUE_4, CODE_REG ; RES 4,r
  BYTE $A8, $2B, CODE_VALUE_5, CODE_REG ; RES 5,r
  BYTE $B0, $2B, CODE_VALUE_6, CODE_REG ; RES 6,r
  BYTE $B8, $2B, CODE_VALUE_7, CODE_REG ; RES 7,r
  BYTE $C0, $2A, CODE_VALUE_0, CODE_REG ; SET 0,r
  BYTE $C8, $2A, CODE_VALUE_1, CODE_REG ; SET 1,r
  BYTE $D0, $2A, CODE_VALUE_2, CODE_REG ; SET 2,r
  BYTE $D8, $2A, CODE_VALUE_3, CODE_REG ; SET 3,r
  BYTE $E0, $2A, CODE_VALUE_4, CODE_REG ; SET 4,r
  BYTE $E8, $2A, CODE_VALUE_5, CODE_REG ; SET 5,r
  BYTE $F0, $2A, CODE_VALUE_6, CODE_REG ; SET 6,r
  BYTE $F8, $2A, CODE_VALUE_7, CODE_REG ; SET 7,r
  BYTE $00

DDFD_CB_TABLE: ; DD CB fo IX, FD CB for IY
  BYTE $46, $29, CODE_VALUE_0, CODE_IX_D2 ; BIT 0,(IX+d)
  BYTE $4E, $29, CODE_VALUE_1, CODE_IX_D2 ; BIT 1,(IX+d)
  BYTE $56, $29, CODE_VALUE_2, CODE_IX_D2 ; BIT 2,(IX+d)
  BYTE $5E, $29, CODE_VALUE_3, CODE_IX_D2 ; BIT 3,(IX+d)
  BYTE $66, $29, CODE_VALUE_4, CODE_IX_D2 ; BIT 4,(IX+d)
  BYTE $6E, $29, CODE_VALUE_5, CODE_IX_D2 ; BIT 5,(IX+d)
  BYTE $76, $29, CODE_VALUE_6, CODE_IX_D2 ; BIT 6,(IX+d)
  BYTE $7E, $29, CODE_VALUE_7, CODE_IX_D2 ; BIT 7,(IX+d)
  BYTE $06, $22, CODE_IX_D2, $0 ; RLC (IX+d)
  BYTE $16, $23, CODE_IX_D2, $0 ; RL (IX+d)
  BYTE $0E, $24, CODE_IX_D2, $0 ; RRC (IX+d)
  BYTE $1E, $25, CODE_IX_D2, $0 ; RR (IX+d)
  BYTE $26, $26, CODE_IX_D2, $0 ; SLA (IX+d)
  BYTE $2E, $27, CODE_IX_D2, $0 ; SRA (IX+d)
  BYTE $3E, $28, CODE_IX_D2, $0 ; SRL (IX+d)
  BYTE $86, $2B, CODE_VALUE_0, CODE_IX_D2 ; RES 0,(IX+d) Byte order: DD CB d 86
  BYTE $8E, $2B, CODE_VALUE_1, CODE_IX_D2 ; RES 1,(IX+d)
  BYTE $96, $2B, CODE_VALUE_2, CODE_IX_D2 ; RES 2,(IX+d)
  BYTE $9E, $2B, CODE_VALUE_3, CODE_IX_D2 ; RES 3,(IX+d)
  BYTE $A6, $2B, CODE_VALUE_4, CODE_IX_D2 ; RES 4,(IX+d)
  BYTE $AE, $2B, CODE_VALUE_5, CODE_IX_D2 ; RES 5,(IX+d)
  BYTE $B6, $2B, CODE_VALUE_6, CODE_IX_D2 ; RES 6,(IX+d)
  BYTE $BE, $2B, CODE_VALUE_7, CODE_IX_D2 ; RES 7,(IX+d)
  BYTE $C6, $2A, CODE_VALUE_0, CODE_IX_D2 ; SET 0,(IX+d)
  BYTE $CE, $2A, CODE_VALUE_1, CODE_IX_D2 ; SET 1,(IX+d)
  BYTE $D6, $2A, CODE_VALUE_2, CODE_IX_D2 ; SET 2,(IX+d)
  BYTE $DE, $2A, CODE_VALUE_3, CODE_IX_D2 ; SET 3,(IX+d)
  BYTE $E6, $2A, CODE_VALUE_4, CODE_IX_D2 ; SET 4,(IX+d)
  BYTE $EE, $2A, CODE_VALUE_5, CODE_IX_D2 ; SET 5,(IX+d)
  BYTE $F6, $2A, CODE_VALUE_6, CODE_IX_D2 ; SET 6,(IX+d)
  BYTE $FE, $2A, CODE_VALUE_7, CODE_IX_D2 ; SET 7,(IX+d)
  BYTE $00

vars: ; Variables are located at the end of program
;vars = $6000 ; Variables are located at fixed starting address set by this line
PREFIX       = vars + v._PREFIX
TABEL1_INDEX = vars + v._TABEL1_INDEX
OPCODE       = vars + v._OPCODE
CODE_LEFT    = vars + v._CODE_LEFT
CODE_RIGHT   = vars + v._CODE_RIGHT
VALUE_1      = vars + v._VALUE_1
VALUE_2      = vars + v._VALUE_2
DISASS_ADDR  = vars + v._DISASS_ADDR
DISASM_CNT   = vars + v._DISASM_CNT
INST_LEN     = vars + v._INST_LEN
BRK_ADDR     = vars + v._BRK_ADDR
SEARCH_START = vars + v._SEARCH_START
SEARCH_END   = vars + v._SEARCH_END
AF_ALT       = vars + v._AF_ALT
BC_ALT       = vars + v._BC_ALT
DE_ALT       = vars + v._DE_ALT
HL_ALT       = vars + v._HL_ALT
AF_REG       = vars + v._AF_REG
BC_REG       = vars + v._BC_REG
DE_REG       = vars + v._DE_REG
HL_REG       = vars + v._HL_REG
IX_REG       = vars + v._IX_REG
IY_REG       = vars + v._IY_REG
SP_REG       = vars + v._SP_REG
PC_REG       = vars + v._PC_REG
SP_SYS       = vars + v._SP_SYS
DUMP_ADDR    = vars + v._DUMP_ADDR
KEY_CODE     = vars + v._KEY_CODE
TRACE_INSTR  = vars + v._TRACE_INSTR
TRACE_BRK    = vars + v._TRACE_BRK
BREAKPOINTS  = vars + v._BREAKPOINTS
CURSOR_SAVE  = vars + v._CURSOR_SAVE
SCREEN_SAVE  = vars + v._SCREEN_SAVE
IN_BUFFER    = vars + v._IN_BUFFER
STACK_TOP    = vars + v ; Stack top is at the end of 'v' structure, _DBG_STACK reserves 64 bytes long space

VIDEO_OFFSET  = SCREEN_SAVE - VIDEORAM

; Table values for CODE_LEFT and CODE_RIGHT
; Registers, same order as in TABEL1
CODE_IX = 1
;CODE_IY = 2 ; Not used
CODE_AF = 3
CODE_BC = 4
CODE_DE = 5
CODE_HL = 6
CODE_SP = 7
CODE_B = 8
CODE_C = 9
CODE_D = 10
CODE_E = 11
CODE_H = 12
CODE_L = 13
CODE_A = 14
CODE_I = 15
CODE_R = 16
; Flags
CODE_F_NZ = 17
CODE_F_Z = 18
CODE_F_NC = 19
CODE_F_C = 20
CODE_F_PO = 21
CODE_F_PE = 22
CODE_F_P = 23
CODE_F_M = 24
; Indirect registers
CODE_IX_INDIRECT = $20
;CODE_IY_INDIRECT = $21 ; Not used
CODE_BC_INDIRECT = $23 ; Once code value skiped for AF in table
CODE_DE_INDIRECT = $24
CODE_HL_INDIRECT = $25 ; (HL), $20 = 32 decimal, has bit 5 set
CODE_SP_INDIRECT = $26
CODE_C_INDIRECT = $28

CODE_REG = $30 ; Bits 5 and 4 are set
CODE_IX_D2 = $31 ; This is different then CODE_IX_D because displacement is not the last instruction byte. It is read at other place then for CODE_IX_D.
CODE_AF_PRIM = $32 ; This is not implemented - there is no ''' character in Galaksija and AF' is not possible to display!
; Unsupported 8-bit registers high and low IX/IY
; CODE_HIX = $32
; CODE_HIY = $33
; CODE_LIX = $34
; CODE_LIY = $35

CODE_VALUE_0 = $40
CODE_VALUE_1 = $41
CODE_VALUE_2 = $42
CODE_VALUE_3 = $43
CODE_VALUE_4 = $44
CODE_VALUE_5 = $45
CODE_VALUE_6 = $46
CODE_VALUE_7 = $47
CODE_VALUE_8_HEX = $48
CODE_VALUE_16_HEX = $50
CODE_VALUE_24_HEX = $58
CODE_VALUE_32_HEX = $60
CODE_VALUE_40_HEX = $68
CODE_VALUE_48_HEX = $70
CODE_VALUE_56_HEX = $78
CODE_VALUE_0_HEX = $7A ; Print 0 as HEX value &00

; d = displacement byte (8-bit signed integer)
; n = 8-bit immediate operand (unsigned integer)
; nn = 16-bit immediate operand (unsigned integer)

; One byte digit values (7th bit set)
CODE_DISPLACEMENT = $80 ; -126 to +129. This is not used now!
; Index registers with displacement
CODE_IX_D = $81 ; (IX + d)
CODE_N = $82
CODE_N_INDIRECT = $84
CODE_DISPLACEMENT_ABS = $88 ; Displacement as absolute address
; (7th and 6th bits set)
CODE_NN = $C3
CODE_NN_INDIRECT = $C4

  .end