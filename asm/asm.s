;
; Mini-Assembler
;
; Assembles single instruction at the program cursor into machine code.
; Following the syntax documented in the Apple II Reference manual
;
; xa assembler seems to have some obscure problems with block markers, so everything
; is on top level. I tried with block proc and (, but tht produced sntax errors far, far away
; Solution would be to use a different assembler. but they all have their quirks.
;
nextch:         iny
                lda in,y
                rts
                ;
                ; consume blanks and $ signs,
                ; since all numbers are hex
                ; returns first non ignored char in a
                ;
eatblp:         iny
eatblank:       lda in,y
                cmp #' '
                beq eatblp
                cmp #'$'
                beq eatblp
                rts
                ; get code for menmonic
                ; input: a has first letter.
                ; return: mnem index in x
syntaxerr:      jsr bell
                lda #' '
@blnks:         jsr cout
                dey
                bpl @blnks
                lda #'^'
                jsr cout
                ldx spnt    ; restore entry point stack position
                txs
                jmp asmz

setaddr:        jsr zmode           ; clear (reused) monitor mode, scan idx
                jsr getnum
                dey                 ; recover non-digit item
                rts
hasaddr:        ldy #$4             ; look at first 5 chars, beginning with last
@hadrlp:        lda in,y
                cmp #':'
                beq setaddr         ; if so, set current address
                dey
                bpl @hadrlp
                ldy #$00            ; no address found, reset 'in' pointer
                rts
                ;
                ; Mini-Assembler entry point.
                ;
asm_entry:      cld ; who knows where we came from... bcd mode tends to confuse hex based arithmetics.
                ;
                ; save stack pointer to spnt to return to clean state
                ; when processing syntax errors
                ; from deeper subroutine level.
                ; very crude exception handling.
                ; This makes it possible to handle syntax errors in subroutines
                ; without too much overhead - process is aborted and asm returns to prompt.
                ;
                tsx
                stx spnt
                lda a1l
                sta pcl
                lda a1h
                sta pch
                ;
                ; main assembler loop
                ;
asmz:           lda #'!'
                sta prompt          ; set prompt to '!'
                jsr crout
                ldy a1h             ; print CR,A1 in hex
                ldx a1l
                jsr prntyx          ; output cr and address
                jsr getlnv
                jsr zmode
                jsr hasaddr         ; see if line has address label "%x:"
                ldx #$01
@setpcl:        lda a1l,x
                sta pcl,x
                dex
                bpl @setpcl

                jsr eatblank        ; skip whitespace
                cmp #k_entr         ; empty line, return to prompt
                beq asmz

                ;
                ; on '*' return to monitor.
                ;
                cmp #'*'
                bne @procmnm
                jmp monz
                ;
                ; get the mnemonic from input line
                ;
                ;
                ; get coded mnemonic into a4
                ; ((menm0 & 0x1f) << 10) + ((menm1 & 0x1f) << 5) + (menm2 & 0x1f)
                ; so the opcode forms a word with 13 bits used.
                ; impossible mnemonics produce a syntax error, invalid mnemonics will produce an
                ; unknown opcode
                ;
@procmnm:       and #$1F    ; ex.:  PHP                                            'P' = $50 -> $10
                sta a4h     ;
                lda #$00
                sta in      ; put a zero into first position to mark input buffer old news.
                jsr nextch  ; $1F useful bits, have to make room for 5 more        `H' = $48, useful $08
                asl         ; $3E                                                        $90
                asl         ; $7C                                                        $20
                asl         ; $F8                                                        $40
                asl         ; $01F0 high bit-> carry                                     $80 + 0
                rol a4h     ; $3FF0                                               $20    $80
                asl         ; $03E0 high bit-> carry                                     $00 + 1
                rol a4h     ; $7FE0                                               $41    $00
                sta a4l     ; $E0 , five lower bits free                                 $00
                jsr nextch  ; $1F                                                  'P' = $50
                and #$1F    ;                                                            $10
                ora a4l     ;                                                            $10
                sta a4l     ;                                                     $41    $10
                ;
                ; now we have the short form in a4.
                ; go on to find the mnemonic in opcodex table
                ; and tehn return this index to the opcode lookup tables
                ldx #(opcodez-opcodex-1)+2
@firstfail:     dex
@scndfail:      dex
                bpl @cont
                jmp syntaxerr
@cont:          lda opcodex,x     high
                cmp a4h
                bne @firstfail
                dex
                lda opcodex,x     low
                cmp a4l
                bne @scndfail
                txa
                lsr         ; rest of the tables have bytes not words, so half it.
                sta mnem
                iny                 ; point to char after mnemonic
                ;
                ; at this point, we have the address set in a1-a3 and the
                ; mnemonic code number in a4l.
                ; Now proceed to  evaluate address part.
                ; Following modes are possible:
                ; - None. Command does not take arguments, it is finished.
                ; - Immediate:  #Value
                ; - Adr/Rel:    Address
                ; - Indexed:    Address,X|Y
                ; - indirect:   (Address)
                ; - indir. indxd (Address),Y
                ; - indexed ind. (Address,X)
                jsr eatblank
                jmp eval_arg
;
; opcode char mappings
;
opcodex:
                .include "opcodes_compressed.s"
opcodez:
;
; opmode groups
;
;                                       |Impl|Imm|Abs |Zero|z_x |z_y |a_x |a_y |Rel |ix_ir|ir_ix|Ind
; type 0: implied/accu, no arguments    | *  |   |    |    |    |    |    |    |    |     |     |
; type 1: relative                      |    |   |(*) |    |    |    |    |    | <- |     |     |
; type 2: full in (like adc)            |    | * | *  | *  | *  |    | *  | *  |    | *   | *   |
; type 3: full out (without implied)    |    |   | *  | *  | *  |    | *  | *  |    | *   | *   |
; type 4: shifting                      | *  |   | *  | *  | *  |    | *  |    |    |     |     |
; type 5: cpx                           |    | * | *  | *  |    |    |    |    |    |     |     |
; type 6: bit                           |    |   | *  | *  |    |    |    |    |    |     |     |
; type 7: dec                           |    |   | *  | *  | *  |    | *  |    |    |     |     |
; type 8: jmp                           |    |   | *  |    |    |    |    |    |    |     |     | *
; type 9: jsr                           |    |   | *  |    |    |    |    |    |    |     |     |
; type a: ldx/ldy changing index reg    |    | * |    | *  | y  | x  | y  | x  |    |     |     |
; type b: stx/sty changing index reg    |    |   | *  | *  | y  | x  |    |    |    |     |     |
;
opmodes:
                .byte $02          ; 'ADC'    - 00
                .byte $02          ; 'AND'
                .byte $04          ; 'ASL'
                .byte $01          ; 'BCC'
                .byte $01          ; 'BCS'
                .byte $01          ; 'BEQ'
                .byte $06          ; 'BIT'
                .byte $01          ; 'BMI'
                .byte $01          ; 'BNE'    - 08
                .byte $01          ; 'BPL'
                .byte $00          ; 'BRK'
                .byte $01          ; 'BVC'
                .byte $01          ; 'BVS'
                .byte $00          ; 'CLC'
                .byte $00          ; 'CLD'
                .byte $00          ; 'CLI'
                .byte $00          ; 'CLV'    - 10
                .byte $02          ; 'CMP'
                .byte $05          ; 'CPX'
                .byte $05          ; 'CPY'
                .byte $07          ; 'DEC'
                .byte $00          ; 'DEX'
                .byte $00          ; 'DEY'
                .byte $02          ; 'EOR'
                .byte $07          ; 'INC'    - 18
                .byte $00          ; 'INX'
                .byte $00          ; 'INY'
                .byte $08          ; 'JMP'
                .byte $09          ; 'JSR'
                .byte $02          ; 'LDA'
                .byte $0a          ; 'LDX'
                .byte $0a          ; 'LDY'
                .byte $04          ; 'LSR'    - 20
                .byte $00          ; 'NOP'
                .byte $02          ; 'ORA'
                .byte $00          ; 'PHA'
                .byte $00          ; 'PHP'
                .byte $00          ; 'PLA'
                .byte $00          ; 'PLP'
                .byte $04          ; 'ROL'
                .byte $04          ; 'ROR'    - 28
                .byte $00          ; 'RTI'
                .byte $00          ; 'RTS'
                .byte $02          ; 'SBC'
                .byte $00          ; 'SEC'
                .byte $00          ; 'SED'
                .byte $00          ; 'SEI'
                .byte $03          ; 'STA'
                .byte $0b          ; 'STX'    - 30
                .byte $0b          ; 'STY'
                .byte $00          ; 'TAX'
                .byte $00          ; 'TAY'
                .byte $00          ; 'TSX'
                .byte $00          ; 'TXA'
                .byte $00          ; 'TXS'
                .byte $00          ; 'TYA'    - 37
opmodez:
                ;
                ;   This is the basic opcodes with bits %aaabbbcc
                ;   for instructions that only have implied mode
                ;   the full opcode is given with bbb bits
                ;   also set to speed up emission.
                ;   see https://www.masswerk.at/6502/6502_instruction_set.html#layout
                ;
code_ac:
                .byte $61          ; 'ADC'    - 00
                .byte $21          ; 'AND'
                .byte $02          ; 'ASL'
                .byte $90          ; 'BCC'
                .byte $B0          ; 'BCS'
                .byte $F0          ; 'BEQ'
                .byte $06          ; 'BIT'
                .byte $30          ; 'BMI'
                .byte $D0          ; 'BNE'    - 08
                .byte $10          ; 'BPL'
                .byte $00          ; 'BRK'
                .byte $50          ; 'BVC'
                .byte $70          ; 'BVS'
                .byte $18          ; 'CLC'
                .byte $D8          ; 'CLD'
                .byte $58          ; 'CLI'
                .byte $B8          ; 'CLV'    - 10
                .byte $61          ; 'CMP'
                .byte $E0          ; 'CPX'
                .byte $C0          ; 'CPY'
                .byte $C2          ; 'DEC'
                .byte $CA          ; 'DEX'
                .byte $88          ; 'DEY'
                .byte $41          ; 'EOR'
                .byte $E2          ; 'INC'    - 18
                .byte $E8          ; 'INX'
                .byte $C8          ; 'INY'
                .byte $4C          ; 'JMP'
                .byte $20          ; 'JSR'
                .byte $A1          ; 'LDA'
                .byte $A2          ; 'LDX'
                .byte $A0          ; 'LDY'
                .byte $42          ; 'LSR'    - 20
                .byte $EA          ; 'NOP'
                .byte $01          ; 'ORA'
                .byte $48          ; 'PHA'
                .byte $08          ; 'PHP'
                .byte $68          ; 'PLA'
                .byte $28          ; 'PLP'
                .byte $22          ; 'ROL'
                .byte $62          ; 'ROR'    - 28
                .byte $40          ; 'RTI'
                .byte $60          ; 'RTS'
                .byte $E1          ; 'SBC'
                .byte $38          ; 'SEC'
                .byte $F8          ; 'SED'
                .byte $78          ; 'SEI'
                .byte $81          ; 'STA'
                .byte $82          ; 'STX'    - 30
                .byte $80          ; 'STY'
                .byte $AA          ; 'TAX'
                .byte $A8          ; 'TAY'
                .byte $BA          ; 'TSX'
                .byte $8A          ; 'TXA'
                .byte $9A          ; 'TXS'
                .byte $98          ; 'TYA'    - 37
code_az:

                ;
                ; print code
                ;
p_opcode:       lda opcode
                ora opb
                sta opcode
                rts

                ;
                ; set instruction length
                ; 0, 1 or 2 additional bytes
                ; modifies x
                ;
one_byte:       ldx #$00
                beq setcmdlen ;always jump
two_bytes:      ldx #$01
                bne setcmdlen ;always jump
three_bytes:    ldx #$02
setcmdlen:      stx cmdlen
                jmp finish_asm  ; done, ready to emit code

asm_simple:     lda opmode
                cmp #$02        ; type 0 and 1 do not need address mode specification, use code_a directl
                bpl @chk_shft
                jmp one_byte
@chk_shft:      cmp #$04        ; deal with accu mode shift operations
                bne loc_err
                lda #$08        ; b = %010 -> aaa010cc
                sta opb
                jmp one_byte

asm_imm:        lda opmode
                cmp #$02        ; type 2
                beq asm_imm_ct
                cmp #$05        ; type 5
                beq asm_imm_ct
                cmp #$0A        ; type a
                beq asm_imm_ct
loc_err:        jmp syntaxerr
asm_imm_ct:     jsr g_adr
                lda opcode
                and #$03        ; isolate 'c' bits
                cmp #$01        ; c=3 has b = %010 for imm, others b=000
                bne @zerob
                lda #$08        ; when c= %11 immediate is placed at b=%010
@zerob:         sta opb
                jmp two_bytes
                ;
                ; get the address
                ; if byte size is demanded, only the lower byte is used.
                ; return the character after the numbers in a
                ;
g_adr:          iny
g_adr_l:        jsr eatblank  ; get rid of spaces and $
                jsr getnum
                dey
                lda a2l
                sta adrl
                lda a2h
                sta adrh
                lda in,y
                rts

                ; So after the mnemonic, there can be three options:
                ; - #
                ; - (
                ; - Address
                ;
                ; After the address there can only be
                ; - ,X|Y  for indexed
                ; - ) for indirect
                ; - ),Y
                ; - ,X)
                ;
                ; Value is $?[\dA-Z]{1,2}
                ; Address is [$?][\dA-Z]{1,4}
eval_arg:       ldx mnem        ; load x with opcode index
                lda opmodes,x   ; mode for x
                sta opmode      ; for access without x reg
                lda code_ac,x   ; bac opcode base for x
                sta opcode      ; for access without x reg
                lda #$00
                sta opb     ; clear opcode b
                lda in,y
                sta mode
                jsr chk_eol      ; end of line
                beq asm_simple  ; nothing there

                beq @err        ; type 0 does not have any arguments.  Eliminate type 0 from further considerations.
                lda in,y        ; back to input char
                cmp #'#'
                beq asm_imm
                ;
                ;
                ; now parse rest of address to determine address mode and get argument
                ;
                ; following options are possible (h = hex digit):
                ; h{1.4}       absolute or zero             ; case 0: just 4 hex numbers   2 or 3 bytes
                ; h{1.4},x|y   absolute or zero indexed     ; case 1: ends with ,x|y       2 or 3 bytes
                ; (h{1,4})     indirect                     ; case 2: starts with (, ends with )  3 bytes
                ; (h{1,2}),y   post-indexed indirect        ; case 3: starts with (, ends with ),y  2 bytes
                ; (h{1,2},x)   pre-indexed indirect;       ; case 4: starts with (, ends with ,x)   2 bytes
                cmp #'('
                beq @indirect
                ;
                ; cases 0 and 1
                ;
                jsr g_adr_l
                lda in,y
                jsr chk_eol
                bne @cont
                jmp asm_direct      ; no more input -> case 0
@cont:          jsr eatblank
                cmp #','             ; only , x|y possible now
                beq @reg_indexed    ; -> check reg next
@err:           jmp syntaxerr       ; syntax error for weverything else
@reg_indexed:   jsr nextch
                jsr chk_xy          ; check if valid reg
                bne @err             ; no, syntax error
                jmp asm_indexed     ; yes, handle hhhh,reg (case 1)
                ;
                ; cases 2-4
@indirect:      jsr g_adr           ; address comes after the brace in any case left..
                cmp #','
                beq @preindexed     ; ',' can only be (hh,x)  -> case 4
                cmp #')'            ;
                beq @ind_or_pi      ; ) can be indirect or indirect post-indexed (hhhh) or (hh),y
                bne @err            ; else err; we have to close the brace at least.
                ;
                ; case 4
                ;
@preindexed:    jsr nextch
                cmp #'X'
                bne @err
                jsr nextch
                cmp #')'
                bne @err
                jsr nextch
                jsr chk_eol
                bne @err
@chk23:         lda opmode
                cmp #$04                ; only works for types 2 % 3
                bpl @err
                jmp two_bytes          ; b = 0, no need to change opcode, opb already set for hh,Y
                ;
                ; cases 2-3
                ;
@ind_or_pi:     jsr nextch
                cmp #','
                beq @indir_reg       ; (hh), -> case 3 reg test
                jsr chk_eol
                beq asm_indirect    ; ->  case 2
                bne @err       ; something unexpected in the line
@indir_reg:     jsr nextch
                cmp #'Y'
                bne @err
                ;
                ; case 3
                ;
                jsr nextch
                jsr chk_eol
                bne @err
                lda #$10            ; default: abs. Or in b = %100 into aaa100cc
                sta opb
                bne @chk23          ; only do it if type 2 or 3

                ; asm_indirect: this only happens with jmp
                ; x must be $1B
asm_indirect:   lda opcode
                cmp #$4C
                beq @check
                jmp syntaxerr
@check:         lda #$20        ;  Or in  %001.000.00 into 010.011.00
                sta opb
                jmp three_bytes
                ;
                ; wrap up the assembly:
                ; emit code bytes to target position,
                ; then borrow the system monitors list command
                ; to print out the redisassenbled line.
                ;
                ;
finish_asm:     jsr p_opcode
                ldy cmdlen
@loop:          lda opcode,y
                sta (pcl),y
                dey
@short:         bpl @loop
                lda #$01  ; disassemble just the one line
                jsr list2
                lda pcl
                sta a1l
                lda pch
                sta a1h
                jmp asmz
                ;
                ; check for end of line - eihter ENTER (set to 0x0a) or 0
                ; Z set if reached eol
                ; imput: current input char in a
                ;
chk_eol:        cmp #$00
                beq @fin
                cmp #k_entr
@fin:           rts

chk_xy:         cmp #'X'
                beq @fin
                cmp #'Y'
@fin:           rts
                ;
                ; get relative address from pc
                ; and literal address in a1
                ; pc now points at rel address byte.
                ; address only convers -128 - +127,
                ; if the distance is larger it is an error
                ;
get_rel_a:      sec                 ; set carry for following substraction
                lda pcl             ;
                adc #$01            ; add 2 to be relative to pc+2
                sbc a2l
                sta adrl            ; store in low adr
                lda pch
                sbc a2h
                bmi @chklow
                lda adrl
                bmi @chkerr         ; pos > $7f
@chkok          jmp two_bytes       ; no opb variants for rel branches
@chklow         lda adrl
                bmi @chkok          ; neg < $80
@chkerr         jmp syntaxerr       ; done, rel addr in adrl
                ;
                ; asm_direct: absolute or zero addr
                ; if adrh == 0 and operation allows zero mode,
                ; use that one. else go for absolute address (16bit)
                ; if op does not have that either, throw syntax error
asm_direct:     lda opmode
                cmp #$01            ; relative branch; special treatment -> compute rel. addr. -> type 2 finished
                beq get_rel_a
                lda in,y
                jsr chk_zero_abs    ; from here on, just examine types 3-b. use zero page where possible.
                bcc @abs_b
                lda #$04            ; or in b = %001 into aaa001cc
                sta opb
                jmp two_bytes
@abs_b:         lda #$0C            ; abs. Or in b = %011 into aaa011cc
                sta opb
                jmp three_bytes

                ;
                ; asm_indexed: select one of the indexed option.
                ; First examine if it is x or y,
                ; the abs or zero
                ; input: a - current in char
asm_indexed:    cmp #'Y'
                beq @idx_y
                lda opmode
                cmp #$05            ; types 2-4, 0 and 1 ignored
                bmi @has_idx_x
                cmp #$0a
                beq @has_idx_x
@err:           jmp syntaxerr
@has_idx_x:     lda adrh            ; zero or absolute?
                bne @abs_x          ; weed out stx/sty
                ;
                ; zpg,X
                ;
                cpx #$1E            ; ldx hh,x no
                beq @err
                cpx #$30            ; stx hh,x no
                beq @err
                lda #$14            ; default: abs. Or in b = %101 into aaa101cc
                sta opb
                jmp two_bytes
                ;
                ; abs,X
@abs_x:         cpx #$1E            ; ldx hhhh,x no
                beq @err
                cpx #$30            ; stx hhhh,x no
                beq @err
                cpx #$31            ; sty hhhh,x no
                beq @err
                lda #$34            ; default: abs. Or in b = %111 into aaa111cc
                sta opb
                jmp three_bytes
@idx_y:         cpx #$1F            ; ldy hhh,y no
                beq @err
                cpx #$31            ; sty hhh,y no
                beq @err
                lda adrh
                bne @abs_y
                lda opmode
                cmp #$0a
                bmi @err
                lda #$14            ; abs. Or in b = %101 into aaa101cc
                sta opb
                jmp two_bytes
@abs_y:         lda opmode
                cmp #$04
                bmi @abs_y_low
                cmp #$0a
                beq @abs_y_ldx
                bne @err
@abs_y_low:     lda #$18            ; default: abs. Or in b = %110 into aaa110cc
                bne @abs_y_out
@abs_y_ldx:     lda #$1C            ; default: abs. Or in b = %111 into aaa111cc
@abs_y_out:     sta opb
                jmp three_bytes

bar_ldx:

                ;
                ; (hh),reg
                ;
indir_reg:      jmp finish_asm
                ;
                ; check if mnemonic allows zero page address mode
                ; input:
                ;   x - opcode index
                ;   a - opmode for index
                ; return c=1 if mode available
chk_zero_abs:   cmp #$08
                beq @ret0
                cmp #$09
                beq @ret0
                lda adrh
                beq @ret1
@ldxy:          cpx #$1E   ; ldx and ldy with direct address only work with zero page. throw error if adr out of zero
                beq @err
                cpx #$1F
                beq @err
@ret0:          clc
                rts
@ret1:          sec
                rts
@err:           jmp syntaxerr



