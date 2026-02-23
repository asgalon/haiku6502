;
; Mini-Assembler
;
; Assembles single instruction at the program cursor into machine code.
; Following the syntax documented in the Apple II Reference manual
;
; Working with xa assembler
; 65C02 bitwise operations in xa need syntax variant <op> #b,zp... instead of <op>b zp....
; Todo Fix 65C02 opcodes
;
nextch:         iny
                lda in,y
                rts
                ;
                ; consume blanks and $ signs,
                ; since all numbers are hex. $ basically is whitespace here to keep it simple.
                ; so lda $00 can also be written lda$ 00 or lda $ $$  $$$  $00....
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
                jsr getnum          ; leaves the eor'ed ':' in a and in,y pointing to next chat
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
                ; get coded mnemonic into tmp
                ; ((menm0 & 0x1f) << 10) + ((menm1 & 0x1f) << 5) + (menm2 & 0x1f)
                ; so the opcode forms a word with 13 bits used.
                ; impossible mnemonics produce a syntax error, invalid mnemonics will produce an
                ; unknown opcode
                ;
@procmnm:       and #$1F    ; ex.:  PHP                                            'P' = $50 -> $10
                sta tmph     ;
                stz in      ; put a zero into first position to mark input buffer old news.
                jsr nextch  ; $1F useful bits, have to make room for 5 more        `H' = $48, useful $08
                asl         ; $3E                                                        $90
                asl         ; $7C                                                        $20
                asl         ; $F8                                                        $40
                asl         ; $01F0 high bit-> carry                                     $80 + 0
                rol tmph     ; $3FF0                                               $20    $80
                asl         ; $03E0 high bit-> carry                                     $00 + 1
                rol tmph     ; $7FE0                                               $41    $00
                sta tmpl     ; $E0 , five lower bits free                                 $00
                jsr nextch  ; $1F                                                  'P' = $50
                and #$1F    ;                                                            $10
                ora tmpl     ;                                                            $10
                sta tmpl     ;                                                     $41    $10
                ;
                ; now we have the short form in tmp.
                ; go on to find the mnemonic in opcodex table
                ; and then return this index into the opcode lookup tables
                ldx #(opcodez-opcodex-1)+3
@firstfail:     dex
@scndfail:      dex
                bne @cont
                jmp syntaxerr
@cont:          lda opcodex-1,x     high
                cmp tmph
                bne @firstfail
                dex
                lda opcodex-1,x     low
                cmp tmpl
                bne @scndfail
                txa
                lsr         ; rest of the tables have bytes not words, so half it.
                sta mnem
                iny                 ; point to char after mnemonic
                ;
                ; at this point, we have the address set in a1-a3 and the
                ; mnemonic code number in mnem.
                ; Now proceed to  evaluate address part.
                ; Following modes are possible:
                ; - None. Command does not take arguments, it is finished.
                ; - Immediate:  #Value
                ; - Adr/Rel:    Address
                ; - Indexed:    Address,X|Y   -----+--- branching after ','
                ; - Zero,rel:   Address,Adress ---/
                ; - indirect:   (Address) -------\
                ; - indir. indxd (Address),Y -----+---- joint stem "(Address"
                ; - indexed ind. (Address,X) ----/
                ldx #$03
@bitnlp:        cmp @bitn_mnems,x
                bne @bitnct
                pha
                jsr nextch
                cmp #$30
                bcs @bitnct
                cmp #$38
                bcc @bitnct
                and #$07
                asl
                asl
                asl
                asl
                sta opb           ; save bit number as hogh nibble for integration into opcode
@bitnct:        dex
                bpl @bitnlp
                jsr eatblank
                jmp eval_arg
@bitn_mnems:    .byte $03,$04,$2E,$37   ; RMB,SMB,BBR and BBS are followed by single digit bit number
;
; opcode char mappings
;
opcodex:
                .include "opcodes_compressed.s"
opcodez:
;
; opmode groups
;
;                                       |Impl|Imm|Abs |Zero|z_x |z_y |a_x |a_y |Rel |ix_ir|ir_ix|Ind|(z)|(abs,x)| zr |
; type 0: implied/accu, no arguments    | *  |   |    |    |    |    |    |    |    |     |     |   |   |       |    |
; type 1: relative                      |    |   |(*) |    |    |    |    |    | <- |     |     |   |   |       |    |
; type 2: full in (like adc)            |    | * | *  | *  | *  |    | *  | *  |    | *   | *   |   | * |       |    |
; type 3: full out (without implied)    |    |   | *  | *  | *  |    | *  | *  |    | *   | *   |   |   |       |    |
; type 4: shifting                      | *  |   | *  | *  | *  |    | *  |    |    |     |     |   |   |       |    |
; type 5: cpx                           |    | * | *  | *  |    |    |    |    |    |     |     |   |   |       |    |
; type 6: bit                           |    | * | *  | *  | *  |    | *  |    |    |     |     |   |   |       |    |
; type 7: dec                           | *  |   | *  | *  | *  |    | *  |    |    |     |     |   |   |       |    |
; type 8: jmp                           |    |   | *  |    |    |    |    |    |    |     |     | * |   |    *  |    |
; type 9: jsr                           |    |   | *  |    |    |    |    |    |    |     |     |   |   |       |    |
; type a: ldx/ldy changing index reg    |    | * |    | *  | y  | x  | y  | x  |    |     |     |   |   |       |    |
; type b: stx/sty changing index reg    |    |   | *  | *  | y  | x  |    |    |    |     |     |   |   |       |    |
; type c: bbr/bbs zpg,rel               |    |   |    |    |    |    |    |    |    |     |     |   |   |       |  * |
; type d: rmb/smb                       |    |   |    | *  |    |    |    |    |    |     |     |   |   |       |    |
; type e: trb/tsb                       |    |   | *  | *  |    |    |    |    |    |     |     |   |   |       |    |
;
; 4 bits per mnem index, low first high second
opmodes:
                .byte $22          ; 'ADC' - 'AND'   - 00   - 00
                .byte $C4          ; 'ASL' - 'BBR'
                .byte $1C          ; 'BBS' - 'BCC'
                .byte $11          ; 'BCS' - 'BEQ'
                .byte $16          ; 'BIT' - 'BMI'   - 08   - 04
                .byte $11          ; 'BNE' - 'BPL'
                .byte $01          ; 'BRA' - 'BRK'
                .byte $11          ; 'BVC' - 'BVS'
                .byte $00          ; 'CLC' - 'CLD'   - 10   - 08
                .byte $00          ; 'CLI' - 'CLV'
                .byte $52          ; 'CMP' - 'CPX'
                .byte $75          ; 'CPY' - 'DEC'
                .byte $00          ; 'DEX' - 'DEY'   - 18   - 0c
                .byte $72          ; 'EOR' - 'INC'
                .byte $00          ; 'INX' - 'INY'
                .byte $98          ; 'JMP' - 'JSR'
                .byte $a2          ; 'LDA' - 'LDX'   - 20   - 10
                .byte $4a          ; 'LDY' - 'LSR'
                .byte $20          ; 'NOP' - 'ORA'
                .byte $00          ; 'PHA' - 'PHP'
                .byte $00          ; 'PHX' - 'PHY'   - 28   - 14
                .byte $00          ; 'PLA' - 'PLP'
                .byte $00          ; 'PLX' - 'PLY'
                .byte $4D          ; 'RMB' - 'ROL'   - 2E
                .byte $04          ; 'ROR' - 'RTI'   - 30
                .byte $20          ; 'RTS' - 'SBC'
                .byte $00          ; 'SEC' - 'SED'
                .byte $D0          ; 'SEI' - 'SMB'    - 37
                .byte $03          ; 'STA' - 'STP'    - 38
                .byte $bb          ; 'STX' - 'STY'
                .byte $0b          ; 'STZ' - 'TAX'
                .byte $E0          ; 'TAY' - 'TRB'
                .byte $0E          ; 'TSB' - 'TSX'    - 40
                .byte $00          ; 'TXA' - 'TXS'
                .byte $00          ; 'TYA' - 'WAI'    - 45
opmodez:
                ;
                ;   This is the basic opcodes with bits %aaabbbcc.
                ;   for instructions that only have implied mode
                ;   the full opcode is given with bbb bits
                ;   also set to speed up emission.
                ;   see https://www.masswerk.at/6502/6502_instruction_set.html#layout
                ;
code_ac:
                .byte $61          ; 'ADC'    - 00
                .byte $21          ; 'AND'
                .byte $02          ; 'ASL'
                .byte $0F          ; 'BBR'
                .byte $8F          ; 'BBS'
                .byte $90          ; 'BCC'
                .byte $B0          ; 'BCS'
                .byte $F0          ; 'BEQ'
                .byte $06          ; 'BIT'    - 08
                .byte $30          ; 'BMI'
                .byte $D0          ; 'BNE'
                .byte $10          ; 'BPL'
                .byte $80          ; 'BRA'
                .byte $00          ; 'BRK'
                .byte $50          ; 'BVC'
                .byte $70          ; 'BVS'
                .byte $18          ; 'CLC'    - 10
                .byte $D8          ; 'CLD'
                .byte $58          ; 'CLI'
                .byte $B8          ; 'CLV'
                .byte $61          ; 'CMP'
                .byte $E0          ; 'CPX'
                .byte $C0          ; 'CPY'
                .byte $C2          ; 'DEC'
                .byte $CA          ; 'DEX'    - 18
                .byte $88          ; 'DEY'
                .byte $41          ; 'EOR'
                .byte $E2          ; 'INC'
                .byte $E8          ; 'INX'
                .byte $C8          ; 'INY'
                .byte $4C          ; 'JMP'
                .byte $20          ; 'JSR'
                .byte $A1          ; 'LDA'    - 20
                .byte $A2          ; 'LDX'
                .byte $A0          ; 'LDY'
                .byte $42          ; 'LSR'
                .byte $EA          ; 'NOP'
                .byte $01          ; 'ORA'
                .byte $48          ; 'PHA'
                .byte $08          ; 'PHP'
                .byte $DA          ; 'PHX'    - 28
                .byte $5A          ; 'PHY'
                .byte $68          ; 'PLA'
                .byte $28          ; 'PLP'
                .byte $FA          ; 'PLX'
                .byte $7A          ; 'PLY'
                .byte $07          ; 'RMB'
                .byte $22          ; 'ROL'
                .byte $62          ; 'ROR'    - 30
                .byte $40          ; 'RTI'
                .byte $60          ; 'RTS'
                .byte $E1          ; 'SBC'
                .byte $38          ; 'SEC'
                .byte $F8          ; 'SED'
                .byte $78          ; 'SEI'
                .byte $87          ; 'SMB'
                .byte $81          ; 'STA'    - 38
                .byte $DB          ; 'STP'
                .byte $82          ; 'STX'
                .byte $80          ; 'STY'
                .byte $00          ; 'STZ'
                .byte $AA          ; 'TAX'
                .byte $A8          ; 'TAY'
                .byte $14          ; 'TRB'
                .byte $04          ; 'TSB'    - 40
                .byte $BA          ; 'TSX'
                .byte $8A          ; 'TXA'
                .byte $9A          ; 'TXS'
                .byte $98          ; 'TYA'
                .byte $CB          ; 'WAI'    - 45
code_az:
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
g_adr_r:        iny
                jsr eatblank  ; get rid of spaces and $
                jsr getnum
                dey
                lda a2l
                sta a1l
                lda a2h
                sta a1h
                lda in,y
                rts

                ;
                ; check for end of line - eihter ENTER (set to 0x0a) or 0
                ; Z set if reached eol
                ; imput: current input char in a
                ;
chk_eol:        cmp #$00
                beq @fin
                cmp #k_entr
@fin:           rts

                ;
                ; returns Z=1 if 'x' or 'y' in a
                ;
chk_xy:         cmp #'X'
                beq @fin
                cmp #'Y'
@fin:           rts
                ;
                ; substract addresses stored in zp
                ; [x,x+1] - [y,y+1]
                ; result in [auxl,auxh
sub16:          lda loc0,x
                sec
                sbc loc0,y
                sta calcl
                lda loc1,x
                sbc loc1,y
                sta calch
                rts;
                ;
                ; print code
                ;
p_opcode:       lda opcode
                ora opb
                sta opcode
                rts

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
eval_arg:       ldx mnem            ; load x with opcode index
                lda code_ac,x       ; bac opcode base for x
                sta opcode          ; for access without x reg
                txa
                lsr
                tax                 ; opmodes nibble select in carry
                lda opmodes,x       ; mode for x
                jsr selnibl
                sta opmode          ; for access without x reg
                smb #1,mode           ; set mode to != 0 so getln does not copy a1 to a2, a3
                and #$07
                cmp #$07            ; test for RMB SMB BBR BBS
                beq @to_addr_part   ; yes, opcode and high bitno in opb already have all parts
@opcont:        lda #$00
                sta opb             ; clear opcode b
                lda in,y
                jsr chk_eol         ; end of line
                bne @opc2
                jmp asm_simple      ; nothing there
@opc2:          lda opmode
                beq @err            ; type 0 does not have any arguments.  Eliminate type 0 from further considerations.
@to_addr_part:  lda in,y            ; back to input char
                cmp #'#'
                bne @opc3
                jmp asm_imm
                ;
                ;
                ; now parse rest of address to determine address mode and get argument
                ;
                ; following options are possible (h = hex digit):
                ; h{1.4}              absolute or zero             ; case 0: just 4 hex numbers   2 or 3 bytes
                ; h{1.4},x|y|h{1,4}   absolute or zero indexed     ; case 1: ends with ,x|y|addr  2 or 3 bytes
                ; (h{1,4})            indirect                     ; case 2: starts with (, ends with )  3 bytes
                ; (h{1,2}),y          post-indexed indirect        ; case 3: starts with (, ends with ),y  2 bytes
                ; (h{1,2},x)          pre-indexed indirect;       ; case 4: starts with (, ends with ,x)   2 bytes
@opc3:          cmp #'('
                beq @indirect
                ;
                ; cases 0 and 1
                ;
                jsr g_adr_l
                jsr chk_eol
                bne @cont
                jmp asm_direct      ; no more input -> case 0
@cont:          jsr eatblank
                cmp #','             ; only , x|y possible now
                beq @reg_indexed    ; -> check reg next
@err:           jmp syntaxerr       ; syntax error for weverything else
@reg_indexed:   jsr nextch
                jsr chk_xy          ; check if valid reg
                bne @zp_rel         ; no, must be zp,rel
                jmp asm_indexed     ; yes, handle hhhh,reg (case 1)
@zp_rel:        lda opcode
                and #$0F
                eor #$0F            ; must end on F for zero,rel mode
                bne @err
                jsr g_adr_r           ; rel part
                jmp get_rel_a

                ;
                ; cases 2-4
@indirect:      jsr g_adr           ; address comes after the brace in any case left..
                cmp #','
                beq @preindexed     ; ',' can only be (hh,x)  -> case 4
                cmp #')'            ;
                beq ind_or_pi      ; ) can be indirect or indirect post-indexed (hhhh) or (hh),y
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
chk23:          lda opmode
                cmp #$04                ; only works for types 2 % 3
                bpl err1
                bra two_bytes          ; b = 0, no need to change opcode, opb already set for hh,Y

                ;
                ; set instruction length
                ; 0, 1 or 2 additional bytes
                ; modifies x
                ;
one_byte:       ldx #$00
                bra setcmdlen
two_bytes:      ldx #$01
                bra setcmdlen
three_bytes:    ldx #$02
setcmdlen:      stx cmdlen
                bra finish_asm  ; done, ready to emit code

asm_simple:     lda opmode
                cmp #$02        ; type 0 and 1 do not need address mode specification, use code_a directl
                bmi one_byte
@chk_shft:      cmp #$04        ; deal with accu mode shift operations
                bne err1
@is_shft:       lda #$08        ; b = %010 -> aaa010cc
                sta opb
                bra one_byte

                ;
                ; cases 2-3
                ;
ind_or_pi:      jsr nextch
                cmp #','
                beq @indir_reg       ; (hh), -> case 3 reg test
                jsr chk_eol
                beq asm_indirect    ; ->  case 2
                bne err1       ; something unexpected in the line
@indir_reg:     jsr nextch
                cmp #'Y'
                bne err1
                ;
                ; case 3
                ;
                jsr nextch
                jsr chk_eol
                bne err1
                lda #$10            ; default: abs. Or in b = %100 into aaa100cc
                sta opb
                bne chk23          ; only do it if type 2 or 3
err1:           jmp syntaxerr

                ; asm_indirect: this only happens with jmp
                ; x must be $1B
asm_indirect:   lda opcode
                cmp #$4C
                bne err1
                lda #$20        ;  Or in  %001.000.00 into 010.011.00
                sta opb
                bra three_bytes
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
                bpl @loop
                lda #$01  ; disassemble just the one line
                jsr list2
                lda pcl
                sta a1l
                lda pch
                sta a1h
                jmp asmz
                ;
                ; get relative address from pc
                ; and literal address in a1
                ; pc now points at rel address byte.
                ; address only convers -128 - +127,
                ; if the distance is larger it is an error
                ; TODO: optimize validity test
                ;
get_rel_a:      stx xreg
                sty yreg
                ldx #a2l
                ldy #pcl
                jsr sub16           ;  calc = target - pc
                lda #$02
                sta tmpl
                lda #$00
                sta tmph
                ldx #calcl
                ldy #tmpl
                jsr sub16           ; substract 2 from result for jump relative to pc+2
                ldx xreg            ; restore x and y registers
                ldy yreg
                lda calch
                cmp #$FF
                beq @chk_neg
                cmp #$00
                bne @chkerr
                bit calcl
                bpl @chkok
@chkerr:        jmp syntaxerr
@chk_neg:       bit calcl
                bpl @chkerr
@chkok:         lda calcl
                ldx opcode
                cpx #$0f
                bne @n_branch       ; if not 0f, do branch in arg1, else 3 byte bittest op bbr/bbs with branch in arg2
                sta adrh
                jmp three_bytes
@n_branch:      sta adrl            ; done, rel addr in adrl
                jmp two_bytes       ; no opb variants for rel branches
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
asm_indexed:    ldx mnem
                cmp #'Y'
                beq @idx_y
                lda opmode
                cmp #$05            ; types 2-4, 0 and 1 ignored
                bmi @has_idx_x
                cmp #$0a            ; types a and b , ldx/y and stx/y
                bpl @has_idx_x
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
                lda #$1C            ; default: abs. Or in b = %111 into aaa111cc
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
@ldxy:          cpx #$1E   ; ldx and ldy with direct address only work with zero page. throw error if adr not in zp
                beq @err
                cpx #$1F
                beq @err
@ret0:          clc
                rts
@ret1:          sec
                rts
@err:           jmp syntaxerr



