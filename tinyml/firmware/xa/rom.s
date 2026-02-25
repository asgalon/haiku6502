;
; haiku6502 ROM
;
; Annotated and modified emulator ROM using orig_monitor_rom.s as a template.
; This code is only for academic purposes.
; This file is compatible with xa assembler and is not relocatable.
; Monitor command jump table changed to full word vectors for now, changes did not fit within FE page.
; There are a few nifty tricks in the original assembler code to save a few bytes here and there that probably
; would not have been done this way were there more than 12KB address space available for ROM. Brilliant, but difficult to maintain.
; This rom incorporates 65C02 instructions into the system monitor and mini-assembler (TODO), which makes it a lot larger. For other applications,
; a stirpped down ROM version is needed to maximize application storage. With this one, probably the whole top 4K are full at the end unless
; it can be optimized a lot.
;
; The following modifications have been made:
;
; - fixed character mapping. ASCII chars are now ASCII chars in the right code points.
; - removed memory mapped text and graphics screens. The screen is now a terminal peripheral controlled
;   through I/O ports 0xC010-0xC01F
; - keyboard strobe is now in 0xC001, to make space for more i/o control addresses
; - removed screen address calculations, no longer needed with sane terminal cursor coordinates that
;   are no longer aligned with cathode ray tube electronic beam scan sequence.
; - Bell just sends a CTL-G to terminal
; - Tape in/out write/reads the memory directly to the tape file, without square wave frequency modulated encoding.
; - The screen size is taken from the terminal dimensions. Size is no longer restricted to 1KB text pages.
; - Lo-Res and Hi-Res graphics modes are gone for now since they don't work too well with the standard ncurses based
;   terminal peripheral. A graphics terminal peripheral could be added, though.
; - The peripheral system is not completed, especially emulator interrupt management is rudimentary. So peripheral
;   rom areas can be addressed, but the extension rom area 0xC800-0xC8FF needs work. This does not ffect this rom,
;   though...
; - Added 'Q' command to monitor to terminate emulator via jump to address $FFFF
; - Added Mni-Assembler command as '!', '*' returns from mini-assembler to monitor
; - Mini-Assembler resurrected following mostly the manual description. It is a complete new implementation in asm.s
; The monitor part works pretty much the same as described in the Apple II Reference Manual. It has a nice disassembler.
; It provides RAM editing as hex dump only for now.
;
; Memory layout:
;
; 0x0000-0x00FF Zero page
; 0x0100-9x01FF Stack
; 0x0200-0x02FF text buffer
; 0x0300-0x03FF system vars & vectors
; 0x0400-0xBFFF free RAM
; 0xC000-0xCFFF I/O
; 0xD000-0xFFFF ROM; thereof
;               0xD000 - 0xF000 Reserved for language modules
;               0xF000 - 0xF800 Mini-Assembler (asm.s)
;               0xF800 - 0xFFFF System Monitor ROM
;               0xFFFA - 0xFFFF Hardwired 6502 NMI, Reset and IRQ vectors, have to be kept at fixed addresses.
; On Reset, the program counter is loaded from 0xFFFC and 0xFFFD. all addresses with least
; significant (lsb,low) byte first.
;
; Adapted for haiku6502 65C02 variant
;

                .include "symbols.inc"

                .org    $F200       ; ROM start address

                .dsb $1BA, $EA
                .include "asm.s"

                ;
                ; print zero terminated string
                ; max 255 chars.
                ; input:
                ;   string address in a5l/h
print:          ldy #$00
@loop:          lda (a5l),y
                beq @fin
                jsr cout
                iny
                bne @loop
@fin:           rts
                ;
                ; Emulator exit
                ; terminate by setting PC to $FFFF, which should not happen in normal operation.
                ;
exit:           jmp $FFFF
                ;
                ; getline vector.
                ;
getlnv:         jmp (rdline)
stdin:          lda termin
                rts
                ;
                ; readline from stdio
                ; this will block until line is read
                ; this way it uses standard readline
                ; funcionality hostside.
                ;
stdrdln:        phy
                lda prompt      ; set prompt
                sta termp
                ldy #$FF
loop:           iny
                lda terml
                sta in,y
                bne loop
                lda #k_entr
                sta in,y
                ply
                rts
stdout:         sta termout
                rts
nxtcol:         lda color           ; increment color by 3
                clc
                adc #$03
setcol:         and #$0F            ; sets color = 17*A mod 16
                sta color
                asl
                asl
                asl
                asl                 ; << 4 = *16
                ora color           ; + 1
                sta color
                rts
selnibl:        bcc rtmaskz         ; if even, use low nibble else use high nibble (was scrn2)
selhigh:        lsr
                lsr
                lsr                 ; shift high nibble into low nibble
                lsr
rtmaskz:        and #$0F            ; mask lower 4 bits
                rts
insds1:         ldx pcl             ; print pcl,h
                ldy pch
                jsr pryx2
                jsr prblnk          ; followed by a blank
                lda (pcl)
                sta opcode
insds2:         tay
                lsr                 ; even / odd test for aaab bbcc opcode with cc odd or even
                bcc ieven           ; now acc = 0aaa bbbc with lower c in carry flag
                ;
                ; now cc is 01 or 11, 11
                ;
                ror                 ; bit 1 test, acc now 10aa abbb, c high bit in carry flag
                bcc @insc1          ; xxxxxx11 -> RMB SMB, WAI, STP, BBR BBS on 65C02
                and #03             ; test low bb bits. 1 -> RMB/SMB, 2-> WAI,STP, 3 -> BBR,BBS 0-> column 03 -> err
                beq err
@fmt_cnt:       tax
                cpx #$02
                bne @not_b
                lda opcode
                and #$E0            ; column B has only CB and DB.
                cmp #$C0
                bne err
@not_b:         lda @fmt_xtra,x     ; fmt2 index for 0-N/A, 1-RMB SMB->ZERO 2-WAI,STP->IMPLIED, 3-BBR BBS->$10 zpg,rel
                bra getfmt
@fmt_xtra:      .byte $00,$02,$00,$10
@insc1:         and #$87            ; mask bits c000 0bbb
ieven:          lsr                 ; lsb into carry for l/r test (b low bit), acc = 0100 00bb (odd) or 00aa abbb
                                    ; so fmt1 index goes from 0x00 to 0x43 = dez. 67
                tax
                lda fmt1,x          ; get format index byte
                jsr selnibl         ; r/l h-byte on carry
                bne getfmt          ; 0 marks invalid ops

err:            lda #$20             ; set print format index to 0
                stz format
                stz length
                rts
getfmt:         tax
                lda fmt2,x          ; index into print format table
                sta format          ; save for addr field formatting
                and #$03            ; mask for 2-bit length
                                    ; (0=1 byte, 1=2 byte, 2=3 byte)
                sta length
                tya                 ; opcode
                and #$8F            ; mask for 1xxx1010 test
                tax                 ;  save it
                tya                 ; opcode to a again
                ldy #$03            ; handle at least 3 bits
                cpx #$8A
                beq mnndx3         ; if mask matches, handle case A, else continue down
                ; form index into mnemonic tble
                ;
                ; 1)  1xxx1010 => 0010 1xxx  (C)   8 mnemonics
                ; 2)  xxxyyy01 => 0011 1xxx  (E)   8 mnemonics
                ; 3)  xxxyyy10 => 0011 0xxx  (D)   8 mnemonics   where lower yy != 00 unless apart from A2
                ; 4)  xxxyy100 => 0010 0xxx  (B)   8 mnemonics
                ; 5)  xxxxx000 => 000x xxxx  (A)  32 mnemonics
                ;
                ; Have to adopt that to 65C02 additions. This algorithm is brilliant,
                ; but completely unserviceable.
                ; So first coes the test for case C. If it matches, it goes to mnndx3, decrements y to 2,
                ; then goes to mnndx1. now a shift to the right, bit 0 into carry (0). mnndx3 again, y=1, shift, carry(1).
                ; now shift 2 bits right (total shifted 4 bits right to 00001xxx), add in shifted 1 bit at $20, now y=0, and done.
                ;
                ; Second case xxxyyy01
                ;
                ; first lsr, so 0xxx yyy0 carry 1 then lsr lsr 000x xxyy. ora 20 001x xxyy. dey to 2. to mnndx2.
                ; lsr 0001 xxxy ora $20 0011 xxxy. dey to 1. to mnndx2
                ; lsr 0001 1xxx 0ra 20 0011 1xxx dey to 0. done.
                ;
                ; Third case xxxyyy10
                ;
                ; first lsr, so 0xxx yyy1 carry 0 then mnndx3. dey to 2, to mnndx1.
                ; lsr, so 00xx xyyy carry 1
                ; then lsr lsr 0000 xxxy. ora 20 0010 xxxy. dey to 1. to mnndx2.
                ; lsr 0001 0xxx ora $20 0011 0xxx. dey to 0. done.
                ;
                ; Fourth case xxxyy100
                ;
                ; first lsr, so 0xxx yy10 carry 0 then mnndx3. dey to 2, to mnndx1.
                ; lsr, so 00xx xyy1 carry 0 then mnndx3. dey to 1, to mnndx1.
                ; lsr, so 000x xyy carry 1
                ; then lsr lsr 0000 0xxx. ora 20 0010 0xxx. dey to 0. done.
                ;
                ; Fifth case xxxxx000
                ;
                ; first lsr, so 0xxx xx00 carry 0 then mnndx3. dey to 2, to mnndx1.
                ; lsr, so 00xx xxx0 carry 0 then mnndx3. dey to 1, to mnndx1.
                ; lsr, so 000x xxxx carry 0 then mnndx3. dey to 0, done.
                ;
                ; now we have a few more cases for 65C02 to deal with:
                ; (a) 0XYY 0100 BIT, STZ                                -> $4A - $4B
                ; (b) 000X Y100 TSB, TRB                                -> $48 - $49
                ; (c) 0XX1 1010 INC, DEC, PHY, PLY                      -> $44 - $47
                ; (d) 1000 1001 BIT #    0001 1.010 10.10 100.0         -> $4E (last one)
                ; (e) 110X 1011 WAI, STP                                -> $4C - $4D
                ; (f) XYYY X111 RMB, SMB, BBR, BBS   -> 0x0100 00XX     -> $40 - $43
                ; (g) XXX1 0010 -> same as case E
                ;
                ; we have indices >= 0100 0000 = $40 free.
                ;
                ; This will not be as elegant as the original ones until I can figure out a good
                ; way to compute all the positions...
                ;
                cmp #$89            ; case d) bit, only one opcode
                bne @mnndxc1
                lda #<(mnemidx_d-mnemidx)  ; one mnemonic, last one.
                rts
@mnndxc1:       and #$1F            ; test case g first
                cmp #$12            ; odd hi rows in lo column 2
                bne @cont8          ;
                lda opcode          ; teanslate into type E index: xxxyyy10 -> 0011 1xxx
                lsr
                lsr
                lsr
                lsr
                lsr
                ora #$38
                rts
@cont8:         and #$0F            ; mask lower nibble
                tax
                lda opcode          ; restore opcode in a
                cpx #$0F            ; compare mask with 0F or 07 for 65C01 instructions
                beq @case_f
@mnndxc2:       cpx #$07
                bne @case_b         ; no match, continue with case b
@case_f:        ldx #$40            ; base for index
                bit #$08            ; A and $08 == 0?
                beq @ttopbit        ; yes, base $40, else base $42
                inx                 ; XYYY 1111 -> BBR, BBS at $42,$43
                inx
@ttopbit:       asl                 ; XYYY 0111 -> RMB, SMB at $40,$41 X -> C
                txa                 ; 0YYY 0111 -> RMB at $40, 0YYY 1111 -> BBR at $42
                adc #$00            ; 1YYY 0111 -> SMB at $41, 1YYY 1111 -> BBS at $43
                rts
@case_b:        and #$E7            ; mask for case b
                cmp #$04            ; 000x y100
                bne @case_a
                lda opcode
                jsr selhigh
                ora #$48
                rts
@case_a:        lda opcode
                and #$8F
                cmp #$04            ; 0XYY 0100
                bne @case_e
                ldx #$4A
                bbr #6,opcode,@mnbit  ; bbr6 opcode,@mnbit
                ldx #$4B
@mnbit:         txa
                rts
@case_e:        lda opcode
                cmp #$CB            ; 0%1100 1011 WAI, 0%1101 1011 STO
                bne @not_wai
                lda #$4C
                rts
@not_wai:       cmp #$DB
                bne @case_c
                lda #$4D
                rts
@case_c:        and #$9F
                tax
                lda opcode          ; restore opcode
                cpx #$1A            ; mask matches case C?
                bne @p6502          ; continue with standard opcode sets
                and #$60
                lsr                 ; 0xx0 0000    ->
                jsr selhigh         ; 0000 00xx
                ora #$44            ; index $44 - $47
                rts

@p6502:         lda opcode
mnndx1:         lsr
                bcc mnndx3         ; form index into mnemonic tble
                lsr
@mnndx2:        lsr
                ora #$20
                dey
                bne @mnndx2
                iny
mnndx3:         dey
                bne mnndx1
                rts
                ;
                ; print bit index for 65c02 bitwise operation mnemonics
                ;
prcbit:         lda monauxl         ; for 65C02 bitwise operations, isolate the bit index
                beq @prbit
                eor #$FF
                beq @prbit
                jmp prblnk          ; print 3 blanks and return from there
@prbit:         lda opcode
                and #$70
                jsr selhigh
                jsr prhexz
                ldx #$02
                jmp prbl2           ; print two blanks and return from there

instdsp:        jsr insds1          ; gen fmt, len bytes
                pha                 ; save mnemonic table index (not opcode)
                ldy #$00            ; clear y
@prntop:        lda (pcl),y
                jsr prbyte
                ldx #$01            ; print 1 blank
@prntbl:        jsr prbl2
                cpy length          ; print inst (1-3 bytes)
                iny                 ; in a 12 char field (65C02 bit ops mnemonics are 4 chars long)
                bcc @prntop
                ldx #$03            ; char count for mnemonic print
                cpy #$04
                bcc @prntbl
                pla                 ; recover mnemonic index
                tay                 ; use as mnemonic index
                lsr                 ; $40-$41 -> $20,$42-$43 -> $21, for BBR BBS special address mode
                sec
                sbc #$21            ; BBR/BBS
                sta monauxl         ; monauxl == 0 if BBR/BBS, not zero else
                lda mnemidx,y       ; mnemind now has the index into opcode_mnem table
                tay
                lda opcode_mnem+1,y
                sta lmnem           ; fetch 3 char mnemonic
                lda opcode_mnem,y ;   (packed in 2 bytes, only chars A-Z
                sta rmnem
prmn1:          lda #$00
                ldy #$05
prnm2:          asl rmnem           ; shift 5 bits of
                rol lmnem           ;   character into Accu
                rol                 ;      (clears carry)
                dey
                bne prnm2
                adc #'?'            ; set char offset for A-Z
                jsr cout            ; output a char of mnem
                dex
                bne prmn1
                jsr prcbit          ; print bit op index, carry set if 2 blanks output
                lda monauxl
                bne @normaladr
                lda #'$'             ; BBR/BBS addr: $<byte1>,$<rel(byte2)>
                jsr cout
                ldy #$01
                lda (pcl),y         ; get arg byte 1
                jsr prbyte
                lda #','             ; BBR/BBS addr: $<byte1>,
                jsr cout
                lda #'$'             ; BBR/BBS addr: $<byte1>,$
                jsr cout
                ldy #$02
                lda (pcl),y
                inc                 ; adjust for pc
                inc
                jmp reladr
@normaladr:     ldy length
                ldx #$06            ; count for 6 format bits
pradr1:         cpx #$03
                beq pradr5          ; if x=3 then addr
pradr2:         asl format          ; fmt2 get highest bit
                bcc pradr3
                lda char1-1,x
                jsr cout
                lda char2-1,x
                beq pradr3
                jsr cout
pradr3:         dex
                bne pradr1
                rts
pradr4:         dey
                bmi pradr2
                jsr prbyte
pradr5:         lda format
                cmp #$E8            ; handle relative address mode (format $9D after 3 asls)
                lda (pcl),y         ;  special (print target, not offset)
                bcc pradr4
reladr:         jsr pcadj3
                tax                 ; pcl,pch+offset+1 to a,y
                inx
                bne prntyx          ; +1 to y,x
                iny
                ;
                ; print y and x  as 4 digit hex number
                ;
prntyx:         tya
prntax:         jsr prbyte          ; output target adr
prntx:          txa                 ;    of branch and return
                jmp prbyte
prblnk:         ldx #$03            ; blank count
prbl2:          lda #' '            ; load a space
prbl3:          jsr cout            ; output a blank
                dex
                bne prbl2
                rts
pcadj:          sec                 ; 0=1-byte, 1=2-byte,
pcadj2:         lda length          ;   2=3-byte
pcadj3:         ldy pch             ;                                y = pch
                tax                 ; test displacement sign         x = length
                bpl pcadj4          ;   (for rel branch)      1- pl
                dey                 ;                         y 1->0
pcadj4:         adc pcl             ;                         pcl + A + C
                bcc rts2            ; pcl+LENGTH(or Displc.)+1 to A
                iny                 ;   if carry increase y (pch)
rts2:           rts
;
; fmt1
;       Format aaab bbcc, where cc == Y0 == 00 or 10
;       if Y=0: then left half byte
;       if Y=1  then right half byte
;                    (X=index)                      bytes:   aaab bbY0 instructions             even opcodes
fmt1:           .byte $04,$22,$54,$33               ; aaa == 000, bbb = 000 - 011               $00 - $0E
                .byte $ED,$82,$54,$93               ; aaa == 000, bbb = 100 - 111               $10 - $1E
                .byte $03,$22,$54,$33               ; aaa == 001, bbb = 000 - 011               $20 - $2E
                .byte $ED,$88,$54,$99               ; aaa == 001, bbb = 100 - 111               $30 - $3E
                .byte $04,$20,$54,$33               ; aaa == 010, bbb = 000 - 011               $40 - $4E
                .byte $ED,$80,$44,$90               ; aaa == 010, bbb = 100 - 111               $50 - $5E
                .byte $04,$22,$54,$3B               ; aaa == 011, bbb = 000 - 011               $60 - $6E
                .byte $ED,$88,$44,$9F               ; aaa == 011, bbb = 100 - 111               $70 - $7E
                .byte $0D,$22,$44,$33               ; aaa == 100, bbb = 000 - 011               $80 - $8E
                .byte $ED,$C8,$44,$93               ; aaa == 100, bbb = 100 - 111               $90 - $9E
                .byte $11,$22,$44,$33               ; aaa == 101, bbb = 000 - 011               $A0 - $AE
                .byte $ED,$C8,$44,$A9               ; aaa == 101, bbb = 100 - 111               $B0 - $BE
                .byte $01,$22,$44,$33               ; aaa == 110, bbb = 000 - 011               $C0 - $CE
                .byte $ED,$80,$44,$90               ; aaa == 110, bbb = 100 - 111               $D0 - $DE
                .byte $01,$22,$44,$33               ; aaa == 111, bbb = 000 - 011               $E0 - $EE
                .byte $ED,$80,$44,$90               ; aaa == 111, bbb = 100 - 111               $F0 - $FE
                                                    ; bytes: ZZXXXY01 instructions              odd opcodes
                .byte $26,$31,$87,$9A               ; aaa == ..0, bb. = 00, b low nibble sel    masked 0x39
;
; fmt2
;
fmt2:           .byte $00       ; 0 - ERR
                .byte $21       ; 1 - IMM
                .byte $81       ; 2 - Z-PAGE
                .byte $82       ; 3 - ABS
                .byte $00       ; 4 - IMPLIED
                .byte $00       ; 5 - ACCUMULATOR
                .byte $59       ; 6 - (ZPAG,X)
                .byte $4D       ; 7 - (ZPAG),Y
                .byte $91       ; 8 - ZPAG,X
                .byte $92       ; 9 - ABS,X
                .byte $86       ; A - ABS,Y
                .byte $4A       ; B - (ABS)
                .byte $85       ; C - ZPAG,Y
                .byte $9D       ; D - RELATIVE or ZP,REL
                .byte $49       ; E - (Z-PAGE)      ; 65C02  ( - $40, 2 bytes - $01, ) - $04
                .byte $5A       ; F - (ABS,X)       ; 65C02  ( - $40, 3 bytes - $02, , - $10 ) - $04
                .byte $9E       ;10 - ZERO,REL      ; 65C02  BBx:  3 bytes - $02,    rel for format
char1:          .byte ',', ')', ',', '#', '(', '$'  ; 1st comma X - $10, 2nd comma Y $04
char2:          .byte 'Y',0,"X$$",0
                .include "sysmon_mnemonics_compressed.s"
;
; monitor stepping
; The instruction under the pointer is copied to loctions 3d-45 with two jump directions behind.
; first one is when no branch happens, second one is for active branch condition. branch rel address is always saved
; and rewritten to pc+3. So one instruction is executed from $3D, then the registers printed out, user pc adjusted, and
; return to monitor prompt.
;
step:           jsr instdsp         ; disassemble one instruction
                pla                 ;   at (pcl,h)
                sta rtnl            ; adjust to user
                pla                 ;   stack, save
                sta rtnh            ;   return address
                ldx #$09
xqinit:         lda initbl-1,x      ; init xeq (execute) area
                sta xqt-1,x
                dex
                bne xqinit
                lda (pcl,x)         ; user opcode byte
                beq xbrk            ; special if BRK
                ldy length          ; LEN from disassembly
                stz monauxl         ; prepare flag for 65C02 stuff
                cmp #$20
                beq xjsr            ; handle jsr, rts, jmp,
                cmp #$60            ;   jmp (), rti special
                beq xrts
                cmp #$4C
                beq xjmp            ; when Z is 1, C is 0
                cmp #$6C
                beq xjmpat          ; when Z is 1, C is 0
                cmp #$7C            ; 65C02 JMP (abs,x)
                beq xjmpatx         ; when Z is 1, C is 0
                cmp #$40
                beq xrti
                cmp #$80            ; BRA rel
                bne @notbra
                lda #$04            ; prime branch synth rel addr.
                bra xq2
@notbra:        and #$1F            ; aaabbbcc -> ...bbbcc
                eor #$14            ; 000bbbcc eor 00011000 -> 000!b!bbcc
                cmp #$04            ; copy user instruction to xeq area -> expected ... 1 0000 -> Relative branches
                beq xq2             ;   with trailing nops
                and #$0F            ; test for BBR/BBS
                eor #$0F                ; align with bit 4
                cmp #$04            ; 4 bit exored away, was $FF, resulting in 0%0000 1011
                beq xq2             ; TODO: not really, have to figure out what to do here with the zp,rel
xq1:            lda (pcl),y         ; change rel branch
                inc
xq2:            dec
                sta xqtnz,y         ;   disp to 4 for
                dey                 ;   jmp to branch or
                bpl xq1             ;   nbranch from xeq
                jsr restore         ; restore user reg contents
                jmp xqtnz           ; xeq (execute) user op from RAM
irqvec:         sta acc             ;    (return to nbranch)
                pla                 ; fetch p status register into a
                pha                 ; * * IRQ handler
                asl
                asl
                asl                 ; put B flag in high bit
                bmi break           ; if set, handle BRK
                jmp (irqloc)        ; user routine vector in RAM
break:          plp
                jsr sav1            ; save regs on break
                pla                 ; including pc
                sta pcl
                pla
                sta pch
xbrk:           jsr insds1          ; print user pc
                jsr rgdsp1          ;   and registers
                jmp mon             ; go to monitor
xrti:           clc
                pla                 ; simulate rti by expecting
                sta status          ;    status from stack, then rts
xrts:           pla                 ; rts simulation
                sta pcl             ;   extract pc from stack
                pla                 ; and update pc by 1 (LEN=0)
pcinc2:         sta pch
pcinc3:         lda length          ; update pc by LEN
                jsr pcadj3
                sty pch
                clc                 ; short unconditional jump wtih C as bool param
                bra newpcl          ;  CLC BCC #rel
xjsr:           clc
                jsr pcadj2          ; update pc and push
                tax                 ;   onto stack for
                tya                 ;   JSR simulate
                pha
                txa
                pha
                ldy #$02
xjmpatx:        smb #0,monauxl        ; monauxl = 0x01
xjmp:           clc
xjmpat:         lda (pcl),y
                tax                 ; load pc for jump,
                dey                 ;   (JMP) simulate
                lda (pcl),y
                stx pch
newpcl:         sta pcl
                bbr #0,monauxl,@no_x   ; add x only if bit0 set
                rmb #0,monauxl         ; reset xjmpatx flag
                clc
                lda xreg
                phy
                jsr pcadj3          ; add x offset
                sta pcl
                sty pch
                ply
                sec                 ; carry set for ind loop
@no_x:          bcs xjmp
rtnjmp:         lda rtnh
                pha
                lda rtnl
                pha
regdsp:         jsr crout           ; display user reg
rgdsp1:         lda #<acc           ;  contents with
                sta a3l             ;  labels
                lda #>acc
                sta a3h
                ldx #$FB
rdsp1:          lda #' '
                jsr cout            ; output space
                lda rtbl-$FB,x      ; register name (x has FB so base address = rtbl - FB)
                jsr cout
                lda #'='            ; '='
                jsr cout
                lda acc+5,x
                jsr prbyte
                inx
                bmi rdsp1
                rts
branch:         clc                 ; branch taken,
                ldy length         ;  add length to pc (01 for normal branches, 02 for zp,rel
                lda (pcl),y
                jsr pcadj3
                sta pcl
                tya
                sec
                bra pcinc2
nbranch:        jsr save            ; normal return after
                sec                 ;   xeq user of
                bra pcinc3          ; go update pc
                ; template for step instruction
                ; initblx for zp,rel format
initbl:         nop
                nop
                nop                 ; dummy fill for
                jmp nbranch         ;   xeq area
                jmp branch
                ; Register label table
rtbl:           .byte 'A'
                .byte 'X'
                .byte 'Y'
                .byte 'P'
                .byte 'S'
pread:          lda ptrig           ; paddle read; trigger paddles
                ldy #$00            ; init count
                nop                 ; compensate timing for first count
                nop                 ; 2 x 2 cycles
pread2:         lda paddl0,x        ; count y-reg
                bpl rts2d           ;    every 12 usec
                iny
                bne pread2          ;    exit at 255 max
                dey
rts2d:          rts

init:           stz status          ;   software
settxt:         lda #$00            ;   full screen window
setwnd:         sta wndtop          ; set for 40 col window
                                    ;    top in accu,
                stz wndlft          ;    bottom at line 24
                lda termww          ; get terminal window width
                sta wndwdth
                lda termwh          ; get terminal window height
                sta wndbtm
                sec
                sbc #$01            ; last possible row in window
tabv:           sta cv              ; vtabs to row in accu
                rts
mulpm:          jsr md1             ; abs value of ac, aux
mul:            ldy #$10            ; index for 16 bits
mul2:           lda acl             ; acx * aux + xtnd
                lsr                 ;  to ac, xtnd
                bcc mul4            ; if no carry,
                clc                 ;   no partial product
                ldx #$FE
mul3:           lda xtndl+2,x       ; add multiplicant (aux)
                adc auxl+2,x        ;   to partial product
                sta xtndl+2,x       ;     (xtnd)
                inx
                bne mul3
mul4:           ldx #$03
mul5:           ror acl,x           ; orig DFB #$76, #$50 ?!? This is probably why Woz wanted undocumented ROR to be made official...
                dex
                bpl mul5
                dey
                bne mul2
                rts
divpm:          jsr md1             ; abs value of ac, aux
div:            ldy #$10            ; index for 16 bits
div2:           asl ach
                rol ach
                rol xtndl           ; xtnd/aux
                rol xtndh           ;   to ac.
                sec
                lda xtndl
                sbc auxl            ; mod to xtnd
                tax
                lda xtndh
                sbc auxh
                bcc div3
                stx xtndl
                sta xtndh
                inc acl
div3:           dey
                bne div2
                rts
md1:            ldy #$00            ; abs value of ac, aux
                stz sign            ;   with result sign
                ldx #auxl           ;   in lsb of sign
                jsr md2
                ldx #acl
md2:            lda loc1,x          ; x specifies ac or aux
                bpl mdrts
                sec
md3:            tya
                sbc loc0,x          ; compl specified reg
                sta loc0,x          ;   if negative
                tya
                sbc loc1,x
                sta loc1,x
                inc sign
mdrts:          rts

bell1:          lda #k_ctl_g        ; output bell and return
                sta termout         ; pit to term directly here...
                rts
                ;
                ; Store terminal char output and advance screen cursor
                ;
stoadv:         ldy cv              ; cursor y index to y register
                sty termcy          ; set terminal cursor y
                ldy ch              ; cursor h index to y register
                sty termcx          ; set terminal cursor x
                ldy invflg          ;
                sty termesc         ; invflg has the command byte for the char attribute normal or reverse
                sta termout         ; output char
                ;
                ; advance cursor
                ;
                ; add next line routine if cursor proceeds to right margin
                ;
advance:        inc ch              ; increment cursor h index
                lda ch              ;   (move right)
                cmp wndwdth         ; beyond window width?
                bcs cr              ;   yes, cr to next line
rts3:           rts                 ; no, return
                ;
                ; Video out
                ;
                ; Write character to terminal I/O
                ;
vidout:         cmp #' '            ; control char?
                bcs stoadv          ;   no, output it
                cmp #k_entr           ; CR?  0x1D | 0x80
                beq cr              ;   yes
                cmp #k_lf           ; LF?
                beq lf              ;   yes
                cmp #k_bs           ; backspace (CTRL-H)?
                bne bell1            ;   no, check for bell
bs:             dec ch              ; decrement cursor h index
                bpl rts3            ; if pos, ok, else move up
                lda wndwdth         ; set ch to wndwdth-1
                sta ch
                dec ch              ; rightmost screen position
up:             lda wndtop          ; cursor v index
                cmp cv
                bcs rts4            ; if top line then return
                dec cv              ; decr cursor v
rts4:           rts
esc1:           eor #k_esc            ; esc?
                beq home            ;   if so, do home and clear
                sbc #$02            ; esc-a or -b check
                bmi advance         ;   a, advance
                beq bs              ;   b, backspace
                sbc #$02            ; esc-c or -d check
                bmi lf              ;   c, down
                beq up              ;   d, go up
                sbc #$02            ; esc-e or -f check
                bcc clreol          ;   e, clear to end of line
                bne rts4            ;   not f, return
clreop:         ldy ch              ; cursor h to y
                lda cv              ; cursor v to a
cleop1:         pha                 ; save current line on stk
                lda #t_cls          ; clear screen command
                sta termesc         ; send to terminal
home:           lda wndtop          ; init cursor v
                sta cv              ;   and h-indices
                stz ch              ; then clear to end of page
                beq cleop1
cr:             stz ch              ; cursor to left of index
lf:             inc cv              ; incr cursor v (down 1 line)
                lda cv
                cmp wndbtm          ; off screen?
                bcc rts4            ;   no, done
                dec cv              ; decr cursor v (back to bottom)
scroll:         lda #t_scr          ; scroll command
                sta termesc         ; let the terminal scroll.
                rts
clreol:         lda #t_cll          ; clear to eol command
                sta termesc         ; execute
                rts
wait:           sec
wait2:          pha
wait3:          sbc #$01
                bne wait3           ; 1.02.4 uSec
                pla                 ; (13+2712*A+512*A*A) on 1 MHz
                sbc #$01
                bne wait2           ; busy, busy, busy waiting
                rts
                ;
                ; increment source and target addresses a1 and a4
                ; until source reaches a2
                ; sets carry when a1 >= a2
                ;
nxta4:          inc a4l             ; incr 2-byte a4
                bne nxta1           ;  and a1
                inc a4h
                ; reuse nxta1 for nxta4...
                ;
                ;  increment a1 until it reached a2
                ;
nxta1:          lda a1l             ; incr 2-byte a1
                cmp a2l             ;    set carry if a1l >= a2l
                lda a1h             ;   and compare to a2
                sbc a2h             ;  carry set if a1 >= a2
                inc a1l
                bne rts4b
                inc a1h
rts4b:          rts                 ; return carry bit
charout:        ldy cv              ; cursor y index to y register
                sty termcy          ; set terminal cursor y
                ldy ch              ; cursor h index to y register
                sty termcx          ; set terminal cursor x
                sta termout         ; output char
                rts
rdkey:          ldy #t_blnk        ; set screen to flash
                sty termesc
                lda termout         ; get char at cursor pos.
                jsr charout
                ldy #t_norm        ; set screen back to normal after
                sty termesc
                jmp (kswl)          ; go to user key-in
keyin:          inc rndl
                bne keyin2          ; inc random number
                inc rndh
keyin2:         lda kbd
                beq keyin           ; busy waiting loop until key != 0...
                jsr charout         ; replace flashing screen
                bit kbdstrb         ; clear key strobe
                rts
esc:            jsr rdkey           ; get keycode
                jsr esc1            ;  handle esc function
rdchar:         jsr rdkey           ; read key
                cmp #k_esc            ; ESC?
                beq esc             ;   yes, don't return
                rts
notcr:          lda invflg
                pha
                lda #t_norm
                sta invflg          ; echo user line
                lda in,x            ;   non inverse
                jsr cout
                pla
                sta invflg
                lda in,x
                cmp #k_bs           ; check for edit keys
                beq bckspc          ;  bs, ctrl-x
                cmp #k_ctl_x
                beq cancel
                cpx #$F8            ; margin?
                bcc notcr1
                jsr bell            ; yes, sound bell
notcr1:         inx                 ; advance input index
                bne nxtchar
cancel:         lda #'\\'          ; backslash after cancelled LTN?
                jsr cout
getlnz:         jsr crout           ; output cr
getln:          jmp (rdline)
getlnw:         lda prompt
                jsr cout            ; output prompt char
                ldx #$01            ; init input index
bckspc:         txa                 ;   will backspace to u
                beq getlnz
                dex
nxtchar:        jsr rdchar
                cmp #k_ctl_u           ; use screen char
                bne captst          ;  for ctrl-u
                lda termout         ; get char under cursor
captst:         cmp #$60
                bcc addinp          ; convert to caps; for now..
                and #$DF
addinp:         sta in,x            ; add to input buffer
                cmp #k_entr
                bne notcr
                jsr clreol          ; clear to eol if cr

crout:          lda #k_entr
                bne cout            ; branches always ?!?
pra1:           ldy a1h             ; print CR,A1 in hex
                ldx a1l
                ;
                ; print address header at start of line
                ; yyxx-
                ;
pryx2:          jsr crout
                jsr prntyx
                ldy #$00
                lda #'-'            ; print '-'
                jmp cout
                ;
                ; Examine 8 bytes at address
                ; Triggered by command xxxx.yyyy
                ;
xam8:           lda a1l
                ora #$07            ; set to finish at
                sta a2l             ;   mod 8=7
                lda a1h
                sta a2h
mod8chk:        lda a1l
                and #$07
                bne dataout
                ;
                ; Examine byte at address
                ;
xam:            jsr pra1
dataout:        lda #' '
                jsr cout            ; output blank
                lda (a1l),y
                jsr prbyte          ; print byte in hex
                jsr nxta1
                bcc mod8chk         ; check if time to,
                rts                 ;  print address
xampm:          lsr                 ; determine if mon
                bcc xam             ;  mode is xam
                lsr                 ;  add or sub
                lsr
                lda a2l
                bcc add
                eor #$ff            ; sub - form 2's complement
add:            adc a1l
                pha
                lda #'='            ; print '=' the result
                jsr cout
                pla
                ;
                ; print byte as 2 hex digits
                ;
prbyte:         pha                 ; save a for second half
                lsr                 ; shift 4 high digits into low nibble, destroys accu
                lsr
                lsr
                lsr
                jsr prhexz          ; first hex per subroutine,
                pla                 ;   second hex direct, reuse rts for prbyte
prhex:          and #$0F            ; print hex digit in accu
                ;
                ; print lower accu nibble as hex number
                ;
prhexz:         ora #$30            ;   lsb's
                cmp #$3A
                bcc cout
                adc #$06            ; afterwards, flow into cout
                ;
                ; Character out to screen
                ; go through cswl zero page vector
                ;
cout:           jmp (cswl)          ; vector to user output routine
                ;
                ; Standard cswl character out routine
                ;
cout1:          phy                 ; save the y register
                pha                 ; save A
                jsr vidout          ; output a as ascii
                pla                 ; restore A
                ply                 ; restore y and return
                rts
                ;
                ; on blank / return, execute preceding command
                ;
bli:            dec ysav
                beq xam8
blank:          dex                 ; blank to mon
                bne setmdz          ; after blank
                cmp #':'            ; data store mode?
                bne xampm           ;   no, xam, add or sub
stor:           sta mode            ; keep in store mode
                lda a2l
                sta (a3l),y         ; store as low byte as (a3)
                inc a3l
                bne rts5            ; incr a3, return
                inc a3h
rts5:           rts
                ;
                ; set the command mode
                ;
setmode:        ldy ysav            ; save converted colon, '+',
                lda in-1,y          ;  '-', '.' as mode.
setmdz:         sta mode
                rts
                ;
                ; '<' command for move / verify
                ; initialize zero page variables for following V/M command
                ;
lt:             ldx #$01
lt2:            lda a2l,x           ; copy a2 (2 bytes) to
                sta a4l,x           ;   a4 and a5
                sta a5l,x
                dex
                bpl lt2
                rts
                ;
                ; copy memory range
                ;
move:           lda (a1l),y         ; move (a1 to a2) to
                sta (a4l),y         ;   (a4)
                jsr nxta4           ; y is set to 0 in zmode. there is no address
                bcc move            ;  mode for zero indirect without y, and move
                rts                 ;  must work with ranges > 255
                ;
                ; verify that two memory ranges have same content
                ;
vfy:            lda (a1l),y         ; verify (a1 to a2) with
                cmp (a4l),y         ;   (a4)
                beq vfyok
                jsr pra1
                lda (a1l),y
                jsr prbyte
                lda #' '            ; space
                jsr cout
                lda #'('            ; '('
                jsr cout
                lda (a4l),y
                jsr prbyte
                lda #')'            ; ')'
                jsr cout
vfyok:          jsr nxta4
                bcc vfy
                rts
                ;
                ; list memory range as assembler
                ;
list:           jsr a1pc            ; move a1 (2 bytes) to
                lda #$14            ;  pc if specified and
list2:          pha                 ;  dissemble 20 instructions
                jsr instdsp
                jsr pcadj           ; adjust pc each instruction
                sta pcl
                sty pch
                pla
                sec
                sbc #$01            ; next 20 instr.
                bne list2
                rts
a1pc:           txa                 ; if user specified address
                beq a1pcrts         ;   copy from a1 to pc
a1pclp:         lda a1l,X
                sta pcl,x
                dex
                bpl a1pclp
a1pcrts:        rts
                ;
                ; set inverse character mode
                ;
setinv:         ldy #t_inv            ; set for inverse video
                bne setiflg
                ;
                ; set norml character mode
                ;
setnorm:        ldy #t_norm            ; set for normal video
setiflg:        sty invflg
                rts
                ;
                ; Switch terminal configuration
                ; Terminal mode in $C01F is 0 when in screen mode,
                ; and 1 when in stdio mode
                ;

                ;
                ; set input port to standard keyboard
                ;
setkbd:         lda #$00            ; simulate port #0 input
inport:         sta a2l             ;   specified (keyin routine)
inprt:          ldx #kswl
                lda termmd          ; window mode - 0, stdio mode - 1
                bne @inprt3         ; stdio
                ldy #<keyin
                lda #>keyin
                sta a2h             ; default msb
                jmp ioprt
@inprt3:        ldy #<stdin         ; load stdout vector
                lda #>stdin         ; y lsb of cout1 standard vector
                sta a2h             ; default msb
                jmp ioprt

                ;
                ; set input port to standard keyboard
                ;
setrdl:         lda #$00            ; simulate port #0 input
rdport:         sta a2l             ;   specified (keyin routine)
rdprt:          ldx #rdline
                lda termmd          ; window mode - 0, stdio mode - 1
                bne @rdprt3          ; stdio
                ldy #<getlnw
                lda #>getlnw
                sta a2h             ; default msb
                jmp ioprt
@rdprt3:        ldy #<stdrdln       ; load stdout vector
                lda #>stdrdln       ; y lsb of cout1 standard vector
                sta a2h             ; default msb
                jmp ioprt

                ;
                ; Set output port to standard terminal
                ;
setvid:         lda #$00            ; simulate port #0 output
outport:        sta a2l             ;   specified (cout routine)
outprt:         ldx #cswl           ; x has zero page location for cout vector
                lda termmd          ; window mode - 0, stdio mode - 1
                bne @outprt3         ; stdio
                ldy #<cout1
                lda #>cout1
                sta a2h             ; default msb
                jmp ioprt
@outprt3:       ldy #<stdout         ; load stdout vector
                lda #>stdout         ; y lsb of cout1 standard vector
                sta a2h             ; default msb

                ;
                ; set either internal or peripheral port
                ; a2l contains a port number 0-7
                ; a2l = 0 - use internal port
                ; a2l > 0 - It is a peripheral, the msb of the address is
                ; 0xCn where n is peripheral #1-7
                ;
ioprt:          lda a2l             ; set ram in/out vectors
                and #$0F
                beq ioprt1
                ora #>ioadr         ; high byte
                ldy #$00
                beq ioprt2
ioprt1:         lda a2h
ioprt2:         sty loc0,x
                sta loc1,x
                rts
                ;
                ; execute installed language
                ;
x_lang:         jmp (lang)
                ; soft entry vector for installed language
bascont:        jmp (lang2)
go:             jsr a1pc            ; adr to pc if specified
                jsr restore         ; restore meta registers
                jmp (pcl)           ; go to user subroutine
regz:           jmp regdsp          ; jump to register display
                ;
                ; trace command
                ;
trace:          dec ysav
stepz:          jsr a1pc            ; adr to pc if specified
                jmp step            ; take one step
                ;
                ; execute user command
                ; 
usr:            jmp usradr          ; to usr subroutine at usradr
                ;
                ; write memory range to tape OUT
                ;
                ; "Tape" now writes the content directly into the tape file.
                ; Without cassette I/O ports writing square waves no longer
                ; makes sense.
                ;
write:          lda (a1l)
                sta tapeio
                jsr nxta1
                bcc write
                lda tapecls         ; close tape file
                jmp bell            ; sound bell and return

                ;
                ; execute command line
                ;
crmon:          jsr bli             ; handle CR as blank
                pla                 ;  then pop stack
                pla                 ; and return to mon
                bne monz
                ;
                ; read memory area from tape file
                ;
                ; This now just needs to read the content
                ; as bytes without complications.
                ;
read:           lda tapeio          ; read a byte
                sta (a1l)           ; store at (a1)
                jsr nxta1           ; incr a1, compare to a2
                bcc read            ; loop until done
                ldy tapecls         ; close tape
                jmp bell            ; good, sound bell and return

                ;
                ; print "ERR" and beep
                ;
prerr:          lda #$C5            ; 'E'
                jsr cout            ; print "ERR" then bell
                lda #$d2            ; 'R'
                jsr cout
                jsr cout
                ;
                ; ring the terminal bell
                ;
bell:           lda #k_ctl_g        ; output bell and return
                jmp cout            ; reuse cout rts directly here...
                ;
                ; restore registers from zero page storage
                ;
restore:        lda status          ; restore 6502 register contents
                pha                 ;  used by debug software
                lda acc
restr1:         ldx xreg
                ldy yreg
                plp
                rts
                ;
                ; save 6502 reg contents to zero page storage
                ;
save:           sta acc             
sav1:           stx xreg
                sty yreg
                php
                pla
                sta status
                tsx
                stx spnt
                cld
                rts
                ;
                ; 6502 reset vector
                ;
reset:          jsr setnorm
                jsr init
                jsr setvid
                jsr setkbd
                jsr setrdl
                ;
                ; Monitor entry point
                ;
mon:            cld
                jsr bell
monz:           lda #'*'            ; Monitor prompt
                sta prompt
                jsr getlnz          ; get line
                jsr zmode           ; clear monitor mode, scan idx
                ;
                ; get the next item from input line
                ;
nxtitm:         jsr getnum          ; get item, non-hex
                sty ysav
                ldy #$19 ; x-reg=0 if no hex input
                ;
                ; look up command subroutine for current character
                ;
chrsrch:        dey
                bmi mon             ; not found, go to mon
                cmp chrtbl,y        ; find cmnd char in table
                bne chrsrch
                jsr tosub           ; found call corresponding subroutine
                ldy ysav
                jmp nxtitm
                ;
                ; save one digit from input
                ; input:
                ;    a - 0x00 - 0x0F
                ;
dig:            ldx #$03
                asl
                asl                 ; shift hex digit to upper nibble
                asl
                asl                 ; now it is 0x00-0xF0
nxtbit:         asl                 ; transport the high bit to carry
                rol a2l             ; rotate carry into low byte of a2,
                rol a2h             ;   carry from a2l to a2h
                dex                 ; repeat 4 times until new digit shifted into lowest nibble of a2
                bpl nxtbit
nxtbas:         lda mode            ; x is $FF here
                bne nxtbs2          ;   if mode is zero
                lda a2h,x           ;    then copy a2 to
                sta a1h,x           ;     a1 and a3
                sta a3h,x
nxtbs2:         inx                 ;   repeat once for l,h pair
                beq nxtbas
                bne nxtchr
                ;
                ; getnum read command
                ; input: y - pointer to current input char
                ; result:
                ;   a - mode (non-hex char after number)
                ;   y - pointer to next char
                ;   (a2) - number found or 0x0000
                ;
getnum:         ldx #$00
                stx a2h             ;  clear A2
                stx a2l
nxtchr:         lda in,y            ; get char
                iny
                eor #$30            ; $30 0 -> $00; $39 9 -> $09; $41 A -> $71; $20 -> $10...
                cmp #$0A
                bcc dig             ; if hex digit
                adc #$88            ; c=1, $76 F + $88 + 1 = $FF; $71 A + $88 + 1 = $FA
                cmp #$FA
                bcs dig             ; if not a digit a now has ch eor #$30 + $B9
                rts
                ;
                ; Go to command routine
                ;
tosub:          tya
                asl                 ; subroutine table has 16 bit addresses now.
                tay                 ; to much fumbling making them all stay on page $FE
                lda subtbl,y        ; Push high order subroutine address on stack
                iny
                pha
                lda subtbl,y        ; Push low order subroutine address on stack
                pha
                lda mode            ;  old mode to A
                ;
                ; clear the y flag and the current mode for next subsequence
                ;
zmode:          ldy #$00            ; clear y
                sty mode            ; clear mode
                rts                 ; go to command subroutine previously pushed on the stack
;
; macros for command line parser used in mon and asm
;
#define coded(ch) (((ch ^ $30) + $89) & $0FF)
#define vector(addr) .byte >addr,<addr
chrtbl:         .byte coded('!')
                .byte coded('Q')
                .byte coded(k_ctl_c)
                .byte coded(k_ctl_y)
                .byte coded(k_ctl_e)
                .byte coded('T')
                .byte coded('V')
                .byte coded(k_ctl_k)
                .byte coded('S')
                .byte coded(k_ctl_p)
                .byte coded(k_ctl_b)
                .byte coded('-')
                .byte coded('+')
                .byte coded('M')
                .byte coded('<')
                .byte coded('N')
                .byte coded('I')
                .byte coded('L')
                .byte coded('W')
                .byte coded('G')
                .byte coded('R')
                .byte coded(':')
                .byte coded('.')
                .byte coded(k_entr)
                .byte coded(' ')
; Monitor commands:
; '!'           enter mini assembler
; 'Q'           terminate emulator by jumping to $FFFF. When PC=$FFFF, the cpu loop terminates.
; ctrl-c        bascont
; ctrl-y        usr
; ctrl-e        regz
; 'T'           trace
; 'V'           vfy
; ctrl-k        inprt
; 'S'           stepz
; ctrl-p        outprt
; ctrl-b        x_lang
; '-'           setmode
; '+'           setmode
; 'M'           move
; '<'           lt
; 'N'           setnorm
; 'I'           setinv
; 'L'           list
; 'W'           write
; 'G'           go
; 'R'           read
; colon         write area addr1,addr2
; '.'           hexdump addr1,addr2
; enter         execute monitor command line
; ' '           add preceding command to line
                ;
                ; table must have msb first lsb second
                ;
subtbl:         vector(asm_entry)     ; ! - Enter assembler
                vector(exit)          ; Q - Quit emulator
                vector(bascont)       ; CTL-C - exit moitor to installed language
                vector(usr)           ; CTL-Y - execute user command at vector %3F8
                vector(regz)          ; CTL-E - examine registers
                vector(trace)         ; "T" - Trace until reset or brk
                vector(vfy)           ; "V" - Verify memory range
                vector(inprt)         ; [0-7] CTL-K - input from keyboard (0) or peripheral card 1-7
                vector(stepz)         ; "S" - Step
                vector(outprt)        ; [0-7] CTL-P - send output to video (0) or printer peripheral 1-7
                vector(x_lang)        ; CTL-B -
                vector(setmode)       ; "-" - hexadecimal substraction
                vector(setmode)       ; "+" - hexadecimal addition
                vector(move)          ; "M" - mpve memory range
                vector(lt)            ; "< aaaa.bbbb M|V" - Move / compare memory range
                vector(setnorm)       ; "N" - set to normal output
                vector(setinv)        ; "I" - set to inverse output
                vector(list)          ; "L" - list preceding memory add
                vector(write)         ; "aaaa.bbbb W" - Save memory range on tape
                vector(go)            ; "aaaa G" - Run program as subroutine at aaaa
                vector(read)          ; "aaaa.bbbb R" - Read memory range from tape
                vector(setmode)       ; colon xx yy... - change current location. to given ehx bytes
                vector(setmode)       ; ".yyyy" - examine bytes between after current position and yyyy
                vector(crmon)         ; enter - starts evaluation of commands in input line
                vector(blank)         ; space separator
                ;
                ; 6502 fixed system vectors. Must start at 0xFFFA
                ; .org not working here, adjusting with
                ; .dsb block "keep_aligned" above
                ;
sysvec_nmi:     .word   nmiloc      ; nmi vector set up in RAM by reset routine
sysvec_reset:   .word   reset       ; reset vector
sysvec_irq:     .word   irqvec      ; irq vector in rom
