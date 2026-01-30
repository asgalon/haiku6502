;
; haiku6502 ROM
;
; Annotated and modified emulator ROM using orig_monitor_rom.s as a template.
; This code is only for academic purposes.
; This file is compatible with xa assembler and is not relocatable.
; Care has to be taken to keep the "go" jump table addresses area on page 0xFE or else it will cease to work this way.
; There are a few nifty tricks in the original assembler code to save a few bytes here and there that probably
; would not have been done this way were there more than 12KB address space available for ROM.
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
; - Tape in/out goes to a tape file. with the right sampling rate, it should be possible to read in data tapes.
;   not much use though, since the rom routine addresses are now no longer compatible.
; - The screen size is taken from the terminal dimensions. Size is no longer restricted to 1KB text pages.
; - Lo-Res and Hi-Res graphics modes are gone for now since they don't work too well with the standard ncurses based
;   terminal peripheral. A graphics terminal peripheral could be added, though.
; - The peripheral system is not completed, especially emulator interrupt management is rudimentary. So peripheral
;   rom areas can be addressed, but the extension rom area 0xC800-0xC8FF needs work. This does not ffect this rom,
;   though...
;
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
;               0xD000 - 0xF800 Reserved for language modules
;               0xF800 - 0xFFFF System Monitor ROM
;               0xFFFA - 0xFFFF Hardwired 6502 NMI, Reset and IRQ vectors, have to be kept at fixed addresses.
; On Reset, the program counter is loaded from 0xFFFC and 0xFFFD. all addresses with least
; significant (lsb,low) byte first.
;
; TODO Fix remaining bugs, cleanup code
;

                .include "symbols.inc"
                .org    $D000       ; ROM start address
                .dsb $1000,$00      ; first filler, 4KB low ROM
lang:           jsr lang_init
lang2:          jmp lang_cont
lang_init:      rts
lang_cont:      .dsb $1800,$00    ; filler up to F800
                ;
                ;  SPACE FREE in last 4k
                ;
keep_aligned:
                .dsb $A8            ; blank must be on page $FE for jump table to work

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
                lsr
                lsr
                lsr                 ; shift high nibble into low nibble
                lsr
rtmaskz:        and #$0F            ; mask lower 4 bits
                rts
insds1:         ldx pcl             ; print pcl,h
                ldy pch
                jsr pryx2
                jsr prblnk          ; followed by a blank
                lda (pcl,x)
insds2:         tay
                lsr                 ; even / odd test
                bcc ieven
                ror                 ; bit 1 test
                bcs err             ; xxxxxx11 invalid opcode
                cmp #$A2
                beq err             ; opcode 89 invalid
                and #$87            ; mask bits
ieven:          lsr                 ; lsb into carry for l/r test
                tax
                lda fmt1,x          ; get format index byte
                jsr selnibl         ; r/l h-byte on carry
                bne getfmt
err:            ldy #$80            ; substitute $80 for invalid ops
                lda #$0             ; set print format index to 0
getfmt:         tax
                lda fmt2,x          ; index into print format table
                sta format          ; save for addr field formatting
                and #$03            ; mask for 2-bit length
                                    ; (P=1 byte, 1=2 byte, 2=3 byte)
                sta length
                tya                 ; opcode
                and #$8F            ; mask for 1xxx1010 test
                tax                 ;  save it
                tya                 ; opcode to a again
                ldy #$03
                cpx #$8A
                beq mnndx3
mnndx1:         lsr
                bcc mnndx3          ; form index into mnemonic tble
                lsr
mnndx2:         lsr                 ; 1)  1xxx1010 => 00101xxx
                ora #$20            ; 2)  xxxyyy01 => 00111xxx
                dey                 ; 3)  xxxyyy10 => 00110xxx
                bne mnndx2          ; 4)  xxxyy100 => 00100xxx
                iny                 ; 5)  xxxxx000 => 000xxxxx
mnndx3:         dey
                bne mnndx1
                rts
                .byte $FF,$FF,$FF   ; ???
instdsp:        jsr insds1          ; gen fmt, len bytes
                pha                 ; save mnemonic table index
prntop:         lda (pcl),y
                jsr prbyte
                ldx #$01            ; print 2 blanks
prntbl:         jsr prbl2
                cpy length          ; print inst (1-3 bytes)
                iny                 ; in a 12 char field
                bcc prntop
                ldx #$03            ; char count for mnemonic print
                cpy #$04
                bcc prntbl
                pla                 ; recover mnemonic index
                tay
                lda mneml,y
                sta lmnem           ; fetch 3 char mnemonic
                lda mnemr,y         ;   (packed in 2 bytes, only chars A-Z
                sta rmnem
prmn1:          lda #$00
                ldy #$05
prnm2:          asl rmnem           ; shift 5 bits of
                rol lmnem           ;   character into Accu
                rol                 ;      (clears carry)
                dey
                bne prnm2
                adc #'?'            ; add "?" offset
                jsr cout            ; output a char of mnem
                dex
                bne prmn1
                jsr prblnk          ; output 3 blanks
                ldy length
                ldx #$06            ; count for 6 format bits
pradr1:         cpx #$03
                beq pradr5          ; if x=3 then addr
pradr2:         asl format
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
                cmp #$E8            ; handle relative address mode
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
pcadj3:         ldy pch
                tax                 ; test displacement sign
                bpl pcadj4          ;   (for rel branch)
                dey
pcadj4:         adc pcl
                bcc rts2            ; pcl+LENGTH(or Displc.)+1 to A
                iny                 ;   carry inot y (pch)
rts2:           rts
;
; fmt1 bytes:   XXXXXXY0 instructions
;       if Y=0: then left half byte
;       if Y=1  then right half byte
;                    (X=index)
fmt1:           .byte $04,$20,$54,$30,$0D
                .byte $80,$04,$90,$03,$22
                .byte $54,$33,$0D,$80,$04
                .byte $90,$04,$20,$54,$33
                .byte $0D,$80,$04,$90,$04
                .byte $20,$54,$3B,$0D,$80
                .byte $04,$90,$00,$22,$44
                .byte $33,$0D,$C8,$44,$00
                .byte $11,$22,$44,$33,$0D
                .byte $C8,$44,$A9,$01,$22
                .byte $44,$33,$0D,$80,$04
                .byte $90,$01,$22,$44,$33
                .byte $0D,$80,$04,$90
                .byte $26,$31,$87,$9A       ; ZZXXXXY01 instructions
fmt2:           .byte $00       ; ERR
                .byte $21       ; IMM
                .byte $81       ; Z-PAGE
                .byte $82       ; ABS
                .byte $00       ; IMPLIED
                .byte $00       ; ACCUMULATOR
                .byte $59       ; (ZPAG,X)
                .byte $4D       ; (ZPAG),Y
                .byte $91       ; ZPAG,X
                .byte $92       ; ABS,X
                .byte $86       ; ABS,Y
                .byte $4A       ; (ABS)
                .byte $85       ; ZPAG,Y
                .byte $9D       ; RELATIVE
char1:          .byte ',', ')', ',', '#', '(', '$'
char2:          .byte 'Y',0,"X$$",0
;
; mneml is of form:
; (A) XXXXX000
; (B) XXXYY100
; (C) 1XXX1010
; (D) XXXYYY10
; (E) XXXYYY01
;     (X=index)
mneml:          .byte $1C,$8A,$1C,$23,$5D,$8B
                .byte $1B,$A1,$9D,$8A,$1D,$23
                .byte $9D,$8B,$1D,$A1,$00,$29
                .byte $19,$AE,$69,$A8,$19,$23
                .byte $24,$53,$1B,$23,$24,$53
                .byte $19,$A1                   ; (A) Format Above
                .byte $00,$1A,$5B,$5B,$A5,$69
                .byte $24,$24                   ; (B) Format
                .byte $AE,$AE,$A8,$AD,$29,$00
                .byte $7C,$00                   ; (C) Format
                .byte $15,$9C,$6D,$9C,$A5,$69
                .byte $29,$53                   ; (D) Format
                .byte $84,$13,$34,$11,$A5,$69
                .byte $23,$A0                   ; (E) Format
mnemr:          .byte $D8,$62,$5A,$48,$26,$62
                .byte $94,$88,$54,$44,$C8,$54
                .byte $68,$44,$E8,$94,$00,$B4
                .byte $08,$84,$74,$B4,$28,$6E
                .byte $74,$F4,$CC,$4A,$72,$F2
                .byte $A4,$8A                   ; (A) Format
                .byte $00,$AA,$A2,$A2,$74,$74
                .byte $74,$72                   ; (B) Format
                .byte $44,$68,$B2,$32,$B2,$00
                .byte $22,$00                   ; (C) Format
                .byte $1A,$1A,$26,$26,$72,$72
                .byte $88,$C8                   ; (D) Format
                .byte $C4,$CA,$26,$48,$44,$44
                .byte $A2,$C8                   ; (E) Format
                .byte $FF,$FF,$FF
;
; monitor stepping
; this is nice... it emulates itself to execute a program stepwise
;
step:           jsr instdsp         ; disassemble one instruction
                pla                 ;   at (pcl,h)
                sta rtnl            ; adjust to user
                pla                 ;   stack, save
                sta rtnh            ;   return address
                ldx #$08
xqinit:         lda initbl-1,x      ; init xeq (execute) area
                sta xqt,x
                dex
                bne xqinit
                lda (pcl,x)         ; user opcode byte
                beq xbrk            ; special if BRK
                ldy length          ; LEN from disassembly
                cmp #$20
                beq xjsr            ; handle jsr, rts, jmp,
                cmp #$60            ;   jmp (), rti special
                beq xrts
                cmp #$4C
                beq xjmp
                cmp #$6C
                beq xjmpat
                cmp #$40
                beq xrti
                and #$1F            ; TODO comment
                eor #$14
                cmp #$04            ; copy user instruction to xeq area
                beq xq2             ;   with trailing nops
xq1:            lda (pcl),y         ; change rel branch
xq2:            sta xqtnz,y         ;   disp to 4 for
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
                clc                 ; short unconditional jump
                bcc newpcl          ;  CLC BCC #rel
xjsr:           clc
                jsr pcadj2          ; update pc and push
                tax                 ;   onto stack for
                tya                 ;   JSR simulate
                pha
                txa
                pha
                ldy #$02
xjmp:           clc
xjmpat:         lda (pcl),Y
                tax                 ; load pc for jump,
                dey                 ;   (JMP) simulate
                lda (pcl),y
                stx pch
newpcl:         sta pcl
                bcs xjmp
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
                ldy #$01            ;  add LEN+2 to pc
                lda (pcl),y
                jsr pcadj3
                sta pcl
                tya
                sec
                bcs pcinc2
nbranch:        jsr save            ; normal return after
                sec                 ;   xeq user of
                bcs pcinc3          ; go update pc
initbl:         nop
                nop                 ; dummy fill for
                jmp nbranch         ;   xeq area
                jmp branch
rtbl:           .byte $C1           ; 'A'
                .byte $D8           ; 'X'
                .byte $D9           ; 'Y'
                .byte $D0           ; 'P'
                .byte $D3           ; 'S'
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
init:           lda #$00            ; clr status for debug
                sta status          ;   software
settxt:         lda #$00            ;   full screen window
setwnd:         sta wndtop          ; set for 40 col window
                lda #$00            ;    top in accu,
                sta wndlft          ;    bottom at line 24
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
mul5:           ror acl,x           ; orig DFB #$76, #$50 ?!? maybe a bug in their assembler software
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
                sty sign            ;   with result sign
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
                .dsb $10,$EA
rts2b:          rts
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
                ldy #$00
                sty ch              ; then clear to end of page
                beq cleop1
cr:             lda #$00            ; cursor to left of index
                sta ch
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
nxta4:          inc a4l             ; incr 2-byte a4
                bne nxta1           ;  and a1
                inc a4h
nxta1:          lda a1l             ; incr 2-byte a1
                cmp a2l
                lda a1h             ;   and compare to a2
                sbc a2h
                inc a1l             ;  carry set if >=
                bne rts4b
                inc a1h
rts4b:          rts
headr:          ldy #$4B            ; write or skip A * 256 'long 1'
                jsr zerdly          ;   half cycles
                bne headr           ;     (650 usec each)
                adc #$fe
                bcs headr           ; then a 'short 0'
                ldy #$21            ;    (400 usec)
wrbit:          jsr zerdly          ; write two half cycles
                iny                 ;   of 250 usec ('0')
                iny                 ;   or 500 usec ('0')
zerdly:         dey                 ; it means 'zero delay'
                bne zerdly
                bcc wrtape          ; y is count for
                ldy #$32            ;   timing loop
onedly:         dey                 ; 'ones delay'
                bne onedly
wrtape:         pha
                ldy tpdir
                lda tapeout,y         ; y=0 - tape out, y=1 - tape in
                pla
                ldy #$2C
                dex
                rts
rdbyte:         ldx #$08            ; 8 bits to read
rdbyt2:         pha                 ; read two transitions
                jsr rd2bit          ;   (find edge)
                pla
                rol                 ; next bit
                ldy #$3A            ; count for samples
                dex
                bne rdbyt2
                rts
rd2bit:         jsr rdbit
rdbit:          dey                 ; decr y until
                lda tapein          ;   tape transition
                eor lastin
                bpl rdbit
                eor lastin
                sta lastin
                cpy #$80            ; set carry on y register
                rts
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
getln:          lda prompt
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
cout1:          sty ysav1           ; save the y register
                pha                 ; save A
                jsr vidout          ; output a as ascii
                pla                 ; restore A
                ldy ysav1           ; restore y and return
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
                ; set input port to standard keyboard
                ;
setkbd:         lda #$00            ; simulate port #0 input
inport:         sta a2l             ;   specified (keyin routine)
inprt:          ldx #kswl
                ldy #<keyin         ; see if it is the default vector
                bne ioprt
                ;
                ; Set output port to standard terminal
                ;
setvid:         lda #$00            ; simulate port #0 output
outport:        sta a2l             ;   specified (cout routine)
outprt:         ldx #cswl
                ldy #<cout1         ; compare with address of default vector
ioprt:          lda a2l             ; set ram in/out vectors
                and #$0F
                beq ioprt1
                ora #>ioadr         ; high byte
                ldy #$00
                beq ioprt2
ioprt1:         lda #>cout1
ioprt2:         sty loc0,x
                sta loc1,x
                rts
                ;
                ; execute installed language
                ;
x_lang:         jmp lang   
                ; soft entry vector for installed language
bascont:        jmp lang2
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
write:          lda #$00           ; set header to WRITE
                sta tpdir
                lda #$40
                jsr headr           ; write 10-sec header
                ldy #$27
wr1:            ldx #$00
                eor(a1l,x)
                pha
                lda (a1l,x)
                jsr wrbyte
                jsr nxta1
                ldy #$1D
                pla
                bcc wr1
                ldy #$22
                jsr wrbyte
                lda tapecls         ; close tape
                beq bell            ; sound bell and return
wrbyte:         ldx #$10
wrbyt2:         asl
                jsr wrbit
                bne wrbyt2
                rts
                ;
                ; execute command line
                ;
crmon:          jsr bli             ; handle CR as blank
                pla                 ;  then pop stack
                pla                 ; and return to mon
                bne monz
                ;
                ; read memory area from tape IN
                ;
                ; first synchronize timing with the header block,
                ; Then read bytes and check checksum
                ; 1s and 0s have different lengths, so number and frequency 
                ; of machine cycles is important for executing this
                ;
read:           lda #$01            ; set header to READ
                sta tpdir
                jsr rd2bit          ; find tapein edge
                lda #$16
                jsr headr           ; delay 3.5s
                sta chksum          ; init checksum = $FF
                jsr rd2bit          ; find tapein edge
rd2:            ldy #$24            ; look for sync bit
                jsr rdbit           ; (short 0)
                bcs rd2             ; loop until found
                jsr rdbit           ; skip second sync h-cycle
                ldy #$3B            ; index for 0/1 test
rd3:            jsr rdbyte          ; read a byte
                sta (a1l,x)         ; store at (a1)
                eor chksum
                sta chksum          ; update running checksum
                jsr nxta1           ; incr a1, compare to a2
                ldy #$35            ; compensate 0/1 index
                bcc rd3             ; loop until done
                jsr rdbyte          ; read chksum byte
                ldy tapecls         ; close tape
                cmp chksum
                beq bell            ; good, sound bell and return
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
                ldy #$17            ; x-reg=0 if no hex input
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
                ; get one hex digit from input
                ;
dig:            ldx #$03
                asl
                asl                 ; got hex digit in low nibble, shift into A2
                asl
                asl                 ; first, position in high nibble of accu
nxtbit:         asl                 ; transport a high bit to carry
                rol a2l             ; rotate carry into low byte into carry
                rol a2h             ; rotate carry into high byte
                dex                 ; leave x = $FF if digit
                bpl nxtbit
nxtbas:         lda mode
                bne nxtbs2          ;   if mode is zero
                lda a2h,x           ;    then copy a2 to
                sta a1h,x           ;     a1 and a3
                sta a3h,x
nxtbs2:         inx
                beq nxtbas
                bne nxtchr
                ;
                ; getnum read command
                ; result:
                ;   a - mode
                ;   x - number of numeric args
                ;
getnum:         ldx #$00            ;  clear A2
                stx a2h
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
tosub:          lda #>go            ; Push high order subroutine address on stack
                pha
                lda subtbl,y        ; Push low order subroutine address on stack
                pha
                lda mode            ;  old mode to A
                ;
                ; 
zmode:          ldy #$00            ; clear y
                sty mode            ; clear mode
                rts                 ; go to command subroutine previously pushed on the stack
#define F(ch) (((ch ^ $30) + $89) & $0FF)
chrtbl:         .byte F(k_ctl_c)
                .byte F(k_ctl_y)
                .byte F(k_ctl_e)
                .byte F('T')
                .byte F('V')
                .byte F(k_ctl_k)
                .byte F('S')
                .byte F(k_ctl_p)
                .byte F(k_ctl_b)
                .byte F('-')
                .byte F('+')
                .byte F('M')
                .byte F('<')
                .byte F('N')
                .byte F('I')
                .byte F('L')
                .byte F('W')
                .byte F('G')
                .byte F('R')
                .byte F(':')
                .byte F('.')
                .byte F(k_entr)
                .byte F(' ')
; Monitor commands:
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

subtbl:         .byt <bascont       ; CTL-C - exit moitor to installed language
                .byt <usr           ; CTL-Y - execute user command at vector %3F8
                .byt <regz          ; CTL-E - examine registers
                .byt <trace         ; "T" - Trace until reset or brk
                .byt <vfy           ; "V" - Verify memory range
                .byt <inprt         ; [0-7] CTL-K - input from keyboard (0) or peripheral card 1-7
                .byt <stepz         ; "S" - Step
                .byt <outprt        ; [0-7] CTL-P - send output to video (0) or printer peripheral 1-7
                .byt <x_lang        ; CTL-B -
                .byt <setmode       ; "-" - hexadecimal substraction
                .byt <setmode       ; "+" - hexadecimal addition
                .byt <move          ; "M" - mpve memory range
                .byt <lt            ; "< aaaa.bbbb M|V" - Move / compare memory range
                .byt <setnorm       ; "N" - set to normal output
                .byt <setinv        ; "I" - set to inverse output
                .byt <list          ; "L" - list preceding memory add
                .byt <write         ; "aaaa.bbbb W" - Save memory range on tape
                .byt <go            ; "aaaa G" - Run program as subroutine at aaaa
                .byt <read          ; "aaaa.bbbb R" - Read memory range from tape
                .byt <setmode       ; colon xx yy... - change current location. to given ehx bytes
                .byt <setmode       ; ".yyyy" - examine bytes between after current position and yyyy
                .byt <crmon         ; enter - starts evaluation of commands in input line
                .byt <blank         ; space separator
                ;
                ; 6502 fixed system vectors. Must start at 0xFFFA
                ; .org not working here, adjusting with
                ; .dsb block "keep_aligned" above
                ;
sysvec_nmi:     .word   nmiloc      ; nmi vector set up in RAM by reset routine
sysvec_reset:   .word   reset       ; reset vector
sysvec_irq:     .word   irqvec      ; irq vector in rom

