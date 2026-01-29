;
; Apple II Monitor ROM
;
; reconstructed from
;
; Apple ][ Reference Manual (1979), by Christopher Espinosa, Apple Computer Inc,
;                                   "Monitor ROM Listing", pp. 155-171.
;                                   Reorder Apple product number A2L0001A (030-0004-01)
;
;
;
; Warning: This is a transcript for academic purposes only
; from the listing that apparently stems from a 9 needle dot matrix printer
; and also had a fixed column width for the assembler text which
; cuts off long DFB lines at the start of the comment column.
; There also may be a few reading errors and typos introduced with the transcription.
; it goes through the xa assembler without errors, but
; that does not mean it is completely correct.
; this only contains the published top 2K of the ROM.
; but not the BASIC interpreter that is located below.
; Who wants BASIC anyway? It is and always has been an awful
; programming language more likely to teach beginners bad habits.
;
; ***************************
; ^                         *
; ^        APPLE II         *
; ^     SYSTEM MONITOR      *
; ^                         *
; ^    COPYRIGHT 1977 BY    *
; ^   APPLE COMPUTER, INC.  *
; ^                         *
; ^   ALL RIGHTS RESERVED   *
; ^                         *
; ^       S. WOZNIAK        *
; ^        A. BAUM          *
; ^                         *
; ***************************
;
; manuscript copy from the printed manual listing
; with some additional comments. tested with xa assembler
;
; Memory layout:
;
; 0x0000-0x00FF Zero page
; 0x0100-9x01FF Stack
; 0x0200-0x02FF text buffer
; 0x0300-0x03FF system vars & vectors
; 0x0400-0x0600 reserved for peripherals
; 0x0600-0xBFFF free RAM
; 0xC000-0xCFFF I/O
; 0xD000-0xFFFF ROM
;

                .include "orig_symbols.inc"
                .org    $F800       ; ROM start address
plot:           lsr                 ; y-coord/2
                php                 ; save lsb in carry on stack
                jsr gbascalc        ; calc base adr in gbasl,h  see comment for bascalc.
                plp                 ; restore lsb carry from stack
                lda #$0F            ; mask $0F if even
                bcc rtmask          ;    test with lsb still in carry
                adc #$E0            ; mask $F0 if odd
rtmask:         sta mask
plot1:          lda (gbasl),y       ; data
                eor color           ;   xor color
                and mask            ;     and mask
                eor (gbasl),y       ;       xor data
                sta (gbasl),y       ;         to data
                rts
hline:          jsr plot            ; plot square
hline1:         cpy h2              ; done?
                bcs rts1            ;   yes, return
                iny                 ;   no, incr x-coord index
                jsr plot1           ; plot next square
                bcc hline1          ; always taken
vlinez:         adc #$01            ; next y-coord
vline:          pha                 ;  save y-coord in a on stack
                jsr plot            ; plot square
                pla
                cmp v2              ; done?
                bcc vlinez          ;   no, loop
rts1:           rts
clrscr:         ldy #$2F            ; max y, full scrn clr
                bne clrsc2          ; always taken
clrtop:         ldy #$27             ; max y, top scrn clr
clrsc2:         sty v2              ; store as bottom coord for vline calls
                ldy #$27            ; rightmost x.coord (column)
clrsc3:         lda #$00            ; top coord for vline calls
                sta color           ; clear color (black)
                jsr vline           ; draw vline
                dey                 ; next leftmost x-coord
                bpl clrsc3          ; loop until done
                rts
gbascalc:       pha                 ; for input bits 000D EFGH  (compare with bascalc below)
                lsr
                and #$03
                ora #$04            ; generate gbash = 0000 01FG
                sta gbash
                pla                 ; and gbasl = HDED E000
                and #$18
                bcc gbcalc
                adc #$7F
gbcalc:         sta gbasl
                asl
                asl
                ora gbasl
                sta gbasl
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
scrn:           lsr                 ; read screen y-coord/2
                php                 ; save lsb in carry on stack
                jsr gbascalc        ; calc base address
                lda (gbasl),y       ; get byte
                plp                 ; restore lsb in carry
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
                adc #$BF            ; add "?" offset
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
prntyx:         tya
prntax:         jsr prbyte          ; output target adr
prntx:          txa                 ;    of branch and return
                jmp prbyte
prblnk:         ldx #$03            ; blank count
prbl2:          lda #$A0            ; load a space
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
char1:          .byte $AC,$A9,$AC,$A3,$A8,$A4   ; ",),#($"
char2:          .byte $D9,$00,$D8,$A4,$A4,$00   ; "Y",0,"X$$",0
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
rdsp1:          lda #$A0
                jsr cout            ; output space
                lda rtbl-$FB,x      ; register name (x has FB so base address = rtbl - FB)
                jsr cout
                lda #$BD            ; '='
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
                lda lores
                lda lowscr
settxt:         lda txtset          ; set for text mode
                lda #$00            ;   full screen window
                beq setwnd
setgr:          lda txtclr          ; set for graphics mode
                lda mixset          ;   lower 4 lines as
                jsr clrtop          ;   text window
                lda #$14
setwnd:         sta wndtop          ; set for 40 col window
                lda #$00            ;    top in accu,
                sta wndlft          ;    bottom at line 24
                lda #$28
                sta wndwdth
                lda #$18
                sta wndbtm          ;   vtab to row 23
                lda #$17
tabv:           sta cv              ; vtabs to row in accu
                jmp vtab
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
;
; bascalc is that complicated because on the apple 2, screen memory
; is organized in a way that makes it more efficient for the CRT beam sequence
; the beam does not go from top to bottom in sequence because that would make
; a typical 1970s phosphorus computer monitor flicker. Instead, it goes over
; each nth character row in the first pixel line, then each second character pixel line
; and so on. this way the pixels are activated equally over the whole area
; and through the afterglow period of the pixels flickering is reduced to a minimum.
; With modern monitor equipment all this does not make sense anymore, so it has to be replaced...
;
bascalc:        pha                 ; calc base address in basl,h
                lsr                 ;   for given binary line nr 000A BCDE. E->carry
                and #$03            ;   0 <= line <= $17 (23)
                ora #$04            ; arg = 000A BCDE, generate
                sta bash            ;   bash=0000 01CD
                pla                 ;    and basl=EABA 8000
                and #$18            ;  now we have 000A B000 for basl
                bcc bsclc2          ;  if carry set,
                adc #$7F            ;    add it as 0x80 -> high bit
bsclc2:         sta basl            ;   now we have E00A B000 in basl
                asl
                asl                 ;   now a has  0AB0 0000
                ora basl            ;   add in the rest already stored in basl
                sta basl            ; and we are done
                rts
bell1:          cmp #$87            ; bell char (ctrl-G)?
                bne rts2b           ;  no, return
                lda #$40
                jsr wait            ; delay .01 seconds on 1MHz clock
                ldy #$C0
bell2:          lda #$0C            ; toggle speaker at
                jsr wait            ;    1kHz for .1 sec
                lda spkr
                dey
                bne bell2
rts2b:          rts
stoadv:         ldy ch              ; cursor h index to y register
                sta (basl),y        ; stor char in line
advance:        inc ch              ; increment cursor h index
                lda ch              ;   (move right)
                cmp wndwdth         ; betond window width?
                bcs cr              ;   yes, cr to next line
rts3:           rts                 ; no, return
vidout:         cmp #$A0            ; control char?
                bcs stoadv          ;   no, output it
                tay                 ; inverse video?
                bpl stoadv          ;   yes, output it
                cmp #$8D            ; CR?  0x1D | 0x80
                beq cr              ;   yes
                cmp #$8A            ; LF?
                beq lf              ;   yes
                cmp #$88            ; backspace (CTRL-H)?
                bne bell1           ;   no, check for bell
bs:             dec ch              ; decrement cursor h index
                bpl rts3            ; if pos, ok, else move up
                lda wndwdth         ; set ch to wndwdth-1
                sta ch
                dec ch              ; rightmost screen position
up:             lda wndtop          ; cursor v index
                cmp cv
                bcs rts4            ; if top line then return
                dec cv              ; decr cursor v
vtab:           lda cv              ; get cursor v
vtabz:          jsr bascalc         ; generate base addr
                adc wndlft          ; add window left index
                sta basl            ; to basl
rts4:           rts
esc1:           eor #$C0            ; esc?
                beq home            ;   if so, do home and clear
                adc #$FD            ; esc-a or -b check
                bcc advance         ;   a, advance
                beq bs              ;   b, backspace
                adc #$FD            ; esc-c or -d check
                bcc lf              ;   c, down
                beq up              ;   d, go up
                adc #$FD            ; esc-e or -f check
                bcc clreol          ;   e, clear to end of line
                bne rts4            ;   not f, return
clreop:         ldy ch              ; cursor h to y
                lda cv              ; cursor v to a
cleop1:         pha                 ; save current line on stk
                jsr vtabz           ; calc base address
                jsr cleolz          ; clear to eol, set carry
                ldy #$00            ; clear from h index=0 for rest
                pla                 ; increment current line
                adc #$00            ; (carry is set)
                cmp wndbtm          ; done to bottom of window?
                bcc cleop1          ;   no, keep clearing lines
                bcs vtab            ;   yes, tab to current line
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
                bcc vtabz           ;   no, set base addr
                dec cv              ; decr cursor v (back to bottom)
scroll:         lda wndtop          ; start at top of scrl window
                pha
                jsr vtabz           ; generate base address
scrl1:          lda basl            ; copy basl,h
                sta bas2l           ;  to bas2l,h
                lda bash
                sta bas2h
                ldy wndwdth         ; init y to rightmost index
                dey                 ; of scrolling window
                pla
                adc #$01            ; incr line number
                cmp wndbtm          ; done?
                bcs scrl3           ;   yes, finish
                pha
                jsr vtabz           ; form basl,h (base addr)
scrl2:          lda (basl),y        ; move a char of line
                sta (bas2l),y
                dey                 ; next char of line
                bpl scrl2
                bmi scrl1           ; next line
scrl3:          ldy #$00            ; clear bottom line
                jsr cleolz          ; get base address for bottom line
                bcs vtab            ; carry is set
clreol:         ldy ch              ; cursor h index
cleolz:         lda #$A0            ; ' '
cleol2:         sta (basl),y        ; store blnks from 'here'
                iny                 ;   to end of lines (wndwdth)
                cpy wndwdth
                bcc cleol2
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
headr:          ldy #$4B            ; write A * 256 'long 1'
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
wrtape:         ldy tapeout         ; tape output toggle
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
rdkey:          ldy ch
                lda (basl),y        ; set screen to flash
                pha
                and #$3F
                ora #$40
                sta (basl),y
                pla
                jmp (kswl)          ; go to user key-in
keyin:          inc rndl
                bne keyin2          ; inc random number
                inc rndh
keyin2:         bit kbd
                bpl keyin           ; busy waiting loop...
                sta (basl),y        ; replace flashing screen
                lda kbd             ; get keycode
                bit kbdstrb         ; clear key strobe
                rts
esc:            jsr rdkey           ; get keycode
                jsr esc1            ;  handle esc function
rdchar:         jsr rdkey           ; read key
                cmp #$9B            ; ESC?
                beq esc             ;   yes, don't return
                rts
notcr:          lda invflg
                pha
                lda #$FF
                sta invflg          ; echo user line
                lda in,x            ;   non inverse
                jsr cout
                pla
                sta invflg
                lda in,x
                cmp #$88            ; check for edit keys
                beq bckspc          ;  bs, ctrl-x
                cmp #$98
                beq cancel
                cpx #$F8            ; margin?
                bcc notcr1
                jsr bell            ; yes, sound bell
notcr1:         inx                 ; advance input index
                bne nxtchar
cancel:         lda #$DC            ; backslash after cancelled LTN?
                jsr cout
getlnz:         jsr crout           ; output cr
getln:          lda prompt
                jsr cout            ; output prompt char
                ldx #$01            ; init input index
bckspc:         txa                 ;  will backspace to u
                beq getlnz
                dex
nxtchar:        jsr rdchar
                cmp #pick           ; use screen char
                bne captst          ;  for ctrl-u
                lda (basl),y
captst:         cmp #$E0
                bcc addinp          ; convert to caps
                and #$DF
addinp:         sta in,x            ; add to input buffer
                cmp #$8D
                bne notcr
                jsr clreol          ; clear to eol if cr
crout:          lda #$8D
                bne cout            ; branches always ?!?
pra1:           ldy a1h             ; print CR,A1 in hex
                ldx a1l
pryx2:          jsr crout
                jsr prntyx
                ldy #$00
                lda #$AD            ; print '-'
                jmp cout
xam8:           lda a1l
                ora #$07            ; set to finish at
                sta a2l             ;   mod 8=7
                lda a1h
                sta a2h
mod8chk:        lda a1l
                and #$07
                bne dataout
xam:            jsr pra1
dataout:        lda #$A0
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
                lda #$BD            ; print '=' the result
                jsr cout
                pla
prbyte:         pha                 ; print byte as 2 hex
                lsr                 ;   digits, destroys accu
                lsr
                lsr
                lsr
                jsr prhexz          ; first hex per subroutine,
                pla                 ;   second hex direct, reuse rts for prbyte
prhex:          and #$0F            ; print hex digit in accu
prhexz:         ora #$B0            ;   lsb's
                cmp #$BA
                bcc cout
                adc #$06
cout:           jmp (cswl)          ; vector to user output routine
cout1:          cmp #$A0
                bcc coutz           ; don't output ctrl's inverse
                and invflg          ; mask with inverse flag
coutz:          sty ysav1           ; sve y register
                pha                 ; save A
                jsr vidout          ; output a as ascii
                pla                 ; restore A
                ldy ysav1           ; restore y and return
                rts
bli:            dec ysav
                beq xam8
blank:          dex                 ; blank to mon
                bne setmdz          ; after blank
                cmp #$BA            ; data store mode?
                bne xampm           ;   no, xam, add or sub
stor:           sta mode            ; keep in store mode
                lda a2l
                sta (a3l),y         ; store as low byte as (a3)
                inc a3l
                bne rts5            ; incr a3, return
                inc a3h
rts5:           rts
setmode:        ldy ysav            ; save converted colon, '+',
                lda in-1,y          ;  '-', '.' as mode.
setmdz:         sta mode
                rts
lt:             ldx #$01
lt2:            lda a2l,x           ; copy a2 (2 bytes) to
                sta a4l,x           ;   a4 and a5
                sta a5l,x
                dex
                bpl lt2
                rts
move:           lda (a1l),y         ; move (a1 to a2) to
                sta (a4l),y         ;   (a4)
                jsr nxta4
                bcc move
                rts
vfy:            lda (a1l),y         ; verify (a1 to a2) with
                cmp (a4l),y         ;   (a4)
                beq vfyok
                jsr pra1
                lda (a1l),y
                jsr prbyte
                lda #$A0            ; space
                jsr cout
                lda #$A8            ; '('
                jsr cout
                lda (a4l),y
                jsr prbyte
                lda #$A9            ; ')'
                jsr cout
vfyok:          jsr nxta4
                bcc vfy
                rts
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
setinv:         ldy #$3F            ; set for inverse video
                bne setiflg
setnorm:        ldy #$FF            ; set for normal video
setiflg:        sty invflg
                rts
setkbd:         lda #$00            ; simulate port #0 input
inport:         sta a2l             ;   specified (keyin routine)
inprt:          ldx #kswl
                ldy #<keyin         ; see if it is the default vector
                bne ioprt
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
                nop
                nop
xbasic:         jmp basic
bascont:        jmp basic2
go:             jsr a1pc            ; adr to pc if specified
                jsr restore         ; restore meta registers
                jmp (pcl)           ; go to user subroutine
regz:           jmp regdsp          ; jump to register display
trace:          dec ysav
stepz:          jsr a1pc            ; adr to pc if specified
                jmp step            ; take one step
usr:            jmp usradr          ; to usr subroutine at usradr
write:          lda #$40
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
                beq bell
wrbyte:         ldx #$10
wrbyt2:         asl
                jsr wrbit
                bne wrbyt2
                rts
crmon:          jsr bli             ; handle CR as blank
                pla                 ;  then pop stack
                pla                 ; and return to mon
                bne monz
read:           jsr rd2bit          ; find tapein edge
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
                cmp chksum
                beq bell            ; good, sound bell and return
prerr:          lda #$C5            ; 'E'
                jsr cout            ; print "ERR" then bell
                lda #$d2            ; 'R'
                jsr cout
                jsr cout
bell:           lda #$87            ; output bell and return
                jmp cout            ; reuse cout rts directly here...
restore:        lda status          ; restore 6502 register contents
                pha                 ;  used by debug software
                lda acc
restr1:         ldx xreg
                ldy yreg
                plp
                rts
save:           sta acc             ; save 6502 reg contents
sav1:           stx xreg
                sty yreg
                php
                pla
                sta status
                tsx
                stx spnt
                cld
                rts
reset:          jsr setnorm
                jsr init
                jsr setvid
                jsr setkbd
mon:            cld
                jsr bell
monz:           lda #$2A            ; Monitor prompt
                sta prompt
                jsr getlnz          ; get line
                jsr zmode           ; clear monitor mode, scan idx
nxtitm:         jsr getnum          ; get item, non-hex
                sty ysav
                ldy #$17            ; x-reg=0 if no hex input
chrsrch:        dey
                bmi mon             ; not found, go to mon
                cmp chrtbl,y        ; find cmnd char in table
                bne chrsrch
                jsr tosub           ; found call corresponding subroutine
                ldy ysav
                jmp nxtitm
dig:            ldx #$03
                asl
                asl                 ; got hex digit, shift into A2
                asl
                asl
nxtbit:         asl
                rol a2l
                rol a2h
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
getnum:         ldx #$00            ;  clear A2
                stx a2h
                stx a2l
nxtchr:         lda in,y            ; get char
                iny
                eor #$B0
                cmp #$0A
                bcc dig             ; if hex digit
                adc #$88
                cmp #$FA
                bcs dig
                rts
tosub:          lda #>go            ; Push high order subroutine address on stack
                pha
                lda subtbl,y        ; Push low order subroutine address on stack
                pha
                lda mode
zmode:          ldy #$00            ; clr mode, old mode to A
                sty mode
                rts

chrtbl:         .byte $BC           ; F("CTRL-C")
                .byte $B2           ; F("CTRL-Y")
                .byte $BE           ; F("CTRL-E")
                .byte $ED           ; F("T")
                .byte $EF           ; F("V")
                .byte $C4           ; F("CTRL-K")
                .byte $EC           ; F("S")
                .byte $A9           ; F("CTRL-P")
                .byte $BB           ; F("CTRL-B")
                .byte $A6           ; F("-")
                .byte $A4           ; F("+")
                .byte $06           ; F("M") (F=EOR $B0+$89)
                .byte $95           ; F("<")
                .byte $07           ; F("N")
                .byte $02           ; F("I")
                .byte $05           ; F("L")
                .byte $F0           ; F("W")
                .byte $00           ; F("G")
                .byte $EB           ; F("R")
                .byte $93           ; F(":")
                .byte $A7           ; F(".")
                .byte $C6           ; F("CR")
                .byte $99           ; F(" ")
subtbl:         .byte <bascont
                .byte <usr
                .byte <regz
                .byte <trace
                .byte <vfy
                .byte <inprt
                .byte <stepz
                .byte <outprt
                .byte <xbasic
                .byte <setmode
                .byte <setmode
                .byte <move
                .byte <lt
                .byte <setnorm
                .byte <setinv
                .byte <list
                .byte <write
                .byte <go
                .byte <read
                .byte <setmode
                .byte <setmode
                .byte <crmon
                .byte <blank
sysvec_nmi:     .word   nmiloc     ; nmi vector in RAM
sysvec_reset:   .word   reset
sysvec_irq:     .word   irqvec

