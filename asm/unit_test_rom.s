;
; ROM fo runit testing the engine.
;
                .include "symbols.inc"

                .org $D000
                .dsb $2800,$EA
                * = $f800
loop1           ldy #$ff
_lp1            dey
                bne _lp1
                rts
irqvec:         sei
                cli
                rti
nmivec:         sei
                cli
                rti
reset:          sed
                cld
                sei
                cli
                lda #$00
                ldx #$00
                ldy #$00
                sta loc0
                stx loc1
                nop
                jsr loop1
                clc
                lda #$37
                cmp #$de
                clc
                lda #$de
                cmp #$37
                clc
                lda #$37
                cmp #$37
                sta loc0
                clc
                adc #$50
                clv
                lda loc0
                sec
                adc #$50
                clv
                brk
                .byte $00
                .include "check_bcd_mode.s"
filler:         .dsb $07FA-filler+loop1,$EA
                * = $FFFA
sysvec_nmi:     .word   nmivec
sysvec_reset:   .word   reset
sysvec_irq:     .word   irqvec
