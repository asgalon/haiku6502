mneml:         .byte $14,$82,$14,$1b,$54,$83,$13,$99     ; BRK, PHP, BPL, CLC, JSR, PLP, BMI, SEC
               .byte $95,$82,$15,$1b,$95,$83,$15,$99     ; RTI, PHA, BVC, CLI, RTS, PLA, BVS, SEI
               .byte $14,$21,$10,$a6,$61,$a0,$10,$1b     ; BRA, DEY, BCC, TYA, LDA, TAY, BCS, CLV
               .byte $1c,$4b,$13,$1b,$1c,$4b,$11,$99     ; CPY, INY, BNE, CLD, CPX, INX, BEQ, SED
               .byte $00,$12,$53,$53,$9d,$61,$1c,$1c     ; ???, BIT, JMP, JMP, STY, LDY, CPY, CPX
               .byte $a6,$a6,$a0,$a4,$21,$82,$73,$83     ; TXA, TXS, TAX, TSX, DEX, PHX, NOP, PLX
               .byte $0c,$93,$64,$93,$9d,$61,$21,$4b     ; ASL, ROL, LSR, ROR, STX, LDX, DEC, INC
               .byte $7c,$0b,$2b,$09,$9d,$61,$1b,$98     ; ORA, AND, EOR, ADC, STA, LDA, CMP, SBC
               .byte $93,$9b,$10,$10                     ; RMB, SMB, BBR, BBS
               .byte $4b,$21,$82,$83                     ; INC, DEC, PHY, PLY
               .byte $a4,$a4                             ; TSB, TRB
               .byte $12,$9d                             ; BIT, STZ
mneml_d:       .byte $12                                 ; BIT

mnemr:         .byte $96,$20,$18,$06,$e4,$20,$52,$46     ; BRK, PHP, BPL, CLC, JSR, PLP, BMI, SEC
               .byte $12,$02,$86,$12,$26,$02,$a6,$52     ; RTI, PHA, BVC, CLI, RTS, PLA, BVS, SEI
               .byte $82,$72,$c6,$42,$02,$72,$e6,$2c     ; BRA, DEY, BCC, TYA, LDA, TAY, BCS, CLV
               .byte $32,$b2,$8a,$08,$30,$b0,$62,$48     ; CPY, INY, BNE, CLD, CPX, INX, BEQ, SED
               .byte $00,$68,$60,$60,$32,$32,$32,$30     ; ???, BIT, JMP, JMP, STY, LDY, CPY, CPX
               .byte $02,$26,$70,$f0,$70,$30,$e0,$30     ; TXA, TXS, TAX, TSX, DEX, PHX, NOP, PLX
               .byte $d8,$d8,$e4,$e4,$30,$30,$46,$86     ; ASL, ROL, LSR, ROR, STX, LDX, DEC, INC
               .byte $82,$88,$e4,$06,$02,$02,$60,$86     ; ORA, AND, EOR, ADC, STA, LDA, CMP, SBC
               .byte $44,$44,$a4,$a6                     ; RMB, SMB, BBR, BBS
               .byte $86,$46,$32,$32                     ; INC, DEC, PHY, PLY
               .byte $c4,$84                             ; TSB, TRB
               .byte $68,$34                             ; BIT, STZ
               .byte $68                                 ; BIT
