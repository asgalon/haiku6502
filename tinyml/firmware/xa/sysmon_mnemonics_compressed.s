mneml:         .byte $1c,$8a,$1c,$23,$5d,$8b,$1b,$a1     ; BRK, PHP, BPL, CLC, JSR, PLP, BMI, SEC
               .byte $9d,$8a,$1d,$23,$9d,$8b,$1d,$a1     ; RTI, PHA, BVC, CLI, RTS, PLA, BVS, SEI
               .byte $1c,$29,$19,$ae,$69,$a8,$19,$23     ; BRA, DEY, BCC, TYA, LDY, TAY, BCS, CLV
               .byte $24,$53,$1b,$23,$24,$53,$19,$a1     ; CPY, INY, BNE, CLD, CPX, INX, BEQ, SED
               .byte $00,$1a,$5b,$5b,$a5,$69,$24,$24     ; ???, BIT, JMP, JMP, STY, LDY, CPY, CPX
               .byte $ae,$ae,$a8,$ad,$29,$8a,$7c,$8b     ; TXA, TXS, TAX, TSX, DEX, PHX, NOP, PLX
               .byte $15,$9c,$6d,$9c,$a5,$69,$29,$53     ; ASL, ROL, LSR, ROR, STX, LDX, DEC, INC
               .byte $84,$13,$34,$11,$a5,$69,$23,$a0     ; ORA, AND, EOR, ADC, STA, LDA, CMP, SBC
               .byte $9b,$a3,$18,$18                     ; RMB, SMB, BBR, BBS
               .byte $53,$29,$8a,$8b                     ; INC, DEC, PHY, PLY
               .byte $ad,$ac                             ; TSB, TRB
               .byte $1a,$a5                             ; BIT, STZ
               .byte $c0,$a5                             ; WAI, STP
mneml_d:       .byte $1a                                 ; BIT

mnemr:         .byte $d8,$62,$5a,$48,$26,$62,$94,$88     ; BRK, PHP, BPL, CLC, JSR, PLP, BMI, SEC
               .byte $54,$44,$c8,$54,$68,$44,$e8,$94     ; RTI, PHA, BVC, CLI, RTS, PLA, BVS, SEI
               .byte $c4,$b4,$08,$84,$74,$b4,$28,$6e     ; BRA, DEY, BCC, TYA, LDY, TAY, BCS, CLV
               .byte $74,$f4,$cc,$4a,$72,$f2,$a4,$8a     ; CPY, INY, BNE, CLD, CPX, INX, BEQ, SED
               .byte $00,$aa,$a2,$a2,$74,$74,$74,$72     ; ???, BIT, JMP, JMP, STY, LDY, CPY, CPX
               .byte $44,$68,$b2,$32,$b2,$72,$22,$72     ; TXA, TXS, TAX, TSX, DEX, PHX, NOP, PLX
               .byte $1a,$1a,$26,$26,$72,$72,$88,$c8     ; ASL, ROL, LSR, ROR, STX, LDX, DEC, INC
               .byte $c4,$ca,$26,$48,$44,$44,$a2,$c8     ; ORA, AND, EOR, ADC, STA, LDA, CMP, SBC
               .byte $86,$86,$e6,$e8                     ; RMB, SMB, BBR, BBS
               .byte $c8,$88,$74,$74                     ; INC, DEC, PHY, PLY
               .byte $06,$c6                             ; TSB, TRB
               .byte $aa,$76                             ; BIT, STZ
               .byte $94,$62                             ; WAI, STP
               .byte $aa                                 ; BIT
