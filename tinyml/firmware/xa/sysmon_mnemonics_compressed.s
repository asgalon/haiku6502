mnemidx:       .byte $1c,$50,$18,$22,$40,$58,$14,$6a     ; BRK, PHP, BPL, CLC, JSR, PLP, BMI, SEC
               .byte $64,$4e,$1e,$26,$66,$56,$20,$6e     ; RTI, PHA, BVC, CLI, RTS, PLA, BVS, SEI
               .byte $1a,$34,$0c,$8a,$46,$7e,$0e,$28     ; BRA, DEY, BCC, TYA, LDY, TAY, BCS, CLV
               .byte $2e,$3c,$16,$24,$2c,$3a,$10,$6c     ; CPY, INY, BNE, CLD, CPX, INX, BEQ, SED
               .byte $00,$12,$3e,$3e,$78,$46,$2e,$2c     ; ???, BIT, JMP, JMP, STY, LDY, CPY, CPX
               .byte $86,$88,$7c,$84,$32,$52,$4a,$5a     ; TXA, TXS, TAX, TSX, DEX, PHX, NOP, PLX
               .byte $06,$60,$48,$62,$76,$44,$30,$38     ; ASL, ROL, LSR, ROR, STX, LDX, DEC, INC
               .byte $4c,$04,$36,$02,$72,$42,$2a,$68     ; ORA, AND, EOR, ADC, STA, LDA, CMP, SBC
               .byte $5e,$70,$08,$0a                     ; RMB, SMB, BBR, BBS
               .byte $38,$30,$54,$5c                     ; INC, DEC, PHY, PLY
               .byte $82,$80                             ; TSB, TRB
               .byte $12,$7a                             ; BIT, STZ
               .byte $8c,$74                             ; WAI, STP
mnemidx_d:     .byte $12                                 ; BIT

