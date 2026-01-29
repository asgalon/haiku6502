; from http://www.6502.org/tutorials/decimal_mode.html#B
; Verify decimal mode behavior
; Written by Bruce Clark.  This code is public domain.
;
; Returns:
;   ERROR = 0 if the test passed
;   ERROR = 1 if the test failed
;
; This routine requires 17 bytes of RAM -- 1 byte each for:
;   AR, CF, DA, DNVZC, ERROR, HA, HNVZC, N1, N1H, N1L, N2, N2L, NF, VF, and ZF
; and 2 bytes for N2H
;
; Variables:
;   N1 and N2 are the two numbers to be added or subtracted
;   N1H, N1L, N2H, and N2L are the upper 4 bits and lower 4 bits of N1 and N2
;   DA and DNVZC are the actual accumulator and flag results in decimal mode
;   HA and HNVZC are the accumulator and flag results when N1 and N2 are
;     added or subtracted using binary arithmetic
;   AR, NF, VF, ZF, and CF are the predicted decimal mode accumulator and
;     flag results, calculated using binary arithmetic
;
; This program takes approximately 1 minute at 1 MHz (a few seconds more on
; a 65C02 than a 6502 or 65816)
;
        .org $0800

AR     .byte 0      ; 0x0800
CF     .byte 0      ; 0x0801
DA     .byte 0      ; 0x0802
DNVZC  .byte 0      ; 0x0803
ERROR  .byte 0      ; 0x0804
HA     .byte 0      ; 0x0805
HNVZC  .byte 0      ; 0x0806
N1     .byte 0      ; 0x0807
N1H    .byte 0      ; 0x0808
N1L    .byte 0      ; 0x0809
NH     .byte 0      ; 0x080a
N2     .byte 0      ; 0x080b
N2H    .byte 0      ; 0x080c
N2L    .byte 0      ; 0x080d
NF     .byte 0      ; 0x080e
VF     .byte 0      ; 0x080f
ZF     .byte 0      ; 0x0810

TEST    LDY #1        ; 0x084A initialize Y (used to loop through carry flag values)
        LDA #0        ; 0x084F initialize N1 and N2
        STA ERROR     ; 0x084C store 1 in ERROR until the test passes
        STA N1        ; 0x0851
        STA N2        ; 0x0854
LOOP1   LDA N2        ; 0x0857 N2L = N2 & $0F
        AND #$0F      ; 0x085A [1] see text
        CMP #$0A      ; 0x085C
        BCS NEXT2     ; 0x085E
        STA N2L       ; 0x0860
        LDA N2        ; 0x0863 N2H = N2 & $F0
        AND #$F0      ; 0x0866 [2] see text
        CMP #$A0      ; 0x0868
        BCS NEXT2     ; 0x086A
        STA N2H       ; 0x086C
        ORA #$0F      ; 0x086F N2H+1 = (N2 & $F0) + $0F
        STA N2H+1     ; 0x0871
LOOP2   LDA N1        ; 0x0874 N1L = N1 & $0F
        AND #$0F      ; 0x0877 [3] see text
        CMP #$0A      ; 0x0879
        BCS NEXT1     ; 0x087B
        STA N1L       ; 0x087D
        LDA N1        ; 0x0880 N1H = N1 & $F0
        AND #$F0      ; 0x0883 [4] see text
        CMP #$A0      ; 0x0885
        BCS NEXT1     ; 0x0887
        STA N1H       ; 0x0889
        JSR ADD       ; 0x088C
        JSR A6502     ; 0x088F
        JSR COMPARE   ; 0x0892
        BNE DONE      ; 0x0895
        JSR SUB       ; 0x0897
        JSR S6502     ; 0x089A
        JSR COMPARE   ; 0x089D
        BNE DONE      ; 0x08A0
NEXT1   INC N1        ; 0x08A2 [5] see text
        BNE LOOP2     ; 0x08A5 loop through all 256 values of N1
NEXT2   INC N2        ; 0x08A7 [6] see text
        BNE LOOP1     ; 0x08AA loop through all 256 values of N2
        DEY           ; 0x08AC
        BPL LOOP1     ; 0x08AD loop through both values of the carry flag
        LDA #0        ; 0x08AF test passed, so store 0 in ERROR
        STA ERROR     ; 0x08B1
DONE    RTS           ; 0x08B4

; Calculate the actual decimal mode accumulator and flags, the accumulator
; and flag results when N1 is added to N2 using binary arithmetic, the
; predicted accumulator result, the predicted carry flag, and the predicted
; V flag
;
ADD     SED       ; 0x08B5 decimal mode
        CPY #1    ; 0x08B6 set carry if Y = 1, clear carry if Y = 0
        LDA N1    ; 0x08B8
        ADC N2    ; 0x08BB
        STA DA    ; 0x08BE actual accumulator result in decimal mode
        PHP       ; 0x08C1
        PLA       ; 0x08C2
        STA DNVZC ; 0x08C3 actual flags result in decimal mode
        CLD       ; 0x08C6 binary mode
        CPY #1    ; 0x08C7 set carry if Y = 1, clear carry if Y = 0
        LDA N1    ; 0x08C9
        ADC N2    ; 0x08CC
        STA HA    ; 0x08CF accumulator result of N1+N2 using binary arithmetic

        PHP       ; 0x08D2
        PLA       ; 0x08D3
        STA HNVZC ; 0x08D4 flags result of N1+N2 using binary arithmetic
        CPY #1    ; 0x08D7
        LDA N1L   ; 0x08D9
        ADC N2L   ; 0x08DC
        CMP #$0A  ; 0x08DF
        LDX #0    ; 0x08E1
        BCC A1    ; 0x08E3
        INX       ; 0x08E5
        ADC #5    ; 0x08E6 add 6 (carry is set)
        AND #$0F  ; 0x08E8
        SEC       ; 0x08EA
A1      ORA N1H   ; 0x08EB
;
; if N1L + N2L <  $0A, then add N2 & $F0
; if N1L + N2L >= $0A, then add (N2 & $F0) + $0F + 1 (carry is set)
;
        ADC N2H,X ; 0x08EE
        PHP       ; 0x08F1
        BCS A2    ; 0x08F2
        CMP #$A0  ; 0x08F4
        BCC A3    ; 0x08F6
A2      ADC #$5F  ; 0x08F8 add $60 (carry is set)
        SEC       ; 0x08FA
A3      STA AR    ; 0x08FB predicted accumulator result
        PHP       ; 0x08FE
        PLA       ; 0x08FF
        STA CF    ; 0xF900 predicted carry result
        PLA       ; 0xF903
;
; note that all 8 bits of the P register are stored in VF
;
        STA VF    ; 0xF904 predicted V flags
        RTS       ; 0xF907

; Calculate the actual decimal mode accumulator and flags, and the
; accumulator and flag results when N2 is subtracted from N1 using binary
; arithmetic
;
SUB     SED       ; 0xF908 decimal mode
        CPY #1    ; 0xF909 set carry if Y = 1, clear carry if Y = 0
        LDA N1    ; 0xF90B
        SBC N2    ; 0xF90E
        STA DA    ; 0xF911 actual accumulator result in decimal mode
        PHP       ; 0xF914
        PLA       ; 0xF915
        STA DNVZC ; 0xF916 actual flags result in decimal mode
        CLD       ; 0xF919 binary mode
        CPY #1    ; 0xF91A set carry if Y = 1, clear carry if Y = 0
        LDA N1    ; 0xF91C
        SBC N2    ; 0xF91F
        STA HA    ; 0xF922 accumulator result of N1-N2 using binary arithmetic

        PHP       ; 0xF925
        PLA       ; 0xF926
        STA HNVZC ; 0xF927 flags result of N1-N2 using binary arithmetic
        RTS       ; 0xF92A

; Calculate the predicted SBC accumulator result for the 6502 and 65816

;
SUB1    CPY #1    ; 0xF92B set carry if Y = 1, clear carry if Y = 0
        LDA N1L   ; 0xF92D
        SBC N2L   ; 0xF930
        LDX #0    ; 0xF933
        BCS S11   ; 0xF935
        INX       ; 0xF937
        SBC #5    ; 0xF938 subtract 6 (carry is clear)
        AND #$0F  ; 0xF93A
        CLC       ; 0xF93C
S11     ORA N1H   ; 0xF93D
;
; if N1L - N2L >= 0, then subtract N2 & $F0
; if N1L - N2L <  0, then subtract (N2 & $F0) + $0F + 1 (carry is clear)
;
        SBC N2H,X ; 0xF940
        BCS S12   ; 0xF943
        SBC #$5F  ; 0xF945 subtract $60 (carry is clear)
S12     STA AR    ; 0xF947
        RTS       ; 0xF94A

; Calculate the predicted SBC accumulator result for the 6502 and 65C02

;
SUB2    CPY #1     ; 0xF94B set carry if Y = 1, clear carry if Y = 0
        LDA N1L    ; 0xF94D
        SBC N2L    ; 0xF950
        LDX #0     ; 0xF953
        BCS S21    ; 0xF955
        INX        ; 0xF957
        AND #$0F   ; 0xF958
        CLC        ; 0xF95A
S21     ORA N1H    ; 0xF95B
;
; if N1L - N2L >= 0, then subtract N2 & $F0
; if N1L - N2L <  0, then subtract (N2 & $F0) + $0F + 1 (carry is clear)
;
        SBC N2H,X   ; 0xF95E
        BCS S22     ; 0xF961
        SBC #$5F    ; 0xF963 subtract $60 (carry is clear)
S22     CPX #0      ; 0xF965
        BEQ S23     ; 0xF967
        SBC #6      ; 0xF969
S23     STA AR      ; 0xF96B predicted accumulator result
        RTS         ; 0xF96E

; Compare accumulator actual results to predicted results
;
; Return:
;   Z flag = 1 (BEQ branch) if same
;   Z flag = 0 (BNE branch) if different
;
COMPARE LDA DA     ; 0xF96F
        CMP AR     ; 0xF972
        BNE C1     ; 0xF975
        LDA DNVZC  ; 0xF977
        EOR CF     ; 0xF97A
        AND #1     ; 0xF97D mask off C flag
C1      RTS        ; 0xF97F

; These routines store the predicted values for ADC and SBC for the 6502,
; 65C02, and 65816 in AR, CF, NF, VF, and ZF

A6502   LDA VF        ; 0xF980
;
; since all 8 bits of the P register were stored in VF, bit 7 of VF contains
; the N flag for NF
;
        STA NF        ; 0xF983
        LDA HNVZC     ; 0xF986
        STA ZF        ; 0xF989
        RTS           ; 0xF98C

S6502   JSR SUB1      ; 0xF98D
        LDA HNVZC     ; 0xF990
        STA NF        ; 0xF993
        STA VF        ; 0xF996
        STA ZF        ; 0xF999
        STA CF        ; 0xF99C
        RTS           ; 0xF99F

A65C02  LDA AR
        PHP
        PLA
        STA NF
        STA ZF
        RTS

S65C02  JSR SUB2
        LDA AR
        PHP
        PLA
        STA NF
        STA ZF
        LDA HNVZC
        STA VF
        STA CF
        RTS

A65816  LDA AR
        PHP
        PLA
        STA NF
        STA ZF
        RTS

S65816  JSR SUB1
        LDA AR
        PHP
        PLA
        STA NF
        STA ZF
        LDA HNVZC
        STA VF
        STA CF
        RTS