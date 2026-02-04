; common symbol definitions, based on
; 1979 Apple ][ Reference Manual, Monitor ROM Listing

loc0    = $00
loc1    = $01
wndlft  = $20
wndwdth = $21
wndtop  = $22
wndbtm  = $23
ch      = $24
cv      = $25
rdline  = $28   ; vector for readline
rdlinh  = $29   ;   msb
h2      = $2C
lmnem   = $2C
rtnl    = $2C
v2      = $2D
rmnem   = $2D
rtnh    = $2D
mask    = $2E
chksum  = $2E
format  = $2E

length  = $2F
sign    = $2F
color   = $30
mode    = $31
invflg  = $32
prompt  = $33
ysav    = $34
ysav1   = $35
cswl    = $36   ; CSW character out vector
cswh    = $37
kswl    = $38   ; keyboard input vector
kswh    = $39
pcl     = $3A
pch     = $3B
xqt     = $3C
xqtnz   = $3C
a1l     = $3C
a1h     = $3D
a2l     = $3E
a2h     = $3F
a3l     = $40
a3h     = $41
a4l     = $42
a4h     = $43
a5l     = $44
a5h     = $45
acc     = $45
xreg    = $46
yreg    = $47
status  = $48
spnt    = $49
rndl    = $4E   ; pseudo-random
rndh    = $4F
acl     = $50
ach     = $51
xtndl   = $52
xtndh   = $53
auxl    = $54
auxh    = $55
;
; mini-assembler: $60-$70
;
mnem    = $60   ; temp mnemonic index
opcode  = $61   ; temp opcode for assembler
adrl    = $62   ; low address byte for assembler
adrh    = $63   ; high address byte for assembler
opmode  = $64   ; opcode "mode"
opb     = $65   ; opcode 'b' bits
cmdlen  = $66   ; instruction length for assembler, 1-3 bytes
calcl   = $67   ; calculations intermediate storage 16 bit
calch   = $68
tmpl    = $69   ; temporary storage 16 bit
tmph    = $6A

in      = $0200 ; input buffer base
usradr  = $03F8
nmiloc  = $03FB
irqloc  = $03FE

ioadr   = $C000
kbd     = $C000
kbdstrb = $C001
rnd     = $C00F     ; random byte RO

termcy  = $C010     ; terminal cursor y   RW
termcx  = $C011     ; terminal cursor x   RW
termout = $C012     ; termout character under cursor RW
termesc = $C013     ; terminal connamd byte W
termea1 = $C014     ; terminal command argl   (bad design, will change)
termea2 = $C015     ; terminal command argh
termwh  = $C016     ; temrinal window height
termww  = $C017     ; terminal window width
termin  = $C018
terml   = $C019     ; stdio mode next line char of 0 terminated string. R
termp   = $C01A     ; stdio mode next line char of 0 terminated string. RW
termmd  = $C01F     ; terminal mode, RO. #$00 - screen, #$01 - stdio

tapeio  = $C020     ; read/write byte from/to tape file
tapecls = $C021

spkr    = $C030
paddl0  = $C061
ptrig   = $C070

;
;  Ecape commands for terminal:
;
t_cls   = $00   ; Clear screen
t_cll   = $01   ; clear from cursor to end of line
t_clb   = $02   ; clear from cursor to end of line
t_scr   = $03   ; scroll one line up
t_norm  = $04   ; normal chars
t_inv   = $05   ; inverse chars
t_blnk  = $06   ; blinking chars
t_qcch  = $07   ; query current character under cursor

; t_qy    = $04   ; query terminal cursor y; for result read terminl,h
; t_qx    = $05   ; query terminal cursor x; for result read terminl,h

; characters
k_ctl_a = $01
k_ctl_b = $02
k_ctl_c = $03
k_ctl_d = $04
k_ctl_e = $05
k_ctl_f = $06
k_ctl_g = $07
k_bs    = $08
k_tab   = $09
k_lf    = $0A
k_ctl_k = $0B
k_ctl_l = $0C
k_cr    = $0A
k_ctl_n = $0E
k_ctl_o = $0F
k_ctl_p = $10
k_ctl_q = $11
k_ctl_r = $12
k_ctl_s = $13
k_ctl_t = $14
k_ctl_u = $15
k_ctl_v = $16
k_ctl_w = $17
k_ctl_x = $18
k_ctl_y = $19
k_ctl_z = $1A
k_esc   = $1B
k_entr  = k_cr
gr_top  = '+'
gr_btm  = '_'
colon   = $3A
gr_grid = $61
