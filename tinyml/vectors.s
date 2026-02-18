; ---------------------------------------------------------------------------
; vectors.s
; ---------------------------------------------------------------------------
;
; Defines the interrupt vector table.

.import    _reset
.import    _nmi_int, _irq_int

.segment  "VECTORS"

.addr      _nmi_int     ; NMI vector
.addr      _reset       ; Reset vector
.addr      _irq_int     ; IRQ/BRK vector
