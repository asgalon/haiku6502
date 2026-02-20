#
# Makefile for haiku6502rom
#
SRC=asm.s mnemonics_compressed.s sysmon.s sysmon_mnemonics_compressed.s interrupt.s vectors.s

PROGRAM := haiku6502.rom
CC65_TARGET := none

AS		= ca65
ASFLAGS =
CC		= cc65
LD      = ld65
CFLAGS  = -t $(CC65_TARGET) --create-dep $(<:.c=.d) -O
LDFLAGS = -C haiku6502.cfg -m $(PROGRAM).map


########################################

.SUFFIXES:
.PHONY: all clean
all: $(PROGRAM)

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

%.o: %.s
	$(AS) $(ASFLAGS) -l $(<:.s=.lst) $<

%.s:: haiku6502.cfg
	touch $@


%.s:: symbols.inc
	touch $@

$(PROGRAM): $(SRC:.s=.o)
	$(LD) $(LDFLAGS) -o $@ $^

clean:
	$(RM) *.o $(PROGRAM) $(PROGRAM).map