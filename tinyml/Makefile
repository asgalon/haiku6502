SOURCES = asm.s mnemonics_compressed.s sysmon.s sysmon_mnemonics_compressed.s interrupt.s vectors.s

PROGRAM = haiku6502.rom
CC65_TARGET = none

ifdef CC65_TARGET
ASM		= ca65
ASMFLAGS =
CC      = cl65
CFLAGS  = -t $(CC65_TARGET) --create-dep $(<:.c=.d) -O
LDFLAGS = -t $(CC65_TARGET) -m $(PROGRAM).map
else
CC      = gcc
CFLAGS  = -MMD -MP -O
LDFLAGS = -Wl,-Map,$(PROGRAM).map
endif

########################################

.SUFFIXES:
.PHONY: all clean
all: $(PROGRAM)

ifneq ($(MAKECMDGOALS),clean)
-include $(SOURCES:.c=.d)
endif

%.o: %.c
        $(CC) -c $(CFLAGS) -o $@ $<

%.o: %.s
        $(ASM) -c $(ASMFLAGS) -l $(<:.s=.lst)-o $@ $<

$(PROGRAM): $(SOURCES:.c=.o)
        $(CC) $(LDFLAGS) -o $@ $^

clean:
        $(RM) $(SOURCES:.c=.o) $(SOURCES:.c=.d) $(PROGRAM) $(PROGRAM).map