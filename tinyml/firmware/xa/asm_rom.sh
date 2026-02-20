#!/usr/bin/env sh
#
# Assemble unit_test.rom with xa assembler.
xa rom.s "$@" -XMASM -XCA65 -A D000 -o ./haiku6502.rom -P ./standard_rom.lst -l ./standard_rom.lbl
