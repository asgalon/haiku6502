#!/usr/bin/env sh
#
# Assemble unit_test.rom with xa assembler.
xa rom.s $@ -C -XMASM -XCA65 -A D000 -o ../data/standard.rom -P ../doc/standard_rom.lst -l ../doc/standard_rom.lbl
