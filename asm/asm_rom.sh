#!env sh
#
# Assemble unit_test.rom with xa assembler.
xa rom.s -A D000 -o ../data/standard.rom -P ../doc/standard_rom.lst
