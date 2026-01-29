#!env sh
#
# Assemble unit_test.rom with xa assembler.
xa -A F800 unit_test_rom.s -o ../data/unit_test.rom
