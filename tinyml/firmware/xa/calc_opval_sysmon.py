#!/usr/bin/env python
import re

opcodes = [
        'ADC',
        'AND',
        'ASL',
        'BBR',
        'BBS',
        'BCC',
        'BCS',
        'BEQ',
        'BIT',
        'BMI',
        'BNE',
        'BPL',
        'BRA',
        'BRK',
        'BVC',
        'BVS',
        'CLC',
        'CLD',
        'CLI',
        'CLV',
        'CMP',
        'CPX',
        'CPY',
        'DEC',
        'DEX',
        'DEY',
        'EOR',
        'INC',
        'INX',
        'INY',
        'JMP',
        'JSR',
        'LDA',
        'LDX',
        'LDY',
        'LSR',
        'NOP',
        'ORA',
        'PHA',
        'PHP',
        'PHX',
        'PHY',
        'PLA',
        'PLP',
        'PLX',
        'PLY',
        'RMB',
        'ROL',
        'ROR',
        'RTI',
        'RTS',
        'SBC',
        'SEC',
        'SED',
        'SEI',
        'SMB',
        'STA',
        'STP',
        'STX',
        'STY',
        'STZ',
        'TAX',
        'TAY',
        'TRB',
        'TSB',
        'TSX',
        'TXA',
        'TXS',
        'TYA',
        'WAI'
]

mappings = [
        "BRK, PHP, BPL, CLC, JSR, PLP, BMI, SEC",
        "RTI, PHA, BVC, CLI, RTS, PLA, BVS, SEI",
        "BRA, DEY, BCC, TYA, LDY, TAY, BCS, CLV",
        "CPY, INY, BNE, CLD, CPX, INX, BEQ, SED",
        "???, BIT, JMP, JMP, STY, LDY, CPY, CPX",
        "TXA, TXS, TAX, TSX, DEX, PHX, NOP, PLX",
        "ASL, ROL, LSR, ROR, STX, LDX, DEC, INC",
        "ORA, AND, EOR, ADC, STA, LDA, CMP, SBC",
        "RMB, SMB, BBR, BBS, ---, ---, ---, ---",
        "INC, DEC, PHY, PLY, ---, ---, ---, ---",
        "TSB, TRB, ---, ---, ---, ---, ---, ---",
        "BIT, STZ, ---, ---, ---, ---, ---, ---",
        "WAI, STP, ---, ---, ---, ---, ---, ---",
        "BIT, ---, ---, ---, ---, ---, ---, ---"
]

def get_mnem_val(mnem):
        map0 = ord(mnem[0]) - ord('?')
        map1 = ord(mnem[1]) - ord('?')
        map2 = ord(mnem[2]) - ord('?')

        val = (map0 << 11) + (map1 << 6) + (map2 << 1)
        return val

with open("opcodes_compressed.s", "w") as f:
        f.write(f"opcode_mnem:    .word $0000    ; '???' for sysmon\n")
        f.write(f"opcodex:\n")

        for i, mnem in enumerate(opcodes):
                val = get_mnem_val(mnem)
                f.write(f"                .word {"${:04x}".format(val)}    ; #{"${:02x}".format(i)} - {mnem}   ({"${:02x}".format(2* i)})\n")
        f.flush()

# for mline in mappings:
#         for mnem in re.split(',? ', mline):
#                 if mnem in opcodes:
#                         print(f"{"${:02x}".format(opcodes.index(mnem))}, ", end='', flush=True)
#                 else:
#                         print("$00, ", end='', flush=True)
#         print("\n")

with open("sysmon_mnemonics_compressed.s", "w") as f:
        for i, mline in enumerate(mappings):
                if i==0:
                        f.write("mnemidx:       .byte ")
                elif i==13:
                        f.write("mnemidx_d:     .byte ")
                else:
                        f.write("               .byte ")

                # base index on 3F instead of 40 so 0 becomes '?'
                for n,mnem in enumerate(re.split(',? ', mline)):
                        if mnem == '???':
                                f.write("$00")
                        elif mnem == '---':
                                f.write("    ")
                        else:
                                if n > 0:
                                        f.write(",")
                                val = (opcodes.index(mnem)+1) << 1  # word table, index is double.
                                f.write(f"{"${:02x}".format(val)}")
                f.write(f"     ; {mline.replace(', ---','')}\n")
        f.write("\n")
