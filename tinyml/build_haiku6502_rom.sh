#!/usr/bin/env bash

cl65 -C haiku6502.cfg -t none -o haiku6502.rom -l haiku6502.lst asm.s mnemonics_compressed.s sysmon.s sysmon_mnemonics_compressed.s interrupt.s vectors.s
