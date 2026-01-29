# 8bit Haiku Engine Reference Manual

## Overview

The 8bit Haiku Engine emulates an 8 bit microcomputer based on the 6502 CPU. 
The purpose of this engine is to provide a standard platform for developing
programs under the very heavy restrictions of such a machine. There are no more than
2^16^ = 65536 bytes addressable in the memory, and only part of that is usable for data or program.
The clock rate can be configured at _n_ MHz, where 1 <= _n_ <= 10. The overall layout is loosely based on the Apple ][ 
microcomputer from 1979, with a few modifications. The layout of the screen memory
does not make any sense anymore since cathode ray tube monitors are definitely a thing of the past.
The high resolution graphics display of that machine were organized in such a way as to stream out
the screen content following the pattern of scanning lines for the tube. For our engine, we assume
a separate graphics peripheral that is controlled via a number of ports, so the content of the screen is not held 
in the main memory. This simplifies the memory layout and allows for different output geometries.
The standard output unit will be a ncurses-driven terminal screen for presenting a 80x24 text window.

The engine has an 8 bit data bus, a 16 bit address bus, 3 8 bit registers A, X and Y, a 16 bit program counter,
a status register with a few flags, a maskable interrupt, a non maskable interrupt, and tht's it more or less.

## Memory Layout

| System Memory Map |           |
|-------------------|-----------|
| Page Number       |           |
| 0x00              |           |
| ...               | RAM (48K) |
| 0xBF              |           |
|                   |           |
| 0xC0              |           |
| ...               | I/O       |
| 0xCF              |           |
|                   |           |
| 0xD0              |           |
| ...               | ROM       |
| 0xFF              |           |



| Address range | Content          | Scope |
|--------------:|:-----------------|:------|
| 0x0000-0x00FF | Zero Page        | RAM   |
| 0x0100-0x01FF | 6502 Stack       |       |
| 0x0200-0x02FF | Input Buffer     |       |
| 0x0300-0x03FF | Vector locations |       |
| 0x0400-0xBFFF | Free RAM         |       |
| 0xC000-0xCFFF | I/O              | I/O   |
| 0xD000-0xFFFF | ROM              | ROM   |

 
### I/O Locations

The 4K block 0xC000-0xCFFF serves to connect I/O devices such as keyboard input, serial interfaces, disk storage and the text/graphics terminal.
There is no networking device right now, but one could be attached to some I/O address slot.

#### Built-In I/O

|        | 0                       | 1 | 2 | 3 | 4          | 5          | 6          | 7 | 8 | 9 | A | B | C | D | E | F |
|--------|-------------------------|--|--|--|------------|------------|------------|--|--|--|--|--|--|--|--|--|
| 0xC000 | Keyboard Data Input LSB | | | MSB |
| 0xC010 |                         |
| 0xC020 |                         |
| 0xC030 | Speaker Output          |
| 0xC040 |                         |
| 0xC050 |                         |
| 0xC060 | Audio Input             | Button 1 | Button 2 | Button 3 | Joystick 0 | joystick 1 | Joystick 2 | Joystick 3 |
| 0xC070 | Game Controller Out     |
| 0xC080 | Peripheral Slot #0      |
| 0xC090 | Peripheral Slot #1      |
| 0xC0A0 | Peripheral Slot #2      |
| 0xC0B0 | Peripheral Slot #3      |
| 0xC0C0 | Peripheral Slot #4      |
| 0xC0D0 | Peripheral Slot #5      |
| 0xC0E0 | Peripheral Slot #6      |
| 0xC0F0 | Peripheral Slot #7      |

#### Peripheral Slot ROM Space
256 Bytes of peripheral ROM for slot _n_ is available at 0xC100 + 0x0_n_00 - 0xC1FF + 0x0_n_00.


## Appendix

### Literature

[AREF] - The Apple ][ Reference Manual 1979 by Apple Computer  Inc., Christopher Espinosa, Apple Product Number A2L0001A (030-0004-01)
