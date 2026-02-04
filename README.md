# Haiku6502
#### Video Demo:  <URL>
#### Description
This is an emulator for an early 1980ies type home computer based on the 6502 CPU. 
It borrows much of the design from the Apple ][, but I made a number of 
simplifications and modifications. The purpose is not to emulate a specific machine,
but to provide an environment to write programs under a similarly strict set of 
constraints like a japanese haiku poem. IT is really astonishing how much can be done with 
a single KB of assembler code.

## The Goal
The goal for completing the project was to have the emulator running in a virtual terminal window,
with the ability to enter and debug programs at least in machine language and run them, providing 
text-based output in upper case ASCII characters (codes 0x20-0x7F).
Optional features, depending on effort:
 - add assembler input (done)
 - load and save content to file (done, changed monitor tape interface)
 - fill the remaining 10KB of ROM with a small LISP interpreter (skipped)
 - add graphics character support to the ncurses terminal (skipped)
 - add color support (skipped)
 - add further virtual peripherals such as printer interface, graphics card or simple networking. (skipped)
 - get the instruction machine cycle count right for every variation. This is prepared, but not completed.
 - Set up program installation
 - Test build on Linux

## The constraints
There is 48KB of RAM, of which 47KB are free to use, 12KB of ROM, of which the standard
system ROM takes up the top 2KB. The ROM file can be loaded at startup. The only immediately
available programmoing environment is a primitive system monitor to manipulate memory
areas in hexadecimal bytes, the mini assembler, and the disassembler. The screen size in ncurses mode 
is the size of the virtual terminal. the emulator has only ASCII characters below 0x80 to simplify 
handling a bit.

## The features
There are two built-in versions of the user interface: An ncurses-based virtual terminal,
and a standard streams interface using stdin and stdout. This way, the emulator can be used
in a pipe. I based the machine on the Apple II layout, but removed the memory-mapped text 
and graphics displays. I also rearranged the I/O area a bit for convenience. I left in the 
tape interface, though, which can save and restore memory blocks by writing out square 
waves in a data file, which admittedly is very, very silly indeed. The header alone produces about 4MB 
of monotone uncompressed data. I also changed the pseudo ASCII-characters to normal ASCII characters.
'A' is now printed as 0x41 again instead of 0xC1.

## The ROM
I used the printout of the system monitor ROM in the 1979 Apple II Reference Manual as a starting point,
to speed up the process a little. The source is a printout of the assembler source code made on a 9 needle 
dot matrix printer in low quality, so it needed some heavy cleanup. Bugfixing assembler code targeted for 
a slightly different is fun, especially when at the same time debugging the CPU emulator code.
I used the xa open source assembler to produce the new ROM code. ca65 is more capable, but this one is easier 
for non relocatable code.
At the en, the resulting ROM is still about the same size but no longer compatible with the 
original system. It also lacks the BASIC interpreter form the original since that was firstly proprietary 
software, and secondly it was BASIC, which I never liked.
I stripped the graphics handling and modified the system routines to support the changed pseudo-hardware.
The main I/O capabilites are handled by a virtual "terminal" device that occupies the 0xC010-0xC01F range.
I also changed a few of the other I/O locations that no longer make sense without being restricted by the 
original hardware.

## The Emulator
The emulator is written in C++ in a CMake project structure. Unit tests are done with Boost Test library.

### The System Monitor ROM
This one is based on the old system monitor that preceded the Autostart ROM in the Apple II computer. 
Its contents are listed in the reference manual, so I typed this into a file, corrected all reading 
and spelling errors, analysed it, and then modified it to suit the specifications of this emulator.
Functionality mainly as described in that referenc emanual.

    300

shows the content at address 300

    300.320

shows the content of bytes 300 to 320

    300<400.420M

copies bytes 400-420 to location 300-320

    300.3FFW

writes bytes 300-3ff to the tape file in the current directory

    300.3FFR

reads the tape file to 300-3FF

    300G

starts the subroutine located at 300

    300: 12 34 56 78 9A BC

writes the given bytes into the location starting with 300

New:

    Q

Terminates the emulator program

    !

starts the mini assembler,

    300!

starts the mini assembler and sets the input point to address 300


The assembler shows the prompt '!' instead of '*'

    +

goes back to the system monitor

### Command line options

- -c  starts the emulator in command line mode (ncurses is the default)
- -r <romfile> loads a 12KB ROM file into D000-FFFF. It must fill the space and have the 6502 reset and irq vectors at FFFA-FFFF
- -t <tapefile> sets the name of the tapefile, default is tape.data
- -d for some debug output
- -m <ramfile>  Load RAM content
- -l <addr>          ... at this address


## Files
- src and include/haiku6502: C++ source directories
  - src/main.cpp    Application Main entry and command line interface
  - include/haiku6502/setup.h configuration filled from command line args
  - cpu.h/cpp   Details of inner CPU, registers, structs, enums
  - engine.h/cpp The main emulator engine
  - device.h Basic device interface for terminal and i/o peripherals
  - peripheral.h Interface for I/O peripherals
  - slot0_peripheral.h Interface for Memory extension peripherals
  - memory.h/cpp Definitions for memory structure
  - terminal.h/cpp The screen/keyboard device with ncurses screen layout variant and simple stdio command line interface
  - test/test.cpp  Unit tests
- asm: Assembler sources
  - asm/rom.s   System monitor ROM, written in xa assembler, based on Apple II Ref. manual monitor listing
  - asm/asm.s   Mini-Assembler, writen in xa assembler
  - symbols.inc Symbols used in rom.s and asm.s
  - unit_test.rom.s  ROM for unit test
  - am_testrom.sh shell script to assemble unit test rom
  - asm_rom.sh shell script to assemble standard rom
  - calc_opval.py utility script to compute compressed mnemonic codes for lookup
  - opcodes_compressed.s result of calc_opval.py
  - check_bcs_mode.s copied test script for binary coded decimal arithmetics
- data
  - check_bcd_mode_0800.ram  loadable program to check BCD arithmetics
  - standard.rom  Standard ROM file, F4C6-FFFF used
  - unit_test.rom unit test roms
- CMakeLists.txt CMake configurations
- cmake/FindReadline.cmake  CMake setup for standard GNU readline library
