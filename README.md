# Haiku6502
#### Video Demo:  <URL>
#### Description
This is an emulator for an early 1980ies type home computer based on the 6502 CPU. 
It borrows much of the design from the Apple ][, but I made a number of 
simplifications and modifications. The purpose is not to emulate a specific machine,
but to provide an environment to write programs under a similarly strict set of 
constraints like a japanese haiku poem. IT is really astonishing how much can be done with 
a KB of assembler code.

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


