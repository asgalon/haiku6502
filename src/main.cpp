#include <cstdio>
#include <cstring>
#include <iostream>
#include <unistd.h>

#include "../include/haiku6502/setup.h"
#include "haiku6502/engine.h"
#include "haiku6502/slot0_peripheral.h"
#include "haiku6502/terminal.h"

using namespace std;

static void usage(const char *progname) {
    cout << "Usage: " << progname << " [-c] [-d] -r <romfile.rom> [-m <ramfile.ram>]  [-l <load address>] -t <tape.data>";
    // cout << " [-i <sourcefile>] [-o <outputfile>]";
    cout << endl;
    cout << "   -c            Console mode, use standard terminal streams directly" << endl;
    cout << "   -d            Debug output" << endl;
    cout << "   -r <romfile>  Load 12KB ROM file at 0xD000 through 0xFFFF" << endl;
    cout << "   -m <ramfile>  Load RAM content" << endl;
    cout << "   -l <addr>          ... at this address" << endl;
    cout << "   -t <tapefile> Use this file as virtual casette tape (very silly, uses 4MB just for the sync header)" << endl;
    // cout << "   -i <source>   Load assembler source file (not implemented yet)" << endl;
    // cout << "   -o <objname>  Save object file (not implemented yet)" << endl;

}

/**
 * Main entry point
 * @param argc number of command line arguments
 * @param argv pointer to array with arguments, arg0 is the program name
 * @return
 */
int main(const int argc, char *const argv[]) {
    haiku6502::engine_setup setup{};

    int ch;
    while ((ch = getopt(argc, argv, "cdi:l:o:r:t:")) != -1) {
        switch (ch) {
            case 'c':
                setup.console_mode = true;
                break;
            case 'd':
                setup.debug = true;
                break;
            case 'i':
                setup.source = std::string(optarg);
                break;
            case 'o':
                setup.out = std::string(optarg);
                break;
            case 'r':
                setup.rom = std::string(optarg);
                break;
            case 'm':
                setup.ram = std::string(optarg);
                break;
            case 'l':
                setup.ram_load_address = std::stoi(std::string(optarg));
            case 't':
                setup.tape_file = std::string(optarg);
                break;
            case '?':
            default:
                usage(argv[0]);
        }
    }

    if (!haiku6502::check_setup(setup)) {
        usage(argv[0]);
        return 1;
    }

    haiku6502::Engine engine(setup);

    engine.register_terminal(new haiku6502::Terminal(setup));

    engine.run();

    return 0;
}