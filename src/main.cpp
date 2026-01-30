#include <cstdio>
#include <cstring>
#include <unistd.h>

#include "../include/haiku6502/setup.h"
#include "haiku6502/engine.h"
#include "haiku6502/slot0_peripheral.h"
#include "haiku6502/terminal.h"

static void usage(char *progname) {
    printf("Usage: %s -r <romfile.rom> [-m <ramfile.ram>]  [-l <load address>] -t <tape.data>[-i <sourcefile>] [-o <outputfile>]\n", progname);
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
    while ((ch = getopt(argc, argv, "di:l:o:r:t:")) != -1) {
        switch (ch) {
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