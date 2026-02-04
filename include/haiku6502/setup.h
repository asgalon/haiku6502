//
// Created by Peter Köllner on 22/1/26.
//

#ifndef HAIKU6502_SETUP_H
#define HAIKU6502_SETUP_H

#include <string>

namespace haiku6502 {
    struct engine_setup {
        std::string source;
        std::string out;
        std::string rom = "./data/standard.rom";
        std::string ram;
        uint16_t ram_load_address = 0x1000;
        uint16_t ram_start_address = 0x1000;
        std::string tape_file = "./tape.data";

        bool console_mode { false };

        bool debug { false };
    };

    bool check_setup(const engine_setup& setup);
}
#endif //HAIKU6502_SETUP_H