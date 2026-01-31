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
        std::string rom;
        std::string ram;
        uint16_t ram_load_address;
        uint16_t ram_start_address;
        std::string tape_file;
        bool console_mode { false };

        bool debug { false };
    };

    bool check_setup(const engine_setup& setup);
}
#endif //HAIKU6502_SETUP_H