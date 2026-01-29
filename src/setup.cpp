//
// Created by Peter Köllner on 22/1/26.
//
#include <haiku6502/setup.h>

#include <string>

namespace haiku6502 {
    bool check_setup(const engine_setup& setup) {
        if (setup.rom.empty()) {
            return false;
        }
        return true;
    }
}