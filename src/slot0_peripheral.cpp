//
// Created by Peter Köllner on 25/1/26.
//
#include <ncurses.h>
#include <haiku6502/slot0_peripheral.h>

namespace haiku6502 {

    uint8_t Slot0Peripheral::get_io(uint8_t location) {

        return 0;
    }

    void Slot0Peripheral::set_io(uint8_t location, uint8_t value) {
        // for now, just do geT_location twice... TODO pause
        get_io(location);
        get_io(location);
    }
}
