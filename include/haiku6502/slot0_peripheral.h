//
// Created by Peter Köllner on 25/1/26.
//

#ifndef HAIKU6502_KEYBOARD_H
#define HAIKU6502_KEYBOARD_H
#include "peripheral.h"

namespace haiku6502 {
    /**
     * this is for slot 0. It deals with the i/o area of 0xC000-0xC0FF
     * It does not have a rom area. TODO: refactor peripheral to remove the rom array here.
     */
    class Slot0Peripheral : public Peripheral {
        static uint8_t key_map(int key);
    public:
        uint8_t get_io(uint8_t location) override;
        void set_io(uint8_t location, uint8_t value) override;
    };
}
#endif //HAIKU6502_KEYBOARD_H