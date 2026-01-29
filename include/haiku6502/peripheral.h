//
// Created by Peter Köllner on 24/1/26.
//

#ifndef HAIKU6502_PERIPHERAL_H
#define HAIKU6502_PERIPHERAL_H
#include <cstdint>
#include <cstring>

#include "device.h"

namespace haiku6502 {
    namespace Error {
        struct peripheral_error {};
    }

    class Engine;

    /*
     * Interface for peripheral devices.
     * Access ROM and I/O bytes, plus interrupt handling.
     */
    class Peripheral : public Device {
        // 256 bytes of ROM at 0xCn00-0xCnFF for slot n
        // slot0 does not have a rom area
        uint8_t rom[256] = {};
        Engine *eng = nullptr;
    public:
        // init is called when peripheral is added to the engine
        void init(Engine *_eng) {
            eng = _eng;
            device_init();
        }

        // close is called when peripheral is removed from the engine
        void close() {
            device_close();
            eng = nullptr;
        }

        void init_rom(const uint8_t *src, size_t len) {
            memcpy(rom, src, len);
        }

        // Access ROM location 0xCn00 + location 00-FF
        // make sure ROM routines are relocatable.
        uint8_t mem(uint8_t location) const {
            return rom[location];
        }

        // Access extension ROM location 0xC800 + location 00-FF
        // make sure ROM routines are relocatable.
        virtual uint8_t expansion_rom(uint8_t location) const {
            return 0;
        }

        // Get I/O byte at 0xC0sl where
        // s = 8 + slot number
        // l = location & 0x0F.
        // peripheral should only use the lower nibble of the location byte.
        virtual uint8_t get_io(uint8_t location) = 0;
        virtual void set_io(uint8_t location, uint8_t value) = 0;

        virtual bool has_expansion_rom() {
            return false;
        }

        // TODO interrupt handling
    };
}

#endif //HAIKU6502_PERIPHERAL_H