//
// Created by Peter Köllner on 24/1/26.
//

#ifndef HAIKU6502_TERMINAL_H
#define HAIKU6502_TERMINAL_H
#include <cstdint>
#include <ncurses.h>
#include <queue>
#include <haiku6502/device.h>
#include <chrono>

namespace haiku6502 {
    class Terminal : public Device {
        typedef std::chrono::high_resolution_clock Time;

        bool text = true;
        bool low_resolution = true;
        bool primary = true;
        bool mixed = false;
        std::queue<uint8_t> key_pressed;

        int cursor_x = 0;
        int cursor_y = 0;
        int win_h = 40;
        int win_w = 24;
        int current_attributes = A_NORMAL;
        chtype ch = ' ';
        bool key_busy = false;
        std::chrono::time_point<std::chrono::high_resolution_clock> key_pushed;

        bool debug = false;

        static void check_err(int err);
    protected:
        void device_init() override;
        void device_close() override;
    public:
        Terminal(bool dbg);
        ~Terminal() override;

        void set_debug(bool dbg) {
            debug = dbg;
        }

        void print_geom() const;

        void pre_cycle() override;
        void post_cycle() override;

        void key_map(int key);

        bool get_io_byte(uint8_t io_address, uint8_t& result);
        bool set_io_byte(uint8_t io_address, const uint8_t &value);
    };
}

#endif //HAIKU6502_TERMINAL_H