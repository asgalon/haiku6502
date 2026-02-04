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
#include <fstream>
#include <random>

namespace haiku6502 {
    struct engine_setup;

    class Terminal : public Device {
        typedef std::chrono::high_resolution_clock Time;

        std::queue<uint8_t> key_pressed;

        int cursor_x = 0;
        int cursor_y = 0;
        int win_h = 40;
        int win_w = 24;
        int current_attributes = A_NORMAL;

        bool key_busy = false;
        std::chrono::time_point<std::chrono::high_resolution_clock> key_pushed;

        std::string tape_filepath;
        std::fstream tape = std::fstream();

        bool stdio_mode = false;
        char stdio_line[256];
        int lineptr = -1;
        char stdio_prompt;
        bool tape_on = false;
        bool tape_is_write = false;
        bool debug = false;

        std::random_device rnd_dev;
        std::mt19937 gen = std::mt19937(rnd_dev());
        std::uniform_int_distribution<std::mt19937::result_type> dist256 = std::uniform_int_distribution<std::mt19937::result_type>(0,255); // distribution in range [1, 6]


        static void check_err(int err);
    protected:
        void device_init() override;
        void device_close() override;
    public:
        Terminal(const engine_setup& setup);
        ~Terminal() override;

        void set_debug(bool dbg) {
            debug = dbg;
        }

        void print_geom() const;

        void pre_cycle() override;
        void post_tick() override;

        uint8_t tape_read();

        void tape_write(uint8_t value);

        void post_cycle() override;

        void key_map(int key);

        bool get_io_byte(uint8_t io_address, uint8_t& result);

        void prepare_read();

        void close_tape();

        bool set_io_byte(uint8_t io_address, const uint8_t &value);
    };
}

#endif //HAIKU6502_TERMINAL_H