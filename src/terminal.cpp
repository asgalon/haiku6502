//
// Created by Peter Köllner on 24/1/26.
//

#include <clocale>
#include <cstring>
#include <haiku6502/terminal.h>
#include <ncurses.h>
#include <iostream>

#include "haiku6502/peripheral.h"

#include <haiku6502/setup.h>

#include <readline/readline.h>
#include <readline/history.h>

namespace haiku6502 {
    using namespace std;

    Terminal::Terminal(const engine_setup& setup) : tape_filepath(setup.tape_file),
            stdio_mode(setup.console_mode), debug(setup.debug) {

        setlocale(LC_ALL, "");
        if (!stdio_mode) {
            initscr();
            check_err(cbreak());
            check_err(noecho());
            noqiflush();
            check_err(keypad(stdscr, TRUE));
            check_err(nodelay(stdscr, TRUE));
            check_err(start_color());
            check_err(scrollok(stdscr, FALSE));
            check_err(scrollok(stdscr, TRUE));
            getmaxyx(stdscr, win_h, win_w);  // get max y,x int height and width (must add 1);
            if (win_h > 255) {
                win_h = 255;
            }
            if (win_w > 255) {
                win_w = 255;
            }
            mvaddstr(1,0, "Haiku6502");
            mvaddstr(2,0, "v0.8b");
            mvaddstr(3,0, "1979/03/03");
            refresh();
        } else {
            cout << "Haiku6502" << endl;
            cout << "v0.8b" << endl;
            cout << "1979/03/03" << endl;
        }
    }

    Terminal::~Terminal() {
        close_tape();
        if (!stdio_mode) {
            endwin();
        }
    }

    void Terminal::check_err(int err) {
        if (err != OK) {
            throw new Error::peripheral_error();
        }
    }

    void Terminal::print_geom() const {
        if (!stdio_mode && debug) {
            char geom[48];
            int y,x;
            getyx(stdscr, y, x);
            snprintf(geom, 24, "wy: %02x wx: %02x", y, x);
            mvaddstr(1, win_w -25, geom);
            snprintf(geom, 24, "cy: %02x cx: %02x", cursor_y, cursor_x);
            mvaddstr(2, win_w -25, geom);
            snprintf(geom, 24, "h: %02x w: %02x", win_h, win_w);
            mvaddstr(3, win_w -25, geom);
            move(cursor_y, cursor_x);
            refresh();
        }
    }

    void Terminal::key_map(const int key) {
        switch (key) {
            case 0x0a:
            case KEY_ENTER:
                key_pressed.push(0x0A);
                break;
            case KEY_RIGHT:
                key_pressed.push(0x08);
                break;
            case KEY_LEFT:
                key_pressed.push(0x15);
                break;
            default:
                if (key <= 0x7E) {
                    key_pressed.push(key);
                }
        }
    }


    void Terminal::device_init() {
        // first one: super init()

    }

    void Terminal::device_close() {
        // last one: super close()
    }

    void Terminal::pre_cycle() {
        int ch = getch();

        if (ch != ERR) {
            key_map(ch);
        }

    }

    //
    // Simulates an ongoing tape write
    // the output channel has only two levels, 0 and 255.
    // so this produces a square wave with different lengths
    // for 0s and 1s. While the tape operation is ongoing,
    // this function writes one byte of the current state
    // to the tape file. This tape file can then be stored as an audio file.
    // With the right sampling rate, this should work with real hardware...
    // Well, this is probably the silliest emulator feature I ever wrote :)
    void Terminal::post_tick() {
        if (!tape_filepath.empty() && tape_on) {
            if (tape_is_write) {
                if (!tape.is_open()) {
                    tape.open(tape_filepath, std::ios_base::out);
                }
                if (tape.is_open()) {
                    tape << tapevalue;
                }
            } else if (tape.is_open()) {
                if (!tape.eof()) {
                    uint8_t result;  // skip tape byte for tick delay on read.
                    tape >> result;
                } else {
                    close_tape();
                }
            }
        }
    }

    void Terminal::post_cycle() {

    }

    //
    constexpr uint8_t kbd       = 0x00;
    constexpr uint8_t kbdstrb   = 0x01;
    constexpr uint8_t rnd       = 0x0F;
    constexpr uint8_t termcy    = 0x10;
    constexpr uint8_t termcx    = 0x11;
    constexpr uint8_t termout   = 0x12;
    constexpr uint8_t termesc   = 0x13; // esc / control op
    constexpr uint8_t termea1   = 0x14; // esc param 1
    constexpr uint8_t termea2   = 0x15; // esc param 2
    constexpr uint8_t termwh    = 0x16; // window height
    constexpr uint8_t termww    = 0x17; // window width
    constexpr uint8_t termin    = 0x18; // stdio read char
    constexpr uint8_t terml     = 0x19; // stdio read line
    constexpr uint8_t termp     = 0x1A; // stdio prompt
    constexpr uint8_t termmd    = 0x1F; // terminal mode; 0 - ncurses, 1 - stdio
    constexpr uint8_t tapeout   = 0x20; // tape out
    constexpr uint8_t tapein    = 0x21; // tape on
    constexpr uint8_t tapecls   = 0x22; // tape out

    //
    //
    //  Escape commands for terminal:
    //
    enum term_commands {
        t_cls,      // 0 - Clear screen
        t_cll,      // 1 - Clear to eol
        t_clb,      // 2 - clear from cursor pos to bottom
        t_scr,      // 3 - scroll one line up
        t_norm,     // 4 - set normal char mode
        t_inv,      // 5 - set inverse character mode
        t_blnk,     // 6 - set blinking character mode
        t_qcch,     // 7 - query current character

    };
    // constexpr uint8_t  t_cls   = 0x00;
    // constexpr uint8_t  t_txt   = 0x01;  // Text mode
    // constexpr uint8_t  t_gr    = 0x02;  // Graphics mode
    // constexpr uint8_t  t_cll   = 0x03;  // clear from cursor to end of line
    // constexpr uint8_t  t_qh    = 0x04;  // query terminal height; for result read terminl,h
    // constexpr uint8_t  t_qw    = 0x05;  // query terminal width; for result read terminl,h
    // constexpr uint8_t  t_qy    = 0x04;  // query terminal cursor y; for result read terminl,h
    // constexpr uint8_t  t_qx    = 0x05;  // query terminal cursor x; for result read terminl,h


    bool Terminal::get_io_byte(uint8_t io_address, uint8_t& result) {
        if (io_address == kbd) {
            // 0xC000 - keyboard value
            if (!stdio_mode) {
                if (!key_pressed.empty()) {
                    result = key_pressed.front();
                    key_busy = true;
                } else {
                    result = 0;
                }
            }
            return true;
        }
        if (io_address == kbdstrb) {
            if (!stdio_mode) {
                // 0xC001 - clear keyboard strobe
                key_pressed.pop();
                key_busy = false;
            }
            return true;
        }
        if (io_address == rnd) {
            result = dist256(gen);
            return true;
        }
        if (io_address == termout) {
            if (!stdio_mode) {
                chtype ch = inch();
                result = ch | A_CHARTEXT;
            }
            return true;
        }
        if (io_address == termwh) {
            // terminal window height
            result = win_h;
            return true;
        }
        if (io_address == termww) {
            // terminal window width
            result = win_w;
            return true;
        }
        if (io_address == termmd) {
            // terminal mode
            result = stdio_mode ? 1 : 0;
            return true;
        }
        if (io_address == termin) {
            cin >> result;
            return true;
        }
        if (io_address == terml) {
            // terminal mode
            if (stdio_mode) {
                if (lineptr == -1) {
                    strncpy(stdio_line, readline(&stdio_prompt), 256);
                    lineptr = 0;
                }
                if (lineptr != -1) {
                    result = toupper(stdio_line[lineptr]);
                    if (result == 0) {
                        lineptr = -1;
                    } else {
                        lineptr++;
                    }
                }
            }
            return true;
        }
        if (io_address == termp) {
            result = stdio_prompt;
            return true;
        }
        if (io_address == 0x30) {
            // 0xC030 Speaker
            return true;
        }
        if (io_address == tapein) {
            // read tape byte
            prepare_read();

            if (tape_on == true && tape.is_open() && !tape_is_write) {
                if (!tape.eof()) {
                    tape >> result;
                } else {
                    close_tape();
                }
            }

            return true;
        }

        if (io_address == tapeout) {
            // toggle tape byte
            if (!tape_on && !tape_filepath.empty()) {
                tape_on = true;
                tape_is_write = true;
            }
            tapevalue = ~tapevalue;

            return true;
        }
        if (io_address == tapecls) {
            // read tape byte
            close_tape();
        }

        return false;  // allows other components to claim i/o ports
    }

    void Terminal::prepare_read() {
        if (!tape_is_write && !tape.is_open() && !tape_filepath.empty()) {
            tape.open(tape_filepath, std::ios_base::in);
            tape.seekg(5000);  // skip 4 sec worth of header
            tape_on = true;
        }
    }

    void Terminal::close_tape() {
        if (tape.is_open() ) {
            tape.flush();
            tape.close();
        }
        tape_on = false;
        tape_is_write = false;
    }

    bool Terminal::set_io_byte(const uint8_t io_address, const uint8_t& value) {
        if (io_address == termcy) {
            // terminal cursor y
            cursor_y= value;
            return true;
        }
        if (io_address == termcx) {
            // terminal cursor x
            cursor_x = value;
            move(cursor_y, cursor_x);
            return true;
        }
        if (io_address == termout) {
            // terminal output
            if (stdio_mode) {
                cout << value << flush;
            } else {
                if (value >= 0x20 && value <= 0xFE) {
                    chtype ch = static_cast<chtype>(value) | current_attributes;
                    echochar(ch | current_attributes);
                    print_geom();
                } else if (value < 0x20) {
                    switch (value) {
                        case 0x07:
                            beep();
                        default:
                            ;
                    }
                }
                refresh();
            }
            return true;
        }
        if (io_address == termesc) {
            // terminal shortcut ESC sequences
            if (stdio_mode) {

            } else {
                switch (value) {
                    case t_cls:
                        clear();
                        break;
                    case t_cll:
                        clrtoeol();
                        break;
                    case t_clb:
                        clrtobot();
                        break;
                    case t_scr:
                        scroll(stdscr);
                        break;
                    case t_norm:
                        current_attributes = A_NORMAL;
                        break;
                    case t_inv:
                        current_attributes |= A_REVERSE;
                        break;
                    case t_blnk:
                        current_attributes |= A_BLINK;
                        break;
                }
                refresh();
            }
            return true;

        }
        if (io_address == termp) {
            stdio_prompt = value;
            return true;
        }

        return false;
    }

}
