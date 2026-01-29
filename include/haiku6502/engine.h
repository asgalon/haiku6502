//
// Created by Peter Köllner on 22/1/26.
//

#ifndef HAIKU6502_ENGINE_H
#define HAIKU6502_ENGINE_H

#include <haiku6502/cpu.h>
#include <haiku6502/memory.h>

#include "peripheral.h"
#include "setup.h"
#include "terminal.h"

namespace haiku6502 {

    class Engine : protected CpuStatus {

    protected:
        //
        // location of current cycle instruction
        //
        uint16_t cursor;

        //
        // The terminal
        // replaces memory mapped screen layout.
        //
        Terminal *terminal;

        // 8 slots of peripheral hardware,
        // slot #0 has restrictions and is only useful
        // for memory extensions and such. not really
        // implemented right now
        Peripheral *slots[8]{};

        bool debug { false };

        bool irq_request { false };
        bool nmi_request { false };

        void push_stack(uint8_t byte) {
            s = s - 1;
            ram[0x100 | s] = byte;
        }

        void push_stackw(uint16_t word) {
            push_stack(word >> 8);
            push_stack(word & 0x0FF);
        }

        uint8_t pull_stack() {
            return ram[0x100 | s++];
        }

        uint16_t pull_stackw() {
            uint16_t word = pull_stack();
            word |= pull_stack() << 8;
            return word;
        }

        void set_status_nz(uint8_t b);
        void set_status_ov(uint8_t result, uint8_t arg1, uint8_t arg2) {
            bool ov = ((arg1 & 0x80) == (arg2 & 0x80)) && ((result & 0x80) != (arg1 & 0x80));
            if (ov) {
                p |= STATUS_OVERFLOW;
            } else {
                p &= ~STATUS_OVERFLOW;
            }
        }

        void write_result(AddressMode mode, uint8_t arg1, uint8_t arg2, uint8_t byte);

        bool is_bcd() const {
            return (p & STATUS_DECIMAL) != 0;
        }

        bool carry() const {
            return (p & STATUS_CARRY) != 0;
        }

        uint8_t carry_bit() const {
            return p & STATUS_CARRY;
        }
        uint8_t not_carry_bit() const {
            return ~p & STATUS_CARRY;
        }

        bool negative() const {
            return (p & STATUS_NEGATIVE) != 0;
        }

        bool overflow() const {
            return (p & STATUS_OVERFLOW) != 0;
        }

        bool zero() const {
            return (p & STATUS_ZERO) != 0;
        }

        static int16_t relative(uint8_t arg) {
            if ((arg & 0x80) == 0) {
                return arg;
            }
            // Two's complement for negative values
            // Well, would be easier in hardware...
            int16_t neg = 0xFFFFFF00;
            neg |= arg;

            return neg;
        }

        void execute();

        uint8_t *ram;
        uint8_t *rom;

        bool shutdown { false };

        void set_status_flag(StatusFlag flag, bool newState);

    public:
        Engine(const engine_setup& setup);
        ~Engine();

        void set_debug(bool to = true) {
            debug = to;
        }

        uint16_t get_cursor() {
            return cursor;
        }

        void load_ram(uint16_t addr, uint16_t len, const char *filepath) const;

        void register_terminal(Terminal *t);
        void register_peripheral(int slot, Peripheral *peripheral);
        static void remove_peripheral(Peripheral * &slot);

        void run();
        void quit();

        void reset();
        // non maskable interrupt
        void nmi();
        // maskable interrupt
        void irq(uint8_t interrupt);

        // perform an operation and set pc
        int cycle();

        void set_byte(uint16_t address, uint8_t value);
        uint8_t get_byte(uint16_t address);
        uint8_t get_io_byte(uint8_t address) const;
        uint8_t get_peripheral_rom_byte(uint16_t address) const;
        uint8_t get_peripheral_expansion_rom_byte(uint16_t address);

        uint8_t get_param(AddressMode mode, uint8_t arg1, uint8_t arg2);

        static uint16_t address(uint8_t lsb, uint8_t msb) {
            uint16_t addr = msb;
            addr <<= 8;
            addr |= lsb;

            return addr;
        }

        uint16_t indirect(uint16_t addr) {
            return address(static_cast<uint8_t>(get_byte(addr)), static_cast<uint8_t>(get_byte(addr + 1)));
        }

        void get_state(CpuStatus& state) const {
            state.a = a;
            state.p = p & ~STATUS_IGNORE;
            state.pc = pc;
            state.s = s;
            state.x = x;
            state.y = y;
        }

                // Hard reset. map in hard reset vectors and set pc to 0
        void hard_reset();
        // soft reset. set pc to 0
        void soft_reset();

        void op_adc(uint8_t param);
        void op_and(uint8_t param);
        void op_asl(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_bcc(uint8_t arg);
        void op_bcs(uint8_t arg);
        void op_beq(uint8_t arg);
        void op_bit(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_bmi(uint8_t arg);
        void op_bne(uint8_t arg);
        void op_bpl(uint8_t arg);
        void op_brk();
        void op_bvc(uint8_t arg);
        void op_bvs(uint8_t arg);
        void op_clc();
        void op_cld();
        void op_cli();
        void op_clv();
        void op_cmp(uint8_t param);
        void op_cpx(uint8_t param);
        void op_cpy(uint8_t param);
        void op_dec(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_dex();
        void op_dey();
        void op_eor(uint8_t param);
        void op_inc(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_inx();
        void op_iny();
        void op_jmp(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_jsr(uint8_t arg1, uint8_t arg2);
        void op_lda(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_ldx(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_ldy(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_lsr(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_nop();
        void op_ora(uint8_t param);
        void op_pha();
        void op_php();
        void op_pla();
        void op_plp();
        void op_rol(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_ror(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_rti();
        void op_rts();
        void op_sbc(AddressMode addr, uint8_t param);
        void op_sec();
        void op_sed();
        void op_sei();
        void op_sta(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_stx(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_sty(AddressMode addr, uint8_t arg1, uint8_t arg2);
        void op_tax();
        void op_tay();
        void op_tsx();
        void op_txa();
        void op_txs();
        void op_tya();

    };
}

#endif //HAIKU6502_ENGINE_H