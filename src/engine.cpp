//
// Created by Peter Köllner on 22/1/26.
//

#include <haiku6502/engine.h>

#include <fstream>
#include <iostream>
#include <ncurses.h>
#include <unistd.h>

namespace haiku6502 {
    using namespace std;

    Engine::Engine(const engine_setup& setup) {
        debug = setup.debug;

        // 48 KB RAM
        ram = new uint8_t[0xC000];
        // 4 KB I/O Addresses at 0xC000-0xCFFF inbetween...
        // 12 KB ROM
        rom = new uint8_t[0x3000];

        // clock cycle tick waiting time - well this is a 6502, you know?
        req.tv_sec = 0;
        req.tv_nsec = 800;

        // read the rom from file. should be exactly 0x3000 = 12KB = 12288 bytes long
        // longer files will be truncated, shorter files will probably not set the three
        // important interrupt and reset vectors at 0xFFFA-0xFFFF
        ifstream rom_stream(setup.rom, ios::in|ios::binary); // binary in, go to end

        if (debug) {
            char dir[PATH_MAX];

            getwd(dir);

            cout << dir << '\n';
        }

        if (rom_stream.is_open()) {
            rom_stream.read(reinterpret_cast<istream::char_type *>(rom), 0x3000); // read file into rom area up to 0x3000 chars

            rom_stream.close();
        }
    }

    Engine::~Engine() {
        for (auto & slot : slots) {
            remove_peripheral(slot);
        }
        delete terminal;
        terminal = nullptr;
        delete ram;
        delete rom;
    }

    void Engine::load_ram(uint16_t addr, uint16_t len, const char *filepath) const {
        if (addr < 0xC000 && addr + len < 0xC00) {
            std::ifstream in_stream(filepath, std::ios::in| std::ios::binary); // binary in, go to end

            if (in_stream.is_open()) {
                in_stream.read(reinterpret_cast<istream::char_type *>(&ram[addr]), len); // read file into rom area up to len chars

                in_stream.close();
            }
        }
    }

    //
    // Add the terminal component
    //
    void Engine::register_terminal(Terminal *t) {
        terminal = t;
    }

    //
    // Register further I/O peripherals in one of the slots
    //
    void Engine::register_peripheral(int slot, Peripheral *peripheral) {
        // replace whatever had been put there
        remove_peripheral(slots[slot]);
        slots[slot] = peripheral;
        peripheral->init(this);
    }

    //
    // Remove a peripheral
    //
    void Engine::remove_peripheral(Peripheral * &slot) {
        if (slot != nullptr) {
            try {
                slot->close();
            } catch (Error::peripheral_error e) {
                // just ignore for now
            }
            delete slot;
            slot = nullptr;
        }
    }

    //
    // Min entry point
    //
    void Engine::run() {
        this->execute();
    }

    //
    // quit the engine
    //
    void Engine::quit() {
        shutdown = true;
    }

    //
    // Execute main cpu loop
    //
    void Engine::execute() {
        pc = indirect(RESET_VECTOR);

        if (terminal == nullptr) {
            cerr << "No terminal I/O set up, bailing out." << endl;
            shutdown = true;
        }
        irq_request = false;

        while (!shutdown) {
            if (debug) {
                //char str[16];
                //if (pc < 0xfd1b || pc > 0xfd24)
                //printf("pc: 0x%4hx  a: 0x%02hhx  x: 0x%02hhx  y: 0x%02hhx \n", pc, a, x, y);
                //mvaddstr(2, 0, str);
            }

            terminal->pre_cycle();

            int op_cycles = cycle();

            for (int i = 0; i < op_cycles; i++) {
                nanosleep(&req, nullptr);

                terminal->post_tick();  //  repeated each clock tick, 2-7 per instruction
            }

            terminal->post_cycle();  // before or after interrupt handling?

            if (nmi_request) {
                nmi_request = false;
                pc = indirect(NMI_VECTOR);
            } else if ((p & STATUS_INTERRUPT_DISABLE) == 0 && irq_request) {
                irq_request = false;
                pc = indirect(IRQ_VECTOR);
            }
        }
    }

    /*
     * Reset. Load PC from 6502 reset location at 0xFFFC (lsb) and 0xFFFD (msb)
     */
    void Engine::reset() {
        pc = rom[0x2FFC] | rom[0x2FFD] << 8;
    }

    void Engine::nmi() {
        set_status_flag(STATUS_BREAK, false);
        push_stackw(pc);
        push_stack(p);
        nmi_request = true;
    }

    void Engine::irq(uint8_t interrupt) {
        set_status_flag(STATUS_BREAK, false);
        push_stackw(pc);
        push_stack(p);
        irq_request = true;
    }

    int Engine::cycle() {
        cursor = pc;

        if (cursor == 0xFFFF) {
            shutdown = true;        // shutdown emulator
            return 0;               // no cpu cycles spent
        }

        const uint8_t op = get_byte(pc++);
        uint8_t arg1 = 0;
        uint8_t arg2 = 0;

        Instruction i = operations[op];

        int ticks = i.cycles;

        if (i.length > 1) {
            arg1 = get_byte(pc++);
        }
        if (i.length > 2) {
            arg2 = get_byte(pc++);
        }
        switch (i.op) {
            case ADC:
                op_adc(get_param(i.addr, arg1, arg2), ticks);
                break;
            case AND:
                op_and(get_param(i.addr, arg1, arg2), ticks);
                break;
            case ASL:
                op_asl(i.addr, arg1, arg2);
                break;
            case BCC:
                op_bcc(arg1, ticks);
                break;
            case BCS:
                op_bcs(arg1, ticks);
                break;
            case BEQ:
                op_beq(arg1, ticks);
                break;
            case BIT:
                op_bit(i.addr, arg1, arg2);
                break;
            case BMI:
                op_bmi(arg1, ticks);
                break;
            case BNE:
                op_bne(arg1, ticks);
                break;
            case BPL:
                op_bpl(arg1, ticks);
                break;
            case BRK:
                op_brk();
                break;
            case BVC:
                op_bvc(arg1, ticks);
                break;
            case BVS:
                op_bvs(arg1, ticks);
                break;
            case CLC:
                op_clc();
                break;
            case CLD:
                op_cld();
                break;
            case CLI:
                op_cli();
                break;
            case CLV:
                op_clv();
                break;
            case CMP:
                op_cmp(get_param(i.addr, arg1, arg2), ticks);
                break;
            case CPX:
                op_cpx(get_param(i.addr, arg1, arg2));
                break;
            case CPY:
                op_cpy(get_param(i.addr, arg1, arg2));
                break;
            case DEC:
                op_dec(i.addr, arg1, arg2);
                break;
            case DEX:
                op_dex();
                break;
            case DEY:
                op_dey();
                break;
            case EOR:
                op_eor(get_param(i.addr, arg1, arg2), ticks);
                break;
            case INC:
                op_inc(i.addr, arg1, arg2);
                break;
            case INX:
                op_inx();
                break;
            case INY:
                op_iny();
                break;
            case JMP:
                op_jmp(i.addr, arg1, arg2, ticks);
                break;
            case JSR:
                op_jsr(arg1, arg2);
                break;
            case LDA:
                op_lda(i.addr, arg1, arg2, ticks);
                break;
            case LDX:
                op_ldx(i.addr, arg1, arg2, ticks);
                break;
            case LDY:
                op_ldy(i.addr, arg1, arg2, ticks);
                break;
            case LSR:
                op_lsr(i.addr, arg1, arg2);
                break;
            case NOP:
                op_nop();
                break;
            case ORA:
                op_ora(get_param(i.addr, arg1, arg2), ticks);
                break;
            case PHA:
                op_pha();
                break;
            case PHP:
                op_php();
                break;
            case PLA:
                op_pla();
                break;
            case PLP:
                op_plp();
                break;
            case ROL:
                op_rol(i.addr, arg1, arg2);
                break;
            case ROR:
                op_ror(i.addr, arg1, arg2);
                break;
            case RTI:
                op_rti();
                break;
            case RTS:
                op_rts();
                break;
            case SBC:
                op_sbc(i.addr, get_param(i.addr, arg1, arg2), ticks);
                break;
            case SEC:
                op_sec();
                break;
            case SED:
                op_sed();
                break;
            case SEI:
                op_sei();
                break;
            case STA:
                op_sta(i.addr, arg1, arg2);
                break;
            case STX:
                op_stx(i.addr, arg1, arg2);
                break;
            case STY:
                op_sty(i.addr, arg1, arg2);
                break;
            case TAX:
                op_tax();
                break;
            case TAY:
                op_tay();
                break;
            case TSX:
                op_tsx();
                break;
            case TXA:
                op_txa();
                break;
            case TXS:
                op_txs();
                break;
            case TYA:
                op_tya();
                break;
            default:
                ;
        }
        return ticks;
    }

    uint8_t Engine::get_param(const AddressMode mode, const uint8_t arg1, const uint8_t arg2) {
        uint8_t param = 0;

        switch (mode) {
            case ACCU:
                param = a;
                break;
            case IMMEDIATE:
                param = arg1;
                break;
            case ZERO:
                param = get_byte(arg1);
                break;
            case ZERO_X: {
                uint8_t addr = arg1 + x;
                param = get_byte(addr);
            }
                break;
            case ZERO_Y: {
                uint8_t addr = arg1 + y;
                param = get_byte(addr);
            }
                break;
            case ABSOLUTE: {
                uint16_t addr = address(arg1, arg2);
                param = get_byte(addr);
            }
                break;
            case ABSOLUTE_X: {
                param = get_byte(address(arg1, arg2) + x);
            }
                break;
            case ABSOLUTE_Y: {
                param = get_byte(address(arg1, arg2) + y);
            }
                break;
            case INDEXED_INDIRECT_X: {
                param = get_byte(indirect(address(arg1, arg2) + x));
            }
                break;
            case INDIRECT_INDEXED_Y: {
                param = get_byte(indirect(address(arg1, arg2)) + y);
            }
                break;
            case ABSOLUTE_INDIRECT: {
                param = get_byte(indirect(address(arg1, arg2)));
            }
                break;
            case RELATIVE:
                param = pc + arg1;
                break;
            default:
                ;
        }
        return param;
    }

    void Engine::op_adc(uint8_t param, int& ticks) {
        // see http://6502.org/tutorials/decimal_mode.html
        if (!is_bcd()) {
            const uint16_t oper = param + carry_bit();
            uint16_t probe = a + oper;
            set_status_ov(probe, a, oper);
            set_status_flag(STATUS_CARRY, probe > 0x0FF);
            a = probe & 0x0FF;
            set_status_nz(a);
        } else {
            uint8_t al = (a & 0x0F) + (param & 0x0f0F) + carry_bit();
            if (al >= 0x0A) {
                al = (al+0x06 & 0x0F) + 0x10;
            }
            uint16_t a_aux = (a & 0xF0) + (param & 0xF0) + al;
            // Note that a_aux can be >= 0x100 at this point
            if (a_aux >= 0xA0) {
                a_aux += 0x60;
            }
            a = a_aux & 0x0FF;
            set_status_flag(STATUS_CARRY, (a_aux >> 8) != 0);
        }
    }

    void Engine::op_and(uint8_t param, int& ticks) {
        a &= param;

        set_status_nz(a);

    }

    void Engine::op_asl(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        uint8_t oper = get_param(addr, arg1, arg2);

        set_status_flag(STATUS_CARRY, (oper & 0x80) != 0);
        oper <<= 1;

        set_status_flag(STATUS_NEGATIVE, (oper & 0x80) != 0);
        set_status_flag(STATUS_ZERO, oper == 0);

        write_result(addr, arg1, arg2, oper);
    }

    void Engine::op_bcc(uint8_t arg, int& ticks) {
        if (!carry()) {
            pc = pc + relative(arg);
            ticks++;
        }
    }

    void Engine::op_bcs(uint8_t arg, int &ticks) {
        if (carry()) {
            pc = pc + relative(arg);
            ticks++;
        }
    }

    void Engine::op_beq(uint8_t arg, int &ticks) {
        if (zero()) {
            pc = pc + relative(arg);
            ticks++;
        }
    }

    void Engine::op_bit(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        uint8_t oper = get_param(addr, arg1, arg2);
        set_status_flag(STATUS_NEGATIVE, (oper & STATUS_NEGATIVE) != 0);
        set_status_flag(STATUS_OVERFLOW, (oper & STATUS_OVERFLOW) != 0);
        set_status_flag(STATUS_ZERO, (a & oper) == 0);
    }

    void Engine::op_bmi(uint8_t arg, int &ticks) {
        if (negative()) {
            pc = pc + relative(arg);
            ticks++;
        }
    }

    void Engine::op_bne(uint8_t arg, int &ticks) {
        if (!zero()) {
            pc = pc + relative(arg);
            ticks++;
        }
    }

    void Engine::op_brk() {
        set_status_flag(STATUS_BREAK, true);
        push_stackw(pc);
        push_stack(p);
        irq_request = true;
    }

    void Engine::op_bpl(uint8_t arg, int &ticks) {
        if (!negative()) {
            pc = pc + static_cast<int8_t>(arg);
            ticks++;
        }
    }

    void Engine::op_bvc(uint8_t arg, int &ticks) {
        if (!overflow()) {
            pc = pc + relative(arg);
            ticks++;
        }
    }

    void Engine::op_bvs(uint8_t arg, int &ticks) {
        if (overflow()) {
            pc = pc + relative(arg);
            ticks++;
        }
    }

    void Engine::op_clc() {
        set_status_flag(STATUS_CARRY, false);
    }

    void Engine::op_cld() {
        set_status_flag(STATUS_DECIMAL, false);
    }

    void Engine::op_cli() {
        set_status_flag(STATUS_INTERRUPT_DISABLE, false);
    }

    void Engine::op_clv() {
        set_status_flag(STATUS_OVERFLOW, false);
    }

    void Engine::op_cmp(uint8_t param, int &ticks) {
        uint8_t cmp = a - param;
        set_status_flag(STATUS_CARRY, a >= param);
        set_status_nz(cmp);
    }

    void Engine::op_cpx(uint8_t param) {
        uint8_t cmp = x - param;
        set_status_flag(STATUS_CARRY, x >= param);
        set_status_nz(cmp);
    }

    void Engine::op_cpy(uint8_t param) {
        uint8_t cmp = y - param;
        set_status_flag(STATUS_CARRY, y >= param);
        set_status_nz(cmp);
    }

    void Engine::op_dec(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        uint8_t oper = get_param(addr, arg1, arg2);
        --oper;
        write_result(addr, arg1, arg2, oper);
        set_status_nz(oper);
    }

    void Engine::op_dex() {
        --x;
        set_status_nz(x);
    }

    void Engine::op_dey() {
        --y;
        set_status_nz(y);
    }

    void Engine::op_eor(uint8_t param, int &ticks) {
        a ^= param;

        set_status_nz(a);
    }

    void Engine::op_inc(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        uint8_t oper = get_param(addr, arg1, arg2);
        oper++;
        write_result(addr, arg1, arg2, oper);
        set_status_nz(oper);
    }

    void Engine::op_inx() {
        x++;
        set_status_nz(x);
    }

    void Engine::op_iny() {
        y++;
        set_status_nz(y);
    }

    void Engine::op_jmp(AddressMode addr, uint8_t arg1, uint8_t arg2, int &ticks) {
        if (addr == ABSOLUTE_INDIRECT) {
            pc = indirect(address(arg1, arg2));
        } else {
            pc = address(arg1, arg2);
        }
    }

    void Engine::op_jsr(uint8_t arg1, uint8_t arg2) {
        push_stackw(pc); // pc already advanced

        pc = address(arg1, arg2);
    }

    void Engine::op_lda(AddressMode addr, uint8_t arg1, uint8_t arg2, int &ticks) {
        a = get_param(addr, arg1, arg2);
        set_status_nz(a);
    }

    void Engine::op_ldx(AddressMode addr, uint8_t arg1, uint8_t arg2, int &ticks) {
        x = get_param(addr, arg1, arg2);
        set_status_nz(x);
    }

    void Engine::op_ldy(AddressMode addr, uint8_t arg1, uint8_t arg2, int &ticks) {
        y = get_param(addr, arg1, arg2);
        set_status_nz(y);
    }

    void Engine::op_lsr(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        uint8_t oper = get_param(addr, arg1, arg2);

        set_status_flag(STATUS_NEGATIVE, false);
        set_status_flag(STATUS_CARRY, (oper & 0x01) == 1);
        oper >>= 1;

        set_status_flag(STATUS_ZERO, oper == 0);

        write_result(addr, arg1, arg2, oper);
    }

    void Engine::op_nop() {
        // TODO debug stuff
    }

    void Engine::op_ora(uint8_t param, int &ticks) {
        a |= param;

        set_status_nz(a);
    }

    void Engine::op_pha() {
        push_stack(a);
    }

    void Engine::op_php() {
        set_status_flag(STATUS_BREAK, true);
        push_stack(p);
    }

    void Engine::op_pla() {
        a = pull_stack();
        set_status_nz(a);
    }

    void Engine::op_plp() {
        p = pull_stack() & ~STATUS_BREAK;
    }

    void Engine::op_rol(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        uint8_t oper = get_param(addr, arg1, arg2);

        uint8_t carry_in = carry_bit();

        set_status_flag(STATUS_CARRY, (oper & 0x80) != 0);
        oper <<= 1;
        oper |= carry_in;

        set_status_flag(STATUS_NEGATIVE, (oper & 0x80) != 0);
        set_status_flag(STATUS_ZERO, oper == 0);

        write_result(addr, arg1, arg2, oper);
    }

    void Engine::op_ror(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        uint8_t oper = get_param(addr, arg1, arg2);

        uint8_t carry_in = carry_bit() << 7;

        set_status_flag(STATUS_NEGATIVE, carry_in == 0x80);
        set_status_flag(STATUS_CARRY, (oper & 0x01) == 1);
        oper >>= 1;
        oper |= carry_in;
        set_status_flag(STATUS_ZERO, oper == 0);

        write_result(addr, arg1, arg2, oper);
    }

    void Engine::op_rti() {
        pc = pull_stackw();
        p = pull_stack() & ~STATUS_BREAK;
    }

    void Engine::op_rts() {
        pc = pull_stackw();
    }

    void Engine::op_sbc(AddressMode addr, uint8_t param, int &ticks) {
        if (!is_bcd()) {
            const uint16_t oper = (param ^ 0xFF) + carry_bit();
            uint16_t probe = a + oper;
            set_status_flag(STATUS_CARRY, (probe & 0xFF00) != 0);
            set_status_ov(probe, a, oper);
            a = probe & 0x0FF;
            set_status_nz(a);
        } else {
            // see http://6502.org/tutorials/decimal_mode.html
            int16_t al =  (a & 0x0F) - (param & 0x0F) + carry_bit() - 1;
            if (al < 0) {
                al = (al-0x06 & 0x0F) - 0x10;
            }
            uint16_t a_aux = (a & 0xF0) - (param & 0xF0) + al;
            // Note that a_aux can be >= 0x100 at this point
            if (a_aux < 0) {
                a_aux -= 0x60;
            }
            a = a_aux & 0x0FF;
            set_status_flag(STATUS_CARRY, (a_aux >> 8) != 0);
            set_status_flag(STATUS_ZERO, a == 0);
        }
    }

    void Engine::op_sec() {
        set_status_flag(STATUS_CARRY, true);
    }

    void Engine::op_sed() {
        set_status_flag(STATUS_DECIMAL, true);
    }

    void Engine::op_sei() {
        set_status_flag(STATUS_INTERRUPT_DISABLE, true);
    }

    void Engine::op_sta(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        write_result(addr, arg1, arg2, a);
    }

    void Engine::op_stx(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        write_result(addr, arg1, arg2, x);
    }

    void Engine::op_sty(AddressMode addr, uint8_t arg1, uint8_t arg2) {
        write_result(addr, arg1, arg2, y);
    }

    void Engine::op_tax() {
        x = a;

        set_status_nz(x);
    }

    void Engine::op_tay() {
        y = a;

        set_status_nz(y);
    }

    void Engine::op_tsx() {
        x = s;

        set_status_nz(x);
    }

    void Engine::op_txa() {
        a = x;

        set_status_nz(a);
    }

    void Engine::op_txs() {
        s = x;
        // no status flags changed
    }

    void Engine::op_tya() {
        a = y;

        set_status_nz(a);
    }

    void Engine::write_result(AddressMode addr, uint8_t arg1, uint8_t arg2, uint8_t byte) {
        switch (addr) {
            case ACCU:
                a = byte;
                break;
            case ZERO:
                set_byte(arg1, byte);
                break;
            case ZERO_X:
                set_byte((arg1 + x) & 0x0FF, byte);
                break;
            case ZERO_Y:
                set_byte((arg1 + y) & 0x0FF, byte);
                break;
            case ABSOLUTE:
                set_byte(address(arg1, arg2), byte);
                break;
            case ABSOLUTE_X:
                set_byte(address(arg1, arg2) + x, byte);
                break;
            case ABSOLUTE_Y:
                set_byte(address(arg1, arg2) + y, byte);
                break;
            case INDEXED_INDIRECT_X:
                set_byte(indirect(address(arg1, arg2) + x), byte);
                break;
            case INDIRECT_INDEXED_Y:
                set_byte(indirect(address(arg1, arg2)) + y, byte);
                break;
            default:
                ;
        }
    }

    void Engine::set_status_flag(StatusFlag flag, bool newState) {
        if (newState) {
            p |= flag;
        } else {
            p &= ~flag;
        }
    }

    void Engine::set_status_nz(uint8_t b) {
        set_status_flag(STATUS_ZERO, b == 0);
        set_status_flag(STATUS_NEGATIVE, (b & 0x80) != 0);
    }

    uint8_t Engine::get_byte(uint16_t address) {
        uint8_t override = 0;

        if (address < 0xC000) {
            return ram[address];
        }
        if (address >= 0xC000 && address < 0xC100) {
            return get_io_byte(address & 0x0FF);
        }
        if (address >= 0xC100 && address < 0xC800) {
            return get_peripheral_rom_byte(address & 0x07FF);
        }
        if (address >= 0xC800 && address < 0xCFFF) {
            return get_peripheral_expansion_rom_byte(address & 0x07FF);
        }
        if (address >= 0xD000) {
            return rom[address - 0xD000];
        }
        return 0; // CFFF - peripheral expansion rom reset.
    }

    uint8_t Engine::get_io_byte(uint8_t address) const {
        uint8_t value = 0;
        if (address < 0x90) {
            if (terminal->get_io_byte(address, value)) {
                return value;
            }
            if (slots[0] != nullptr) {
                value = slots[0]->get_io(address);
            }
        } else {
            int slot = (address & 0x70) >> 4;

            uint8_t location = address & 0x00F;
            if (slots[slot] != nullptr) {
                value = slots[slot]->get_io(location);
            }
        }
        return value;
    }

    uint8_t Engine::get_peripheral_rom_byte(uint16_t address) const {
        int slot = (address & 0x0700) >> 8;
        uint8_t value = 0;
        uint8_t location = address & 0x0FF;
        if (slots[slot] != nullptr) {
            value = slots[slot]->mem(location);
        }
        return value;
    }

    uint8_t Engine::get_peripheral_expansion_rom_byte(uint16_t location) {
        uint8_t value = 0;

        for (auto & slot: slots) {
            // first come first serve for now...
            if (slot != nullptr  && slot->has_expansion_rom()) {
                value = slot->expansion_rom(location);
                break;
            }
        }
        return value;
    }

    void Engine::set_byte(uint16_t address, uint8_t value) {
        if (address < 0xC000) {
            ram[address] = value;
        } else if (address < 0xC090) {
            const uint8_t location = address & 0x0FF;
            if (!terminal->set_io_byte(address, value) && slots[0] != nullptr) {
                slots[0]->set_io(location, value);
            }
        } else if (address < 0xC100) {
            int slot = (address & 0x070) >> 4;
            const uint8_t location = address & 0x0F;
            if (slots[slot] != nullptr) {
                slots[slot]->set_io(location, value);
            }
        }
    }
}
