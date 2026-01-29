//
// Created by Peter Köllner on 23/1/26.
//

#define BOOST_TEST_MODULE EngineTests
#include <boost/test/unit_test.hpp>

#include "../include/haiku6502/engine.h"

#include <fstream>

struct Fixture {
    Fixture() {
        BOOST_TEST_MESSAGE( "setup fixture" );

        haiku6502::engine_setup setup {

        };

        setup.rom = "./unit_test.rom";
        setup.debug = true;

        engine = new haiku6502::Engine(setup);

        prepare_engine();
    }

    ~Fixture() {
        BOOST_TEST_MESSAGE( "teardown fixture" );
        delete engine;
    }

    void prepare_engine() const {
        for (int address = 0; address < 0xC000; address++) {
            engine->set_byte(address, address & 0x00FF);
        }
    }

    void step(haiku6502::CpuStatus& status) const {
        engine->cycle();
        engine->get_state(status);
    }

    uint8_t get_byte(uint16_t address) const {
        return engine->get_byte(address);
    }

    void load_bce_test() const {
        engine->load_ram(0x0800, 431, "check_bcd_mode_0800.ram");
    }

    haiku6502::Engine *engine;
};

BOOST_FIXTURE_TEST_SUITE(EngineTest, Fixture)

BOOST_AUTO_TEST_CASE(AddressModeImmediate) {

    BOOST_CHECK_EQUAL(0xea, engine->get_byte(0xD000));
    BOOST_CHECK_EQUAL(0x06, engine->get_byte(0xFFFE));

    uint8_t param = engine->get_param(haiku6502::IMMEDIATE, 0x17, 0xff);

    BOOST_CHECK_EQUAL(0x17, param);

    param = engine->get_param(haiku6502::IMMEDIATE, 0xF3, 0xff);

    BOOST_CHECK_EQUAL(0xF3, param);

}

BOOST_AUTO_TEST_CASE(check_bcd_mode) {
    haiku6502::CpuStatus status;

    load_bce_test();

    engine->reset();

    // call test method
    engine->op_jmp(haiku6502::ABSOLUTE, 0x11, 0x08);

    int countdown = 0x1000000;
    // run until reaching DONE
    while (status.pc != 0x087b && --countdown) {
        step(status);
        uint8_t result = get_byte(0x0804);
        if (result != 0) {
            uint16_t pc = engine->get_cursor();
            char pcstr[16];
            snprintf(pcstr, 16, "0x%0x", pc);
            BOOST_CHECK_EQUAL("", pcstr);
            break;
        }
    }
    uint8_t result = get_byte(0x0804);
    BOOST_CHECK_EQUAL(0, result);

}

BOOST_AUTO_TEST_CASE(operation) {
    haiku6502::CpuStatus status;

    engine->reset();

    engine->get_state(status);

    BOOST_CHECK_EQUAL(0xF80c, status.pc);

    step(status); // sed
    BOOST_CHECK_EQUAL(0xF80d, status.pc);
    uint8_t flag = status.p & haiku6502::STATUS_DECIMAL;
    BOOST_CHECK_EQUAL(haiku6502::STATUS_DECIMAL, flag);
    step(status); // cld
    BOOST_CHECK_EQUAL(0xF80e, status.pc);
    flag = status.p & haiku6502::STATUS_DECIMAL;
    BOOST_CHECK_EQUAL(0, flag);

    step(status); // sei
    BOOST_CHECK_EQUAL(0xF80f, status.pc);
    flag = status.p & haiku6502::STATUS_INTERRUPT_DISABLE;
    BOOST_CHECK_EQUAL(haiku6502::STATUS_INTERRUPT_DISABLE, flag);
    step(status); // cli
    BOOST_CHECK_EQUAL(0xF810, status.pc);
    flag = status.p & haiku6502::STATUS_INTERRUPT_DISABLE;
    BOOST_CHECK_EQUAL(0, flag);

    step(status); // lda #$00
    BOOST_CHECK_EQUAL(0xF812, status.pc);
    BOOST_CHECK_EQUAL(0, status.a);
    BOOST_CHECK_EQUAL(haiku6502::STATUS_ZERO, status.p);

    step(status); // ldx #$00
    BOOST_CHECK_EQUAL(0xF814, status.pc);
    BOOST_CHECK_EQUAL(0, status.x);
    BOOST_CHECK_EQUAL(haiku6502::STATUS_ZERO, status.p);

    step(status); // ldy #$00
    BOOST_CHECK_EQUAL(0xF816, status.pc);
    BOOST_CHECK_EQUAL(0, status.y);
    BOOST_CHECK_EQUAL(haiku6502::STATUS_ZERO, status.p);

    step(status); // sta $00
    BOOST_CHECK_EQUAL(0xF818, status.pc);
    BOOST_CHECK_EQUAL(0, get_byte(0));
    BOOST_CHECK_EQUAL(haiku6502::STATUS_ZERO, status.p);

    step(status); // stx $01
    BOOST_CHECK_EQUAL(0xF81a, status.pc);
    BOOST_CHECK_EQUAL(0, get_byte(1));
    BOOST_CHECK_EQUAL(haiku6502::STATUS_ZERO, status.p);

    step(status); // nop
    BOOST_CHECK_EQUAL(0xF81b, status.pc);

    step(status); // jsr $f800
    BOOST_CHECK_EQUAL(0xF800, status.pc);
    BOOST_CHECK_EQUAL(0xFE, status.s);
    BOOST_CHECK_EQUAL(0xf8, get_byte(0x1FF));
    BOOST_CHECK_EQUAL(0x1e, get_byte(0x1FE));

    step(status); // ldy #$ff
    BOOST_CHECK_EQUAL(0xF802, status.pc);
    BOOST_CHECK_EQUAL(0xFF, status.y);
    BOOST_CHECK_EQUAL(haiku6502::STATUS_NEGATIVE, status.p);

    for (int i=0; i < 0xFE; i++) {
        step(status); // dey
        BOOST_CHECK_EQUAL(0xF803, status.pc);
        BOOST_CHECK_EQUAL(0xFE - i, status.y);
        if (status.y >= 0x80) {
            BOOST_CHECK_EQUAL(haiku6502::STATUS_NEGATIVE, status.p);
        } else {
            BOOST_CHECK_EQUAL(0, status.p);
        }
        step(status); // bvc lp1
        BOOST_CHECK_EQUAL(0xF802, status.pc);
        BOOST_CHECK_EQUAL(0xFE - i, status.y);
        if (status.y >= 0x80) {
            BOOST_CHECK_EQUAL(haiku6502::STATUS_NEGATIVE, status.p);
        } else {
            BOOST_CHECK_EQUAL(0, status.p);
        }
    }

    step(status); // dey
    BOOST_CHECK_EQUAL(0xF803, status.pc);
    BOOST_CHECK_EQUAL(0, status.y);
    BOOST_CHECK_EQUAL(haiku6502::STATUS_ZERO, status.p);

    step(status); // bvc lp1
    BOOST_CHECK_EQUAL(0xF805, status.pc);
    BOOST_CHECK_EQUAL(haiku6502::STATUS_ZERO, status.p);

    step(status); // rts
    BOOST_CHECK_EQUAL(0xF81E, status.pc);
    BOOST_CHECK_EQUAL(0, status.s);

    // Compare instruction results
    //
    // +----------------+---+---+---+
    // | Compare Result | N | Z | C |
    // +----------------+---+---+---+
    // | Reg < Arg      | * | 0 | 0 |
    // +----------------+---+---+---+
    // | Reg = Arg      | 0 | 1 | 1 |
    // +----------------+---+---+---+
    // | Reg > Arg      | * | 0 | 1 |
    // +----------------+---+---+---+
    // Expected result for #37 with #$de:
    // N = 0; Z = 0; C = 0
    step(status); // clc
    step(status); // lda #$37
    BOOST_CHECK_EQUAL(0, status.p);
    step(status); // cmp #$de
    BOOST_CHECK_EQUAL(0, status.p);
    // Expected result for #de with #$37:
    // N = 1; Z = 0; C = 1
    step(status); // clc
    step(status); // lda #$de
    BOOST_CHECK_EQUAL(0x80, status.p);
    step(status); // cmp #$37
    BOOST_CHECK_EQUAL(0x81, status.p);
    // Expected result for #37 with #$37:
    // N = 0; Z = 1; C = 1
    step(status); // clc
    step(status); // lda #$37
    BOOST_CHECK_EQUAL(0x00, status.p);
    step(status); // cmp #$37
    BOOST_CHECK_EQUAL(0x03, status.p);

    step(status); // sta $0
    BOOST_CHECK_EQUAL(0x03, status.p); // does not affect p
    step(status); // clc
    // Expected result:
    // A = 0x87
    // N = 1; Z = 0; C = 0; V = 1
    step(status); // adc #$50
    BOOST_CHECK_EQUAL(0x87, status.a);
    BOOST_CHECK_EQUAL(0xC0, status.p);

    step(status); // clv

    step(status); // lda $0
    BOOST_CHECK_EQUAL(0x0, status.p); // does not affect p
    step(status); // sec
    // Expected result:
    // A = 0x88
    // N = 1; Z = 0; C = 0; V = 1
    step(status); // adc #$50
    BOOST_CHECK_EQUAL(0x88, status.a);
    BOOST_CHECK_EQUAL(0xC0, status.p);

    step(status); // clv

}


BOOST_AUTO_TEST_SUITE_END()
