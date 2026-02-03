//
// Created by Peter Köllner on 22/1/26.
//

#ifndef HAIKU6502_CPU_HPP
#define HAIKU6502_CPU_HPP

#include <cstdint>

namespace haiku6502 {
    /*
     * This is a 6502-ish 8-bit cpu with an accumulator,
     * X and Y register, 8 bit stack pointer, status register
     * and a 16 bit program counter. Address room is 64 kBytes
     */
    enum OpCode {
        ADC,
        AND,
        ASL,
        BCC,
        BCS,
        BEQ,
        BIT,
        BMI,
        BNE,
        BPL,
        BRK,
        BVC,
        BVS,
        CLC,
        CLD,
        CLI,
        CLV,
        CMP,
        CPX,
        CPY,
        DEC,
        DEX,
        DEY,
        EOR,
        INC,
        INX,
        INY,
        JMP,
        JSR,
        LDA,
        LDX,
        LDY,
        LSR,
        NOP,
        ORA,
        PHA,
        PHP,
        PLA,
        PLP,
        ROL,
        ROR,
        RTI,
        RTS,
        SBC,
        SEC,
        SED,
        SEI,
        STA,
        STX,
        STY,
        TAX,
        TAY,
        TSX,
        TXA,
        TXS,
        TYA
    };

    enum AddressMode {
        IMPLIED,
        ACCU,
        IMMEDIATE,
        ZERO,
        ZERO_X,
        ZERO_Y,
        ABSOLUTE,
        ABSOLUTE_X,
        ABSOLUTE_Y,
        INDEXED_INDIRECT_X,
        INDIRECT_INDEXED_Y,
        ABSOLUTE_INDIRECT,
        RELATIVE
    };

    enum StatusFlag {
        STATUS_CARRY = 0x01,
        STATUS_ZERO = 0x02,
        STATUS_INTERRUPT_DISABLE = 0x04,
        STATUS_DECIMAL = 0x08,
        STATUS_BREAK = 0x10,
        STATUS_IGNORE = 0x20,
        STATUS_OVERFLOW = 0x40,
        STATUS_NEGATIVE = 0x80
    };

    struct CpuStatus {
        // Accumulator register
        uint8_t a {0};
        // X register.
        uint8_t x {0};
        // Y register
        uint8_t y {0};
        // program counter pc
        uint16_t pc {0};
        // stack pointer
        uint8_t s { 0xFF };

        // status flags (known as P register)
        uint8_t p { STATUS_IGNORE };
    };

    struct Instruction {
        OpCode op;
        AddressMode addr;
        uint8_t length;
        uint8_t cycles;

        Instruction(const OpCode _o, const AddressMode _a, const uint8_t l, const uint8_t t)
            : op(_o), addr(_a), length(l), cycles(t) {}
    };

    extern Instruction operations[256];

    constexpr uint16_t NMI_VECTOR = 0xFFFA;
    constexpr uint16_t RESET_VECTOR = 0xFFFC;
    constexpr uint16_t IRQ_VECTOR = 0xFFFE;
}

#endif //HAIKU6502_CPU_HPP