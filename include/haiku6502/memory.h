//
// Created by Peter Köllner on 22/1/26.
//

#ifndef HAIKU6502_MEMORY_H
#define HAIKU6502_MEMORY_H

namespace haiku6502 {
    const int RAM_SIZE = 0xC000;
    const int ROM_SIZE = 0x4000;
    const int SCREEN0 = 0x2000;
    const int SCREEN_SIZE = 0x780; // 80x24
    const int STACK_BASE = 0x100;
    const int PORT_BASE = 0x300;
}

#endif //HAIKU6502_MEMORY_H