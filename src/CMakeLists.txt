add_library(haiku6502lib
        cpu.cpp
        ../include/haiku6502/cpu.h
        ../include/haiku6502/setup.h setup.cpp
        memory.cpp
        ../include/haiku6502/memory.h
        engine.cpp
        ../include/haiku6502/engine.h
        terminal.cpp
        ../include/haiku6502/terminal.h
        slot0_peripheral.cpp
        ../include/haiku6502/device.h)

add_executable(haiku6502 main.cpp
        ../include/haiku6502/peripheral.h
        ../include/haiku6502/slot0_peripheral.h
        ../include/haiku6502/device.h)

set(CMAKE_INCLUDE_PATH, ../include)
set(INCLUDE_DIRECTORIES ../include)

include_directories(../include)

target_include_directories(haiku6502 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${PROJECT_SOURCE_DIR}/include)

target_link_libraries(haiku6502 PRIVATE haiku6502lib haiku6502_options)
