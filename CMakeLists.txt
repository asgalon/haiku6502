cmake_minimum_required(VERSION 4.1)
project(haiku6502 CXX)

## Set C++ standard/compile-time options
add_library(haiku6502_options INTERFACE)
target_compile_features(haiku6502_options INTERFACE cxx_std_17)

# Add the src to the build
add_subdirectory(src)

add_subdirectory(test)
add_subdirectory(include/haiku6502)

enable_testing()
add_test(NAME EngineTest COMMAND Test WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/data)

set(CMAKE_INCLUDE_PATH, "${PROJECT_SOURCE_DIR}/include")

set(CURSES_NEED_NCURSES TRUE)
set(CURSES_NEED_WIDE FALSE)

find_package(Curses REQUIRED)

if(Curses_FOUND AND NOT TARGET Curses::Curses)
    add_library(Curses::Curses INTERFACE IMPORTED)
    set_target_properties(
            Curses::Curses
            PROPERTIES
            INTERFACE_LINK_LIBRARIES "${CURSES_LIBRARIES}"
            INTERFACE_INCLUDE_DIRECTORIES "${CURSES_INCLUDE_DIRS}"
    )
endif()

## TODO Cleanup for installation
target_include_directories(haiku6502 PUBLIC ${CURSES_INCLUDE_DIR} ./include )
target_include_directories(haiku6502lib PUBLIC ${CURSES_INCLUDE_DIR} ./include)
target_link_libraries(haiku6502 PRIVATE Curses::Curses)
target_link_libraries(Test Curses::Curses)

