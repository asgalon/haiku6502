find_package (Boost COMPONENTS system filesystem unit_test_framework REQUIRED)
add_definitions (-DBOOST_TEST_DYN_LINK)
add_executable (Test test.cpp ../data/test.rom)

target_link_libraries (Test
        haiku6502lib
        ${Boost_FILESYSTEM_LIBRARY}
        ${Boost_SYSTEM_LIBRARY}
        ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
)
target_include_directories(Test PRIVATE
        .
        ${PROJECT_SOURCE_DIR}/src
        ${Boost_INCLUDE_DIRS}
)
