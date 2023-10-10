cmake_minimum_required(VERSION 3.26)
project(MidiJoystick)

set(CMAKE_CXX_STANDARD 20)
#target_compile_options(MidiJoystick PUBLIC -Wall -Wextra -Wfloat-conversion )

file(GLOB_RECURSE sources      main.cpp glue.c joystick.c  midijack.c )

# include all source files
add_executable(MidiJoystick ${sources} )

target_link_libraries( MidiJoystick -ljack )