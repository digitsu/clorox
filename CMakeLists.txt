cmake_minimum_required(VERSION 3.20)
project(clorox C)

set(CMAKE_C_STANDARD 99)

add_executable(clorox main.c common.h chunk.h chunk.c memory.h memory.c debug.h debug.c value.h value.c vm.c vm.h compiler.c compiler.h scanner.c scanner.h object.h object.c table.c table.h)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O0")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0")