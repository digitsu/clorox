# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.20

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /Applications/CLion.app/Contents/bin/cmake/mac/bin/cmake

# The command to remove a file.
RM = /Applications/CLion.app/Contents/bin/cmake/mac/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/digitsu/CLionProjects/clorox

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/digitsu/CLionProjects/clorox/cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/clorox.dir/depend.make
# Include the progress variables for this target.
include CMakeFiles/clorox.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/clorox.dir/flags.make

CMakeFiles/clorox.dir/main.c.o: CMakeFiles/clorox.dir/flags.make
CMakeFiles/clorox.dir/main.c.o: ../main.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object CMakeFiles/clorox.dir/main.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clorox.dir/main.c.o -c /Users/digitsu/CLionProjects/clorox/main.c

CMakeFiles/clorox.dir/main.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clorox.dir/main.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/digitsu/CLionProjects/clorox/main.c > CMakeFiles/clorox.dir/main.c.i

CMakeFiles/clorox.dir/main.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clorox.dir/main.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/digitsu/CLionProjects/clorox/main.c -o CMakeFiles/clorox.dir/main.c.s

CMakeFiles/clorox.dir/chunk.c.o: CMakeFiles/clorox.dir/flags.make
CMakeFiles/clorox.dir/chunk.c.o: ../chunk.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building C object CMakeFiles/clorox.dir/chunk.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clorox.dir/chunk.c.o -c /Users/digitsu/CLionProjects/clorox/chunk.c

CMakeFiles/clorox.dir/chunk.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clorox.dir/chunk.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/digitsu/CLionProjects/clorox/chunk.c > CMakeFiles/clorox.dir/chunk.c.i

CMakeFiles/clorox.dir/chunk.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clorox.dir/chunk.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/digitsu/CLionProjects/clorox/chunk.c -o CMakeFiles/clorox.dir/chunk.c.s

CMakeFiles/clorox.dir/memory.c.o: CMakeFiles/clorox.dir/flags.make
CMakeFiles/clorox.dir/memory.c.o: ../memory.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building C object CMakeFiles/clorox.dir/memory.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clorox.dir/memory.c.o -c /Users/digitsu/CLionProjects/clorox/memory.c

CMakeFiles/clorox.dir/memory.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clorox.dir/memory.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/digitsu/CLionProjects/clorox/memory.c > CMakeFiles/clorox.dir/memory.c.i

CMakeFiles/clorox.dir/memory.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clorox.dir/memory.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/digitsu/CLionProjects/clorox/memory.c -o CMakeFiles/clorox.dir/memory.c.s

CMakeFiles/clorox.dir/debug.c.o: CMakeFiles/clorox.dir/flags.make
CMakeFiles/clorox.dir/debug.c.o: ../debug.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building C object CMakeFiles/clorox.dir/debug.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clorox.dir/debug.c.o -c /Users/digitsu/CLionProjects/clorox/debug.c

CMakeFiles/clorox.dir/debug.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clorox.dir/debug.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/digitsu/CLionProjects/clorox/debug.c > CMakeFiles/clorox.dir/debug.c.i

CMakeFiles/clorox.dir/debug.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clorox.dir/debug.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/digitsu/CLionProjects/clorox/debug.c -o CMakeFiles/clorox.dir/debug.c.s

CMakeFiles/clorox.dir/value.c.o: CMakeFiles/clorox.dir/flags.make
CMakeFiles/clorox.dir/value.c.o: ../value.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building C object CMakeFiles/clorox.dir/value.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clorox.dir/value.c.o -c /Users/digitsu/CLionProjects/clorox/value.c

CMakeFiles/clorox.dir/value.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clorox.dir/value.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/digitsu/CLionProjects/clorox/value.c > CMakeFiles/clorox.dir/value.c.i

CMakeFiles/clorox.dir/value.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clorox.dir/value.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/digitsu/CLionProjects/clorox/value.c -o CMakeFiles/clorox.dir/value.c.s

CMakeFiles/clorox.dir/vm.c.o: CMakeFiles/clorox.dir/flags.make
CMakeFiles/clorox.dir/vm.c.o: ../vm.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building C object CMakeFiles/clorox.dir/vm.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clorox.dir/vm.c.o -c /Users/digitsu/CLionProjects/clorox/vm.c

CMakeFiles/clorox.dir/vm.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clorox.dir/vm.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/digitsu/CLionProjects/clorox/vm.c > CMakeFiles/clorox.dir/vm.c.i

CMakeFiles/clorox.dir/vm.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clorox.dir/vm.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/digitsu/CLionProjects/clorox/vm.c -o CMakeFiles/clorox.dir/vm.c.s

CMakeFiles/clorox.dir/compiler.c.o: CMakeFiles/clorox.dir/flags.make
CMakeFiles/clorox.dir/compiler.c.o: ../compiler.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building C object CMakeFiles/clorox.dir/compiler.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clorox.dir/compiler.c.o -c /Users/digitsu/CLionProjects/clorox/compiler.c

CMakeFiles/clorox.dir/compiler.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clorox.dir/compiler.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/digitsu/CLionProjects/clorox/compiler.c > CMakeFiles/clorox.dir/compiler.c.i

CMakeFiles/clorox.dir/compiler.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clorox.dir/compiler.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/digitsu/CLionProjects/clorox/compiler.c -o CMakeFiles/clorox.dir/compiler.c.s

CMakeFiles/clorox.dir/scanner.c.o: CMakeFiles/clorox.dir/flags.make
CMakeFiles/clorox.dir/scanner.c.o: ../scanner.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Building C object CMakeFiles/clorox.dir/scanner.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clorox.dir/scanner.c.o -c /Users/digitsu/CLionProjects/clorox/scanner.c

CMakeFiles/clorox.dir/scanner.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clorox.dir/scanner.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/digitsu/CLionProjects/clorox/scanner.c > CMakeFiles/clorox.dir/scanner.c.i

CMakeFiles/clorox.dir/scanner.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clorox.dir/scanner.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/digitsu/CLionProjects/clorox/scanner.c -o CMakeFiles/clorox.dir/scanner.c.s

# Object files for target clorox
clorox_OBJECTS = \
"CMakeFiles/clorox.dir/main.c.o" \
"CMakeFiles/clorox.dir/chunk.c.o" \
"CMakeFiles/clorox.dir/memory.c.o" \
"CMakeFiles/clorox.dir/debug.c.o" \
"CMakeFiles/clorox.dir/value.c.o" \
"CMakeFiles/clorox.dir/vm.c.o" \
"CMakeFiles/clorox.dir/compiler.c.o" \
"CMakeFiles/clorox.dir/scanner.c.o"

# External object files for target clorox
clorox_EXTERNAL_OBJECTS =

clorox: CMakeFiles/clorox.dir/main.c.o
clorox: CMakeFiles/clorox.dir/chunk.c.o
clorox: CMakeFiles/clorox.dir/memory.c.o
clorox: CMakeFiles/clorox.dir/debug.c.o
clorox: CMakeFiles/clorox.dir/value.c.o
clorox: CMakeFiles/clorox.dir/vm.c.o
clorox: CMakeFiles/clorox.dir/compiler.c.o
clorox: CMakeFiles/clorox.dir/scanner.c.o
clorox: CMakeFiles/clorox.dir/build.make
clorox: CMakeFiles/clorox.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Linking C executable clorox"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/clorox.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/clorox.dir/build: clorox
.PHONY : CMakeFiles/clorox.dir/build

CMakeFiles/clorox.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/clorox.dir/cmake_clean.cmake
.PHONY : CMakeFiles/clorox.dir/clean

CMakeFiles/clorox.dir/depend:
	cd /Users/digitsu/CLionProjects/clorox/cmake-build-debug && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/digitsu/CLionProjects/clorox /Users/digitsu/CLionProjects/clorox /Users/digitsu/CLionProjects/clorox/cmake-build-debug /Users/digitsu/CLionProjects/clorox/cmake-build-debug /Users/digitsu/CLionProjects/clorox/cmake-build-debug/CMakeFiles/clorox.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/clorox.dir/depend
