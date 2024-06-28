# Makefile for building and running hello.asm

# Default target: Build and run the executable
.PHONY: all
all: run

# Target to build the executable
hello: hello.o
	ld -m elf_i386 hello.o -o hello

# Target to assemble hello.asm into hello.o
hello.o: hello.asm
	nasm -f elf32 hello.asm -o hello.o

# Target to run the executable
.PHONY: run
run: hello
	./hello

# Clean target to remove object files and executable
.PHONY: clean
clean:
	rm -f hello.o hello

