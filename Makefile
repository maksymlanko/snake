# Makefile to assemble, copy, and run assembly code in DOSBox

# Set the assembler
ASM=nasm

# Define the emulator command
EMULATOR=dosbox

# Set the source file and target output
#SRC=graphics.asm
#TARGET=graphics.com
SRC=sum.asm
TARGET=sum.com

DOS_DIR=~/dosprograms

# Default target to build, copy, and run
all: $(TARGET) run

# Assemble the .com file
$(TARGET): $(SRC)
	$(ASM) -f bin $(SRC) -o $(TARGET)
	cp $(TARGET) $(DOS_DIR)

# Run DOSBox
run:
	$(EMULATOR)

# Clean up build artifacts
clean:
	rm -f $(TARGET)
	rm -f $(DOS_DIR)/$(TARGET)

