.PHONY: all
all: build/Main.o

TARGET=embedded
TRIPLE=thumbv6m-none--eabi
CPU=cortex-m0

build/Main.o: Main.mll
	stack run -- compile --target $(TARGET) --triple $(TRIPLE) --cpu $(CPU) $<

.PHONY: clean
clean:
	rm -rf build
