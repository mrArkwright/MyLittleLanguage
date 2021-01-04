.PHONY: all
all: build/LPC810_blinky.o

TARGET=embedded
TRIPLE=thumbv6m-none--eabi
CPU=cortex-m0

build/LPC810_blinky.o: LPC810_blinky.mll
	stack run -- compile --target $(TARGET) --triple $(TRIPLE) --cpu $(CPU) $<

.PHONY: clean
clean:
	rm -rf build
