.PHONY: all
all: build/LPC810_blinky.o

build/LPC810_blinky.o: LPC810_blinky.mll
	stack run -- --target embedded --triple thumbv6m-none--eabi --cpu cortex-m0 $<

.PHONY: clean
clean:
	rm -rf build
