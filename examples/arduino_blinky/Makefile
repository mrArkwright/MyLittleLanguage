CC=~/Library/Arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/bin/avr-gcc
OBJCOPY=~/Library/Arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/bin/avr-objcopy

TARGET=arduino
TRIPLE=avr-atmel-none
CPU=atmega328p

DEVICE=$(shell cat arduino_device)
BOARD=arduino:avr:nano:cpu=atmega328old

.PHONY: all
all: build/arduino_blinky.hex

build/arduino_blinky.hex: build/arduino_blinky.elf
	$(OBJCOPY) -O ihex -R .eeprom $< $@

build/arduino_blinky.elf: build/arduino_blinky.o build/runtime_arduino.o
	$(CC) -w -Os -g -flto -fuse-linker-plugin -Wl,--gc-sections -mmcu=$(CPU) -o $@ $< build/runtime_arduino.o /var/folders/qz/7zfds_gs7bj6vh_5hrz747p40000gn/T/arduino-core-cache/core_arduino_avr_nano_cpu_atmega328old_9f7c9d65799dc11bdd5e3e3499711880.a -lm

build/arduino_blinky.o: arduino_blinky.mll
	stack run -- compile --target $(TARGET) --triple $(TRIPLE) --cpu $(CPU) $<

build/runtime_arduino.o:
	stack run -- compile-runtime --target $(TARGET) --triple $(TRIPLE) --cpu $(CPU)

.PHONY: upload
upload: build/arduino_blinky.hex
	arduino-cli upload -p $(DEVICE) -b $(BOARD) -i $<

.PHONY: clean
clean:
	rm -rf build
