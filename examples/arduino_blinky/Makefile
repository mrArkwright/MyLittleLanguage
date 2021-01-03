CC=~/Library/Arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/bin/avr-gcc
OBJCOPY=~/Library/Arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/bin/avr-objcopy

DEVICE=$(shell cat arduino_device)

build/arduino_blinky.hex: build/arduino_blinky.elf
	$(OBJCOPY) -O ihex -R .eeprom build/arduino_blinky.elf build/arduino_blinky.hex

build/arduino_blinky.elf: build/arduino_blinky.o
	$(CC) -w -Os -g -flto -fuse-linker-plugin -Wl,--gc-sections -mmcu=atmega328p -o $@ $< build/runtime_arduino.o /var/folders/qz/7zfds_gs7bj6vh_5hrz747p40000gn/T/arduino-core-cache/core_arduino_avr_nano_cpu_atmega328old_9f7c9d65799dc11bdd5e3e3499711880.a -lm

build/arduino_blinky.o: arduino_blinky.mll
	stack run -- --target arduino --triple avr-atmel-none --cpu "atmega328p" $<

.PHONY: upload
upload: build/arduino_blinky.hex
	arduino-cli upload -p $(DEVICE) -b arduino:avr:nano:cpu=atmega328old -i $<

.PHONY: clean
clean:
	rm -rf build
