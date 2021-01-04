.PHONY: all
all: build/native_example

build/native_example: build/native_example.o build/runtime_native.o
	ld64.lld-9 -e _Main.main -lSystem -sdk_version 10.15 $< build/runtime_native.o -o $@

build/native_example.o: native_example.mll
	stack run -- compile $<

build/runtime_native.o:
	stack run -- compile-runtime

.PHONY: run
run: build/native_example
	$<

.PHONY: clean
clean:
	rm -rf build
