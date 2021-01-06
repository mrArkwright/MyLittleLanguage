.PHONY: all
all: build/Main

build/Main: build/Main.o build/runtime_native.o
	ld64.lld-9 -e _Main.main -lSystem -sdk_version 10.15 $< build/runtime_native.o -o $@

build/Main.o: Main.mll
	stack run -- compile $<

build/runtime_native.o:
	stack run -- compile-runtime

.PHONY: run
run: build/Main
	$<

.PHONY: clean
clean:
	rm -rf build
