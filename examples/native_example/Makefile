.PHONY: all
all: build/native_example

build/native_example: native_example.mll
	stack run $<

.PHONY: run
run: build/native_example
	$<

.PHONY: clean
clean:
	rm -rf build
