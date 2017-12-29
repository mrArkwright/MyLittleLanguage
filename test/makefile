.PHONY: test
test: main
	./main

main: example.mll MyLittleLanguage
	stack exec MyLittleLanguage example.mll

.PHONY: MyLittleLanguage
MyLittleLanguage:
	stack build

.PHONY: clean
clean:
	rm -f main example.s example.o builtins.o example.ll
