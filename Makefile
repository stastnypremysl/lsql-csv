all: build/Main.o


build/%.o: src/%.hs
	ghc -i ./build/* -c -O3 -o $@ $<


build/Main.o: build/Args.o
build/Args.o: build/Options.o

.PHONY: clean
clean:
	rm build/*
