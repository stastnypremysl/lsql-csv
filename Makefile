all: build
	ghc -j`nproc` -i./src --make -O3 -o ./build/lsql-csv ./src/Main.hs

build:
	mkdir -p build


.PHONY: clean
clean:
	git clean -Xf; rm -r ./build
