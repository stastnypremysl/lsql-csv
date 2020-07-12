all: compile test

.PHONY: compile
compile: build
	ghc -j`nproc` -i./src --make -O3 -o ./build/lsql-csv ./src/Main.hs

.PHONY: debug
debug: build
	ghc -j`nproc` -prof -fprof-auto -fprof-cafs -i./src --make -O3 -o ./build/lsql-csv ./src/Main.hs

build:
	mkdir -p build

.PHONY: install
install:
	cp build/lsql-csv /usr/local/bin

.PHONY: clean
clean:
	git clean -Xf; rm -r ./build

.PHONY: test
test: compile test-sec-delimiter
	echo "All tests succedded."

test-sec-delimiter: compile
	./tests/secondary-delimiter
		
