DESTDIR=/usr/local/bin

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
	cp build/lsql-csv ${DESTDIR}

.PHONY: clean
clean:
	git clean -Xf; rm -r ./build

.PHONY: test
test: compile test-sec-delimiter test-named test-aritmetics test-grouping
	echo "All tests succedded."

.PHONY: test-sec-delimiter
test-sec-delimiter: compile
	./tests/secondary-delimiter

.PHONY: test-named
test-named: compile
	./tests/named
		
.PHONY: test-aritmetics
test-aritmetics: compile
	./tests/aritmetics


.PHONY: test-grouping
test-grouping: compile
	./tests/grouping
