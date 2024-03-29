INSTALL_DIR=/usr/local/bin

all: compile

.PHONY: compile
compile: build
	ghc -j`nproc` -i./src --make -O3 -o ./build/lsql-csv ./main/Main.hs

build:
	mkdir -p build

docs: build
	cd build; find ../src | grep .hs$ | xargs haddock --html

latex-docs: build
	cd build; find ../src | grep .hs$ | xargs haddock --latex

.PHONY: install
install:
	mkdir -p $(INSTALL_DIR)
	cp build/lsql-csv $(INSTALL_DIR)

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
