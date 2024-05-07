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
test: compile test-basics test-examples test-options test-blocks test-functions
	echo -e "\033[1mAll tests succedded.\033[0m"


.PHONY: test-basics
test-basics: $(wildcard ./tests/basics/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mBasics tests succedded.\033[0m"

.PHONY: test-examples
test-examples: $(wildcard ./tests/examples/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mExamples tests succedded.\033[0m"

.PHONY: test-options
test-options: $(wildcard ./tests/options/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mOptions tests succedded.\033[0m"

.PHONY: test-blocks
test-blocks: $(wildcard ./tests/blocks/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mBlocks tests succedded.\033[0m"


.PHONY: test-functions
test-functions: test-onearg-functions test-aggregate-functions test-operators

.PHONY: test-aggregate-functions
test-aggregate-functions: $(wildcard ./tests/aggregate-functions/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mAggregate functions tests succedded.\033[0m"

.PHONY: test-onearg-functions
test-onearg-functions: $(wildcard ./tests/onearg-functions/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mOnearg functions tests succedded.\033[0m"

.PHONY: test-operators
test-operators: $(wildcard ./tests/operators/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mOperators tests succedded.\033[0m"

