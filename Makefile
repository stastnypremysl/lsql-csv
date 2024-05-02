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
test: compile tests-basic tests-example tests-options tests-blocks tests-functions
	echo -e "\033[1mAll tests succedded.\033[0m"


.PHONY: tests-basic
tests-basic: $(wildcard ./tests/basic/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mBasic tests succedded.\033[0m"

.PHONY: tests-example
tests-example: $(wildcard ./tests/example/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mExample tests succedded.\033[0m"

.PHONY: tests-options
tests-options: $(wildcard ./tests/options/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mOptions tests succedded.\033[0m"

.PHONY: tests-blocks
tests-blocks: $(wildcard ./tests/blocks/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mBlocks tests succedded.\033[0m"


.PHONY: tests-functions
tests-functions: tests-onearg-functions tests-aggregate-functions tests-operators

.PHONY: tests-aggregate-functions
tests-aggregate-functions: $(wildcard ./tests/aggregate-functions/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mAggregate functions tests succedded.\033[0m"

.PHONY: tests-onearg-functions
tests-onearg-functions: $(wildcard ./tests/onearg-functions/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mOnearg functions tests succedded.\033[0m"

.PHONY: tests-operators
tests-operators: $(wildcard ./tests/operators/*)
	echo $? | tr ' ' '\n' | xargs -I{} bash -c "{}"
	echo -e "\033[1mOperators tests succedded.\033[0m"

