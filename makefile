FLAGS = -Wall -dynamic -no-keep-o-files -no-keep-hi-files -Wno-unused-imports
SOURCEDIR = src/
INCLUDE_DIRS = src/parser
OUT_DIR = bin

default: build test

all: build expr

makedir:
	mkdir -p $(OUT_DIR)

build: makedir
	ghc $(FLAGS) -o $(OUT_DIR)/soucc -i$(SOURCEDIR):$(INCLUDE_DIRS) src/Main.hs

.PHONY: clean
clean:
	rm -fr $(OUT_DIR)

.PHONY: test
test: 
	@test/test_parser

expr: makedir
	ghc $(FLAGS) -o $(OUT_DIR)/expr -i$(SOURCEDIR):$(INCLUDE_DIRS) src/expr/Main.hs 

