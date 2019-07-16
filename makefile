FLAGS = -Wall -dynamic -no-keep-o-files -no-keep-hi-files -Wno-unused-imports
FILES = src/Main.hs
SOURCEDIR = src/
INCLUDE_DIRS = src/parser
OUT_EXE = bin/soucc

default: build test

build:
	mkdir -p bin
	ghc $(FLAGS) -o $(OUT_EXE) -i$(SOURCEDIR):$(INCLUDE_DIRS) $(FILES)
# 	echo helo

.PHONY: clean
clean:
	rm -f $(OUT_EXE)

.PHONY: test
test: 
	@test/test_parser
