# FLAGS = -dynamic -Wall -Wno-unused-imports -no-keep-o-files -no-keep-hi-files
SOURCEDIR = src/
INCLUDE_DIRS = src/parser:src/code_gen
OUT_DIR = bin
HI_DIR = hi_files
OBJ_DIR = obj_files
FLAGS = -Wall -dynamic -hidir $(HI_DIR) -odir $(OBJ_DIR) -Wno-unused-imports

default: all test

all: build expr parser codegen

makedirs:
	@mkdir -p $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)

build: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/soucc -i$(SOURCEDIR):$(INCLUDE_DIRS) src/Main.hs

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)

.PHONY: test
test: test_parser test_codegen test_integration

.PHONY: test_parser
test_parser:
	@test/test_parser

.PHONY: test_codegen
test_codegen:
	@echo "testing code gen..."
	@runghc -Wall -isrc/:src/code_gen/:src/parser/ test/test_codegen.hs

.PHONY: test_integration
test_integration:
	@echo "running integration test..."
	@test/integration/integration_test

expr: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/expr -i$(SOURCEDIR):$(INCLUDE_DIRS) src/expr/Main.hs 

parser: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/parser -i$(SOURCEDIR):$(INCLUDE_DIRS) src/parser/Main.hs

codegen: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/code_gen -i$(SOURCEDIR):$(INCLUDE_DIRS) src/code_gen/Main.hs
