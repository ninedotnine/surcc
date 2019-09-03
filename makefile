# FLAGS = -dynamic -Wall -Wno-unused-imports -no-keep-o-files -no-keep-hi-files
SOURCEDIR = src/
INCLUDE_DIRS = src/parser:src/code_gen
OUT_DIR = bin
HI_DIR = hi_files
OBJ_DIR = obj_files
FLAGS = -Wall -dynamic -hidir $(HI_DIR) -odir $(OBJ_DIR) -i$(SOURCEDIR):$(INCLUDE_DIRS)  -Wno-unused-imports

default: all test

all: build expr parser codegen

makedirs:
	@mkdir -p $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)

build: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/soucc src/Main.hs

expr: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/expr -main-is Main_Expr src/expr/Main_Expr.hs

parser: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/parser -main-is Main_Parser src/parser/Main_Parser.hs

codegen: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/code_gen -main-is Main_Codegen src/code_gen/Main_Codegen.hs

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)

.PHONY: test
test: test_parser test_codegen test_integration
	@echo "all tests successful! :^D"

.PHONY: test_parser
test_parser: parser
	@test/test_parser

.PHONY: test_codegen
test_codegen: codegen
	@runghc -Wall -isrc/:src/code_gen/:src/parser/ test/test_codegen.hs

.PHONY: test_integration
test_integration:
	@test/integration_test
