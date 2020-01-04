# FLAGS = -dynamic -Wall -Wno-unused-imports -no-keep-o-files -no-keep-hi-files
SOURCEDIR = src/
INCLUDE_DIRS = src/Parser:src/CodeGen
OUT_DIR = bin
HI_DIR = cache/hi_files
OBJ_DIR = cache/obj_files
FLAGS = -Wall -dynamic -j -hidir $(HI_DIR) -odir $(OBJ_DIR) -i$(SOURCEDIR):$(INCLUDE_DIRS)  -Wno-unused-imports -Wall-missed-specialisations

default: all test

all: build expr parser

makedirs:
	@mkdir -p $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)

build: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/soucc src/Main.hs

expr: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/expr -main-is Main_Expr src/Main_Expr.hs

parser: makedirs
	ghc $(FLAGS) -o $(OUT_DIR)/parser -main-is Main_Parser src/Main_Parser.hs

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)

.PHONY: test
test: test_parser test_codegen test_expr_parser test_integration 
	@echo "all tests successful! :^D"

.PHONY: test_parser
test_parser: parser
	@test/test_parser

.PHONY: test_codegen
test_codegen:
	@runghc -Wall -i$(SOURCEDIR):$(INCLUDE_DIRS) test/test_codegen.hs

.PHONY: test_integration
test_integration:
	@test/integration_test

.PHONY: test_expr_parser
test_expr_parser: parser expr
	@test/test_expr_parser

