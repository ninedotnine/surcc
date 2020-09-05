SOURCEDIR := src/
OUT_DIR := bin
CACHE_DIR := cache
TEST_DIR := $(CACHE_DIR)/test
HI_DIR := $(CACHE_DIR)/hi_files
OBJ_DIR := $(CACHE_DIR)/obj_files
FLAGS := -Wall -dynamic -j -hidir $(HI_DIR) -odir $(OBJ_DIR) -i$(SOURCEDIR)  -Wno-unused-imports -Wall-missed-specialisations

.PHONY: soucc expr parser all default

default: all test

all: soucc expr parser

soucc: src/Main_Soucc.hs | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	@ghc $(FLAGS) -o $(OUT_DIR)/soucc -main-is Main_Soucc $<

expr: src/Main_Expr.hs soucc | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	@ghc $(FLAGS) -o $(OUT_DIR)/expr -main-is Main_Expr $<

parser: src/Main_Parser.hs soucc expr | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	@ghc $(FLAGS) -o $(OUT_DIR)/parser -main-is Main_Parser $<

$(OUT_DIR) $(CACHE_DIR) $(TEST_DIR) $(HI_DIR) $(OBJ_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(CACHE_DIR)

.PHONY: test
test: test/parser test/type_checker test/typechecker_progs test/codegen test/expr_parser test/integration test/typechecker_globals
	@echo "all tests successful! :^D"

.PHONY: test/parser test/expr_parser test/integration
test/expr_parser: parser expr
test/parser: parser
test/integration test/expr_parser test/parser: soucc
	@ $@

.PHONY: test/codegen test/type_checker test/typechecker_progs test/typechecker_globals
test/codegen test/type_checker test/typechecker_progs test/typechecker_globals: all | $(TEST_DIR)
	@$(RM) $(CACHE_DIR)/hi_files/Main.hi  	# ugh hack to fix ghc
	@ghc $(FLAGS) -o $(CACHE_DIR)/$@ $@.hs
	@$(CACHE_DIR)/$@

.PHONY: deps
deps: | $(CACHE_DIR)
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/soucc-deps src/Main.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/expr-deps src/Main_Expr.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/parser-deps src/Main_Parser.hs
