SHELL = /bin/sh

SOURCEDIR := src/
OUT_DIR := bin
CACHE_DIR := .cache
TEST_DIR := $(CACHE_DIR)/test
HI_DIR := $(CACHE_DIR)/hi_files
OBJ_DIR := $(CACHE_DIR)/obj_files

HSFLAGS := -dynamic -j
GHC_EXTS := -XOverloadedStrings -XLambdaCase -XStrictData -XScopedTypeVariables -XImportQualifiedPost
GHC_FLAGS := -hidir $(HI_DIR) -odir $(OBJ_DIR) -i$(SOURCEDIR)

GHC_WARNS := -Wall -Wextra -Werror
GHC_WARNS += -Wall-missed-specialisations
GHC_WARNS += -Widentities
GHC_WARNS += -Weverything
GHC_WARNS += -Wno-unused-top-binds
GHC_WARNS += -Wno-unused-imports
GHC_WARNS += -Wno-missing-home-modules -Wno-implicit-prelude -Wno-missing-safe-haskell-mode -Wno-missing-import-lists -Wno-unsafe -Wno-missing-deriving-strategies -Wno-missing-local-signatures -Wno-safe -Wno-monomorphism-restriction -Wno-missing-export-lists
GHC_WARNS += -fmax-errors=2

FLAGS := $(HSFLAGS) $(GHC_EXTS) $(GHC_FLAGS) $(GHC_WARNS)


.PHONY: surcc expr parser typechecker all default

default: all test

all: parser expr typechecker surcc

surcc: src/Main_Surcc.hs expr parser typechecker | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	@ghc $(FLAGS) -o $(OUT_DIR)/$@ -main-is Main_Surcc $<

expr: src/Main_Expr.hs parser typechecker | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	@ghc $(FLAGS) -o $(OUT_DIR)/$@ -main-is Main_Expr $<

typechecker: src/Main_TypeChecker.hs parser | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	@ghc $(FLAGS) -o $(OUT_DIR)/$@ -main-is Main_TypeChecker $<

parser: src/Main_Parser.hs | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	@ghc $(FLAGS) -o $(OUT_DIR)/$@ -main-is Main_Parser $<


$(OUT_DIR) $(CACHE_DIR) $(TEST_DIR) $(HI_DIR) $(OBJ_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(CACHE_DIR)

.PHONY: test
test: test/parser test/type_checker test/codegen test/expr_parser test/integration test/typechecker_globals test/typechecker
	@echo "all tests successful! :^D"

.PHONY: test/expr_parser test/integration test/typechecker
test/expr_parser: parser expr
test/integration test/expr_parser: surcc
	@ $@


.PHONY: test/parser
test/parser: test/TestParser.hs parser surcc | $(TEST_DIR)
	@$(RM) $(CACHE_DIR)/hi_files/Main.hi  	# ugh hack to fix ghc
	ghc $(FLAGS) -o $(TEST_DIR)/parser -main-is TestParser $<
	$(TEST_DIR)/parser


test/typechecker: typechecker
	bin/typechecker --test

.PHONY: test/codegen test/type_checker test/typechecker_globals
# test/codegen test/type_checker test/typechecker_globals: all | $(TEST_DIR)
test/codegen: all | $(TEST_DIR)
	@$(RM) $(CACHE_DIR)/hi_files/Main.hi  	# ugh hack to fix ghc
	@ghc $(FLAGS) -o $(CACHE_DIR)/$@ $@.hs
	@$(CACHE_DIR)/$@

.PHONY: deps
deps: | $(CACHE_DIR)
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/surcc-deps src/Main_Surcc.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/expr-deps src/Main_Expr.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/parser-deps src/Main_Parser.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/typechecker-deps src/Main_TypeChecker.hs
