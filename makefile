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


# the binaries are PHONY targets
# because make doesn't know which .hs source files they depend on
# so just let GHC resolve the depends

.PHONY: default parser typechecker expr surcc

default: surcc test

# GHC compiles modules in parallel.
# asking make to parallelize too just creates trouble.
# that's why the targets depend on each other.

surcc: src/Main_Surcc.hs expr | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	@ghc $(FLAGS) -o $(OUT_DIR)/$@ -main-is Main_Surcc $<

expr: src/Main_Expr.hs typechecker | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
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
.PHONY: test/parser test/codegen test/integration test/typechecker
test:   test/parser test/codegen test/integration test/typechecker
	@echo "all tests successful! :^D"

test/integration: surcc
	@ $@

test/typechecker: typechecker
	$(OUT_DIR)/typechecker --test

test/parser: parser
test/codegen: surcc
test/parser test/codegen: | $(TEST_DIR)
	@$(RM) $(CACHE_DIR)/hi_files/Main.hi  	# ugh hack to fix ghc
	@ghc $(FLAGS) -o $(CACHE_DIR)/$@ $@.hs
	@$(CACHE_DIR)/$@


## i really don't use this and do not know that it works

.PHONY: deps
deps: | $(CACHE_DIR)
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/surcc-deps src/Main_Surcc.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/expr-deps src/Main_Expr.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/parser-deps src/Main_Parser.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/typechecker-deps src/Main_TypeChecker.hs


## tests after this line are not guaranteed to pass or even compile

.PHONY: obsolete_tests
obsolete_tests: test/expr_parser test/type_checker test/typechecker_progs test/typechecker_globals
.PHONY:         test/expr_parser test/type_checker test/typechecker_progs test/typechecker_globals

test/expr_parser: typechecker
	@ $@

test/type_checker test/typechecker_progs test/typechecker_globals: surcc | $(TEST_DIR)
	@$(RM) $(CACHE_DIR)/hi_files/Main.hi  	# ugh hack to fix ghc
	@ghc $(FLAGS) -o $(CACHE_DIR)/$@ $@.hs
	@$(CACHE_DIR)/$@
