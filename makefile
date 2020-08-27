SOURCEDIR := src/
OUT_DIR := bin
CACHE_DIR := cache
HI_DIR := $(CACHE_DIR)/hi_files
OBJ_DIR := $(CACHE_DIR)/obj_files
FLAGS := -Wall -dynamic -j -hidir $(HI_DIR) -odir $(OBJ_DIR) -i$(SOURCEDIR)  -Wno-unused-imports -Wall-missed-specialisations

SRCDIR := src

# SRCS := $(wildcard $(SRCDIR)/*.hs $(SRCDIR)/*/*.hs)
SRCS := $(wildcard $(SRCDIR)/*.hs $(SRCDIR)/*/*.hs $(SRCDIR)/*/*/*.hs)
OBJS := $(patsubst $(SRCDIR)/%.hs,$(OBJ_DIR)/%.o,$(SRCS))
HIS := $(patsubst $(SRCDIR)/%.hs,$(HI_DIR)/%.hi,$(SRCS))

testy:
	@echo $(SRCS)
	@echo $(OBJS)
	@echo $(HIS)

.PHONY: soucc expr parser all default

default: all test

all: $(OUT_DIR)/soucc $(OUT_DIR)/expr $(OUT_DIR)/parser

soucc: $(OUT_DIR)/soucc
$(OUT_DIR)/soucc: src/Main.hs $(SRCS) | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	ghc $(FLAGS) -o $@ $<

expr: $(OUT_DIR)/expr
$(OUT_DIR)/expr: src/Main_Expr.hs $(OUT_DIR)/soucc | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	ghc $(FLAGS) -o $@ -main-is Main_Expr $<

parser: $(OUT_DIR)/parser
$(OUT_DIR)/parser: src/Main_Parser.hs $(OUT_DIR)/soucc | $(OUT_DIR) $(HI_DIR) $(OBJ_DIR)
	ghc $(FLAGS) -o $@ -main-is Main_Parser $<

$(OUT_DIR) $(CACHE_DIR) $(HI_DIR) $(OBJ_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -fr $(OUT_DIR) $(CACHE_DIR)

.PHONY: test
test: test_parser test_type_checker test_codegen test_expr_parser test_integration
	@echo "all tests successful! :^D"

.PHONY: test_parser
test_parser: $(OUT_DIR)/parser
	@test/test_parser

.PHONY: test_type_checker
test_type_checker:
	@runghc -Wall -i$(SOURCEDIR) test/type_checker.hs

.PHONY: test_codegen
test_codegen:
	@runghc -Wall -i$(SOURCEDIR) test/test_codegen.hs

.PHONY: test_integration
test_integration: $(OUT_DIR)/soucc
	@test/integration_test

.PHONY: test_expr_parser
test_expr_parser: $(OUT_DIR)/parser $(OUT_DIR)/expr
	@test/test_expr_parser

.PHONY: deps
deps: | $(CACHE_DIR)
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/soucc-deps src/Main.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/expr-deps src/Main_Expr.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/parser-deps src/Main_Parser.hs
