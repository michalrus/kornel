.PHONY: all ghci nix-build nix-shell clean

all:
	@nix-shell --pure --run "exec $(MAKE) isSafe=yes _unsafe_all"

repl: ghci
	@nix-shell --pure --run "exec cabal repl"

clean:
	@nix-shell --pure --run "exec cabal clean"

autoformat:
	@nix-shell --pure --run "exec $(MAKE) isSafe=yes _unsafe_autoformat"

ci:
	@nix-build


#——————————— Don’t run these directly… probably ————————————————————————————————


.PHONY: _unsafe_check_safety _unsafe_all _unsafe_build _unsafe_test _unsafe_autoformat

_unsafe_all: _unsafe_check_safety _unsafe_test

_unsafe_build: _unsafe_check_safety _unsafe_autoformat
	@cabal build -j
	@hlint .

_unsafe_test: _unsafe_check_safety _unsafe_build
	@cabal test -j --show-details=direct

_unsafe_autoformat: _unsafe_check_safety $(shell find . -name '*.hs' -a -not -path './.*' -a -not -path './dist/*' -printf 'dist/autoformat/%P_fmt\n')

dist/autoformat/%_fmt: %
	@echo "Formatting $<..."
	@hindent --line-length 80 "$<" \
		&& stylish-haskell --inplace "$<" \
		&& mkdir -p $(dir $@) \
		&& touch "$@" \
		|| true # Let’s get to the real compilation errors.

_unsafe_check_safety:
ifeq ($(isSafe),yes)
else
	$(error Please, don’t call ‘_unsafe_*’ rules manually)
endif
