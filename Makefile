.PHONY: all repl clean autoformat ci

all:
	@nix-shell --pure --run "exec $(MAKE) _all"

repl:
	@nix-shell --pure --run "exec cabal repl"

clean:
	@nix-shell --pure --run "exec cabal clean"

autoformat:
	@nix-shell --pure --run "exec $(MAKE) _autoformat"

ci:
	@nix-build


#——————————— Don’t run these directly… probably ————————————————————————————————


.PHONY: _all _build _test _autoformat

_all: _test

_build: _autoformat
	@cabal build -j
	@hlint .

_test: _build
	@cabal test -j --show-details=direct

_autoformat: $(shell find . -name '*.hs' -a -not -path './.*' -a -not -path './dist/*' -printf 'dist/autoformat/%P_fmt\n')

dist/autoformat/%_fmt: %
	@echo "Formatting $<..."
	@hindent --line-length 80 "$<" \
		&& stylish-haskell --inplace "$<" \
		&& mkdir -p $(dir $@) \
		&& touch "$@" \
		|| true # Let’s get to the real compilation errors.
