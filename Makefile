export NIX_PATH := $(shell cat NIX_PATH)

#——————————— Proxies ———————————————————————————————————————————————————————————

.PHONY: all ghci nix-build nix-shell clean

all: build.nix
	@nix-shell --pure --argstr isSafe yes --run "exec $(MAKE) isSafe=yes _unsafe_all"

ghci: build.nix
	@nix-shell --pure --argstr isSafe yes --run "exec cabal repl"

nix-build: build.nix
	@nix-build --argstr isSafe yes

nix-shell: build.nix
	@nix-shell --argstr isSafe yes

clean:
	rm -rv dist/ result build.nix || true

#——————————— Auto-generated files ——————————————————————————————————————————————

build.nix: build.cabal
	nix-shell --pure -p cabal2nix --run "cabal2nix . > build.nix"

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
