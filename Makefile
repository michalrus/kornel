.PHONY: local ci build lint run devloop autoformat autoformat-check

local: autoformat       build lint
ci:    autoformat-check build lint

sources=$(shell find . -name '*.hs' -a -not -path '*/.*' -a -not -path './dist/*' | sort)

inShell=./nix-shell-persistent --pure --run

build:
	@nix-build -j$$(nproc)
lint:
	@$(inShell) 'exec $(MAKE) _lint'
run:
	@$(inShell) "exec $(MAKE) _run"
devloop:
	@$(inShell) "exec $(MAKE) opts='$(subst ','\'',$(testOpts))' -j$$(nproc) _devloop"
autoformat:
	@$(inShell) "exec $(MAKE) -j$$(nproc) _autoformat"
autoformat-check: autoformat
	@$(inShell) 'status=$$(git status --porcelain | grep -v "^M ") ; [ -z "$$status" ] || { printf >&2 "%s\n%s\n" "fatal: some files are unformatted (or repo unclean):" "$$status" ; exit 1 ; }'


#———————————————————————————————————nix-shell———————————————————————————————————


.PHONY: _autoformat _devloop _run _lint _test

_autoformat: $(patsubst %,dist/autoformat/%_fmt,$(sources))

dist/autoformat/%_fmt: %
	@echo "Formatting $<..."
	@hindent --line-length 80 -XTypeApplications "$<" \
		&& stylish-haskell --inplace "$<" \
		&& mkdir -p "$(dir $@)" && touch "$@" \
		|| true # we want to see real compilation errors

_devloop:
	watchexec -rs SIGKILL "export | grep -F WATCHEXEC ; exec $(MAKE) -j1 _test _lint _run"
_run:
	cabal run -j -- -c ./sample-config.dhall
_lint:
	@hlint $(sources)
_test:
	cabal test -j --show-details=streaming --test-option=--color=always '--test-options=$(subst ','\'',$(testOpts))'
