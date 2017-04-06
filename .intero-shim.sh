#!/bin/sh

set -o errexit -o nounset -o noclobber
CDPATH='' cd -- "$(dirname -- "$0")"
NIX_PATH=$(cat NIX_PATH) ; export NIX_PATH

dropUpToDashes='while [ "$1" != "--" ] ; do shift ; done'
shellQuoteArguments(){ getopt --options "" --shell sh -- -- "$@" | cut -c 4- ; }

[ "$1" = "path" ] && [ "$2" = "--project-root" ] && pwd && exit 0
[ "$1" = "exec" ] && { eval "$dropUpToDashes" ; exec nix-shell --pure --argstr isSafe yes --run "exec     $(shellQuoteArguments "$@")" ; }
[ "$1" = "ghc"  ] && { eval "$dropUpToDashes" ; exec nix-shell --pure --argstr isSafe yes --run "exec ghc $(shellQuoteArguments "$@")" ; }

[ "$1" = "ghci" ] && {
  shift
  args=$(for arg in "$@" ; do echo "$arg" ; done |\
    grep -Ev -- '--(docker-run-args|no-build|no-load)' |\
    sed 's/^--ghci-option/--ghc-option/ ; /^--verbosity$/,+1d' |\
    xargs getopt --options "" --shell sh -- -- | cut -c 4-)
  exec nix-shell --pure --argstr isSafe yes --run "exec cabal repl $args"
}

[ "$1" = "ide" ] && [ "$2" = "targets" ] && {
  name=$(cat -- *.cabal | grep -m 1 '^name:' | awk '{ print $2 }')
  cat -- *.cabal |\
    grep -E '^library$|^executable |^test-suite ' |\
    sed "s/^library$/lib/ ; s/^executable /exe:/ ; s/^test-suite /test:/ ; s/^/$name:/"
  exit 0
}

# TODO: hoogle?

exit 1
