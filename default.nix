{ isSafe ? "no" }:

let

  inherit (import <nixpkgs> {}) pkgs lib;

  compiler = pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      "html-entities" = lib.overrideDerivation super.html-entities (oldAttrs: {
        postPatch = ''
          substituteInPlace html-entities.cabal \
            --replace 'directory == 1.2.*' 'directory == 1.3.*'
        '';
      });
    };
  };

  build = compiler.callPackage ./build.nix {};

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = with compiler; [ cabal-install hlint hindent stylish-haskell ];
  });

in

if (isSafe == "yes")
  then build // { inherit env; }
  else throw "Please, use ‘make’ to run ‘nix-*’ commands."
