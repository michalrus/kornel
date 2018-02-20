let

  sources = rec {
    nixpkgs = import ./fetchNixpkgs.nix {
      rev = "f0fac3b578086066b47360de17618448d066b30e";
      sha256 = "0k0nxp13qdg53rcgr804529bnphqsb5v7rwzn47qq64nb147h011";
    };
    nixpkgsWatchexec = import ./fetchNixpkgs.nix {
      rev = "1bccb28904ff1c1ea2fb6278fc950ebd5c8aed1d";
      sha256 = "0mvvr0wr33rhb3ay1xiakq7dgm3jxkdjfjkm79gphb52l73cfig5";
    };
    hie-nix = (import nixpkgs {}).fetchFromGitHub {
      owner = "domenkozar"; repo = "hie-nix";
      rev = "2b7965f26009b43ecd65c2dcb4d15a53941b726e";
      sha256 = "1b2mv9pvbzk0fy1zjchfmkayya9dg1kq4xk0dqm9bzphz2f4icsv";
    };
  };

  ulib = { lib, ... }: {
    sourceByNegativeRegex = regexes: src:
      builtins.filterSource (path: type:
         let relPath = lib.removePrefix (toString src) (toString path);
         in lib.all (re: builtins.match re relPath == null) regexes) src;

    # TODO: How to fully re-use recursive .gitignore’s? https://git.io/vSo80
    gitignoreToRegexes = gitignorePath:
      builtins.map (line: if lib.hasPrefix "/" line then line else ".*/" + line + "($|/.*)")
        (builtins.map (builtins.replaceStrings ["." "**" "*"] ["\\." ".*" "[^/]*"] )
           (builtins.map (lib.removeSuffix "/")
              (lib.filter (line: line != "" && !(lib.hasPrefix "#" line))
                 (lib.splitString "\n" (builtins.readFile gitignorePath)))));
  };

  watchexec = (import sources.nixpkgsWatchexec {}).watchexec;

  hie = (import sources.hie-nix {}).hie80;

in with (import sources.nixpkgs {}); with (ulib pkgs); let

  haskellPackagesWithOverrides = haskell.packages.ghc802.override {
    overrides = self: super: {
      # To make certain IFD deps survive GC.
      haskell-prevent-ifd-gc = [] ++
        buildPackages.haskellPackages.cabal2nix.all;
    };
  };

  build = let
    src = sourceByNegativeRegex (gitignoreToRegexes ./.gitignore ++ ["/config" "/default.nix"]) ./.;
  in haskell.lib.overrideCabal (haskellPackagesWithOverrides.callCabal2nix "kornel" src {}) (drv: {
    # For speed:
    doHaddock = false;
    # Tests require network.
    #   • `doCheck = false` would remove test deps from nix-shell → useless,
    #   • `checkPhase = ""` doesn’t override (see pkgs/development/haskell-modules/generic-builder.nix).
    checkPhase = ":";
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ prevent-ifd-gc ];
  });

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = [ git watchexec prevent-ifd-gc ] ++
      (with haskellPackagesWithOverrides; [ cabal-install hlint hindent stylish-haskell ]);

    # Caution: leave oldAttrs.shellHook in place, or HIE will break (just HIE!).
    shellHook = oldAttrs.shellHook + ''
      export NIX_PATH='nixpkgs=${sources.nixpkgs}'
    '';
  });

  prevent-ifd-gc = let
    srcs = lib.attrValues sources ++ haskellPackagesWithOverrides.haskell-prevent-ifd-gc;
    in writeTextDir "prevent-ifd-gc" (toString srcs + "\n");

in build // { inherit env hie; }
