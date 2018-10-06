let

  sources = rec {
    nixpkgs = import ./fetchNixpkgs.nix {
      rev = "8395f9aa85e621c076334a67d814de8221ce7983";
      sha256 = "04b2gyji9yz9429cy7ah3yidh4clplfgd4smkd0hx06g5n5v790g";
    };
    hie-nix = (import nixpkgs {}).fetchFromGitHub {
      owner = "domenkozar"; repo = "hie-nix";
      rev = "e3113da93b479bec3046e67c0123860732335dd9";
      sha256 = "05rkzjvzywsg66iafm84xgjlkf27yfbagrdcb8sc9fd59hrzyiqk";
    };
    hie-nix-nixpkgs = import "${hie-nix}/fetch-nixpkgs.nix";
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

  inherit (import sources.nixpkgs {}) pkgs;

in with (ulib pkgs); let

  hie = (import sources.hie-nix { pkgs = import sources.hie-nix-nixpkgs {}; }).hie82;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      prelude = self.callCabal2nix "prelude" ./prelude {};

      html-entities = pkgs.haskell.lib.overrideCabal super.html-entities (drv: {
        setupHaskellDepends = with self; [ base Cabal cabal-doctest ]; # https://github.com/nikita-volkov/html-entities/issues/8
      });

      # To make certain IFD deps survive GC.
      haskell-prevent-ifd-gc = []
        ++ pkgs.buildPackages.haskellPackages.cabal2nix.all
        ++ pkgs.buildPackages.cabal2nix.all ++ super.cabal2nix.all;
    };
  };

  build = let
    src = sourceByNegativeRegex (gitignoreToRegexes ./.gitignore ++ ["/config" "/default.nix"]) ./.;
  in pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "kornel" src {}) (drv: {
    # For speed:
    doHaddock = false;
    # Tests require network.
    #   • `doCheck = false` would remove test deps from nix-shell → useless,
    #   • `checkPhase = ""` doesn’t override (see pkgs/development/haskell-modules/generic-builder.nix).
    checkPhase = ":";
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ prevent-ifd-gc ];
  });

  env = pkgs.lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = [ pkgs.git pkgs.watchexec prevent-ifd-gc ] ++
      (with haskellPackages; [ cabal-install hlint hindent stylish-haskell ]);

    # Caution: leave oldAttrs.shellHook in place, or HIE will break (just HIE!).
    shellHook = oldAttrs.shellHook + ''
      export NIX_PATH='nixpkgs=${sources.nixpkgs}'
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LC_ALL=
    '';
  });

  prevent-ifd-gc = let
    srcs = pkgs.lib.attrValues sources ++ haskellPackages.haskell-prevent-ifd-gc;
    in pkgs.writeTextDir "prevent-ifd-gc" (pkgs.lib.concatMapStringsSep "\n" toString srcs + "\n");

in build // { inherit env hie; }
