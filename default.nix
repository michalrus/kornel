let

  sources = rec {
    nixpkgs = import ./fetchNixpkgs.nix {
      rev = "327a84749ed48a20736fdf20b9dd4f5723b01912";
      sha256 = "0sdzl5vw3qlwhlfhjhsdzffc751hipfcmrgajsxpv3l5lykjvdsq";
      sha256Unpacked = "0fgdcy49w073iiy9i65928219n1fy3w61xxsyqn6d8a72dxpcs3n";
    };
    nixpkgsWatchexec = import ./fetchNixpkgs.nix {
      rev = "1bccb28904ff1c1ea2fb6278fc950ebd5c8aed1d";
      sha256 = "0mvvr0wr33rhb3ay1xiakq7dgm3jxkdjfjkm79gphb52l73cfig5";
      sha256Unpacked = "04i20pwq1cfgqs2ds358yzq9c38ip55mkx35w8nhx44xs6y27g9x";
    };
    hie-nix = (import nixpkgs {}).fetchFromGitHub {
      owner = "domenkozar"; repo = "hie-nix";
      rev = "b8d3fec2d73e43bae11116d0a138168676ae2365";
      sha256 = "1g00a85krcyalyr2p189dk0rs62gsnkj5cjs0pam060nmznqb9j4";
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

  watchexec = (import sources.nixpkgsWatchexec {}).watchexec;

  hie = (import sources.hie-nix { pkgs = import sources.hie-nix-nixpkgs {}; }).hie82;

in with (import sources.nixpkgs {}); with (ulib pkgs); let

  haskellPackagesWithOverrides = haskell.packages.ghc822.override {
    overrides = self: super: {
      prelude = self.callCabal2nix "prelude" ./prelude {};

      html-entities = haskell.lib.overrideCabal super.html-entities (drv: {
        setupHaskellDepends = with self; [ base Cabal cabal-doctest ]; # https://github.com/nikita-volkov/html-entities/issues/8
      });

      # To make certain IFD deps survive GC.
      haskell-prevent-ifd-gc = []
        ++ buildPackages.haskellPackages.cabal2nix.all
        ++ buildPackages.cabal2nix.all ++ super.cabal2nix.all;
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
    in writeTextDir "prevent-ifd-gc" (lib.concatMapStringsSep "\n" toString srcs + "\n");

in build // { inherit env hie; }
