let

  pname = "kornel";

  nixpkgs = (import <nixpkgs> {}).pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    rev = "2839b101f927be5daab7948421de00a6f6c084ae";
    sha256 = "0a863cc5462gn1vws87d4qn45zk22m64ri1ip67w0b1a9bmymqdh";
  };

in with import nixpkgs {}; let

  # Temporarily, let’s pick it from nixpkgs:master.
  intero-nix-shim = (import (fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    rev = "7daec8a234da5b097544dd4fb3fa1d2012870d08";
    sha256 = "13j482kskh90mcgdj6wjqbyd8yfyikfp5ipwvrissmshpsypi5sx";
  }) {}).haskellPackages.intero-nix-shim;

  compiler = haskell.packages.ghc802.override {
    overrides = self: super: {

      "html-entities" = lib.overrideDerivation super.html-entities (oldAttrs: {
        postPatch = ''
          substituteInPlace html-entities.cabal \
            --replace 'directory == 1.2.*' 'directory == 1.3.*'
        '';
      });

    };
  };

  # TODO: Consider using `cabal sdist`? https://git.io/vSo8l

  build = let
    src = builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
         lib.all (i: toString i !=            path) [ ./.git ./dist ./result ]
      && lib.all (i:          i != baseNameOf path) [ ".stack-work" ])
      ./.;
  in compiler.callCabal2nix pname src {};

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = with compiler; [ cabal-install hlint hindent stylish-haskell intero-nix-shim ];
  });

in build // { inherit env; }
