let

  pname = "kornel";

  nixpkgs = (import <nixpkgs> {}).pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    rev = "2839b101f927be5daab7948421de00a6f6c084ae";
    sha256 = "0a863cc5462gn1vws87d4qn45zk22m64ri1ip67w0b1a9bmymqdh";
  };

in with import nixpkgs {}; let

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

  fmtInputs = (with haskellPackages; [ hlint hindent stylish-haskell ]);

  # TODO: Consider using `cabal sdist`? https://git.io/vSo8l

  build = let
    src = builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
         lib.all (i: toString i !=            path) [ ./.git ./dist ./result ]
      && lib.all (i:          i != baseNameOf path) [ ".stack-work" ])
      ./.;
  in lib.overrideDerivation (compiler.callCabal2nix pname src {}) (oldAttrs: {
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ fmtInputs;
    preBuild = ''
      hashRaw=$(${nix}/bin/nix-hash --type sha256 .)
      make _autoformat
      rm -r dist/autoformat
      hashFmt=$(${nix}/bin/nix-hash --type sha256 .)
      [ "$hashRaw" = "$hashFmt" ] || { echo >&2 'fatal: a file was commited unformatted' ; exit 1 ; }
    '';
    postBuild = ''
      hlint .
    '';
  });

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = fmtInputs ++ (with compiler; [ cabal-install ]);
  });

in build // { inherit env; }
