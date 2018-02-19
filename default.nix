let

  pname = "kornel";

  nixpkgs = (import <nixpkgs> {}).pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    rev = "f0fac3b578086066b47360de17618448d066b30e";
    sha256 = "1mpwdminwk1wzycwmgi2c2kwpbcfjwmxiakn7bmvvsaxb30gwyyb";
  };

in with import nixpkgs {}; let

  compiler = haskell.packages.ghc802;

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

  hie = let
    hie-nix = fetchFromGitHub {
      owner = "domenkozar"; repo = "hie-nix";
      rev = "2b7965f26009b43ecd65c2dcb4d15a53941b726e";
      sha256 = "1b2mv9pvbzk0fy1zjchfmkayya9dg1kq4xk0dqm9bzphz2f4icsv";
    };
  in (import hie-nix {}).hie80.overrideAttrs (oldAttrs: {
    postInstall = "echo ${hie-nix} >$out/prevent-ifd-gc";
  });

in build // { inherit env hie; }
