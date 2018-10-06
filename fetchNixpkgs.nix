#
# This recipe provides a way to fetch Nixpkgs with an empty
# NIX_PATH. This comes in handy if you want to remove impure
# references to the NIX_PATH from your code base.
#
# Courtesy of
# <https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH>.
#

{ rev                             # The Git revision of nixpkgs to fetch
, sha256                          # The SHA256 of the downloaded data
, system ? builtins.currentSystem # This is overridable if necessary
}:

# TODO: switch to just this after Nix 2.0 is much more mainstream:
#
#   builtins.fetchTarball {
#     url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
#     sha256 = sha256;
#   }

(rec {
  tarball = import <nix/fetchurl.nix> {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

  builtin-paths = import <nix/config.nix>;

  script = builtins.toFile "nixpkgs-unpacker" ''
    "$coreutils/mkdir" "$out"
    cd "$out"
    "$gzip" --decompress < "$tarball" | "$tar" -x --strip-components=1
  '';

  nixpkgs = builtins.derivation ({
    name = "nixpkgs-${builtins.substring 0 6 rev}";

    builder = builtins.storePath builtin-paths.shell;

    args = [ script ];

    inherit tarball system;

    tar       = builtins.storePath builtin-paths.tar;
    gzip      = builtins.storePath builtin-paths.gzip;
    coreutils = builtins.storePath builtin-paths.coreutils;
  });
}).nixpkgs
