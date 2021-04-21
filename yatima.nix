# import niv sources and the pinned nixpkgs
{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, target ? null
}:
with builtins;
let
  # import rust compiler
  rust = import ./nix/rust.nix { inherit sources; };

  # configure naersk to use our pinned rust compiler
  naersk = pkgs.callPackage sources.naersk {
    rustc = rust;
    cargo = rust;
  };

  # tell nix-build to ignore the `target` directory
  src = builtins.filterSource
    (path: type: type != "directory" || builtins.baseNameOf path != "target")
    ./.;
in
naersk.buildPackage {
  buildInputs = with pkgs; [ openssl pkg-config ];
  PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
  # OPENSSL_DIR = "${pkgs.openssl.dev}";
  targets = if target then [ target ] else [ ];
  inherit src;
  remapPathPrefix =
    true; # remove nix store references for a smaller output package
}
