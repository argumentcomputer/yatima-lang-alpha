# import niv sources and the pinned nixpkgs
{ sources ? import ./nix/sources.nix
, nixpkgs ? import sources.nixpkgs { overlays = [ (import ./nix/rust-overlay.nix) ]; }
, target ? null
, rust ? import ./nix/rust.nix {
    inherit nixpkgs;
  }
  # configure naersk to use our pinned rust compiler
, naersk ? nixpkgs.callPackage sources.naersk {
    rustc = rust;
    cargo = rust;
  }
}:
with builtins;
let
  # import rust compiler

  # tell nix-build to ignore the `target` directory
  project = builtins.filterSource
    (path: type: type != "directory" || builtins.baseNameOf path != "target")
    ./.;
in
naersk.buildPackage {
  name = "yatima";
  version = "0.1.0";
  buildInputs = with nixpkgs; [ openssl pkg-config project ];
  PKG_CONFIG_PATH = "${nixpkgs.openssl.dev}/lib/pkgconfig";
  targets = if target then [ target ] else [ ];
  src = project;
  remapPathPrefix =
    true; # remove nix store references for a smaller output package
}
