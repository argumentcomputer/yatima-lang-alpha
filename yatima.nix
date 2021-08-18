# import niv sources and the pinned nixpkgs
{ sources ? import ./nix/sources.nix
, nixpkgs ? import sources.nixpkgs { overlays = [ (import ./nix/rust-overlay.nix) ]; }
, target ? null
, rust ? import ./nix/rust.nix {
    inherit nixpkgs;
  }
  # Wether to run the tests when building
, doCheck ? true
  # configure naersk to use our pinned rust compiler
, naersk ? nixpkgs.callPackage sources.naersk {
    rustc = rust;
    cargo = rust;
  }
, src ? ./.
  # This is impure so it should be provided with the correct information
, system ? builtins.currentSystem
}:
naersk.buildPackage {
  name = "yatima";
  version = "0.1.0";
  buildInputs = with nixpkgs; [ openssl pkg-config ];
  PKG_CONFIG_PATH = "${nixpkgs.openssl.dev}/lib/pkgconfig";
  targets = if target then [ target ] else [ ];
  inherit src;
  doCheck = doCheck;
  remapPathPrefix =
    true; # remove nix store references for a smaller output package
}
