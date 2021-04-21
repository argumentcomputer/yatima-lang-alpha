{ sources ? import ./sources.nix }:
let
  pkgs =
    import sources.nixpkgs { overlays = [ (import sources.nixpkgs-mozilla) ]; };
  channel = "nightly";
  date = "2021-04-20";
  targets = [ "wasm32-unknown-unknown" "wasm32-wasi" ];
  chan = (pkgs.rustChannelOf {
    inherit channel date;
  }).rust.override {
    inherit targets;
    extensions = [ "rust-src" "rust-analysis" ];
  };
in
chan
