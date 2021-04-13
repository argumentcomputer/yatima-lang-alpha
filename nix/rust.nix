{ sources ? import ./sources.nix }:
let
  pkgs =
    import sources.nixpkgs { overlays = [ (import sources.nixpkgs-mozilla) ]; };
  channel = "nightly";
  date = "2021-03-18";
  targets = [ "wasm32-unknown-unknown" "wasm32-wasi" ];
  chan = pkgs.rustChannelOfTargets channel date targets;
in
chan
