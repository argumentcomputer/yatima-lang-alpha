{ sources ? import ./sources.nix }:
let
  pkgs =
    import sources.nixpkgs { overlays = [ (import sources.nixpkgs-mozilla) ]; };
  channel = "nightly";
  date = "2021-01-17";
  targets = [ "wasm32-unknown-unknown" ];
  chan = pkgs.rustChannelOfTargets channel date targets;
in
chan
