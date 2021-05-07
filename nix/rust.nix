{ nixpkgs-mozilla
}:
let
  channel = "nightly";
  date = "2021-04-20";
  targets = [ "wasm32-unknown-unknown" "wasm32-wasi" ];
  sha256 = "HZIDqtsVmER8HlVTBTiuNFpLcX4jypueVpTLNutmIxk=";
  rust = (nixpkgs-mozilla.rustChannelOf {
    inherit channel date sha256;
  }).rust.override {
    inherit targets;
    extensions = [ "rust-src" "rust-analysis" ];
  };
in
rust
