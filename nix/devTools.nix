{ nixpkgs, rust, pre-commit-hooks }:
{
  inherit (nixpkgs) niv wasm-pack wasmtime openssl pkg-config nodejs yarn clippy rust-analyzer;
  # inherit (pre-commit-hooks) pre-commit nixpkgs-fmt nix-linter rustfmt;
  inherit rust;
  # inherit grin;
}
