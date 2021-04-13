{ sources ? import ./sources.nix
}:
let
  # default nixpkgs
  pkgs = import sources.nixpkgs { };

  # gitignore.nix 
  gitignoreSource = (import sources."gitignore.nix" { inherit (pkgs) lib; }).gitignoreSource;

  pre-commit-hooks = (import sources."pre-commit-hooks.nix");

  rust = (import ./rust.nix { inherit sources; });

  src = gitignoreSource ./..;
in
{
  inherit pkgs src;

  # provided by shell.nix
  devTools = {
    inherit (pkgs) niv wasm-pack wasmtime valgrind;
    inherit (pre-commit-hooks) pre-commit nixpkgs-fmt nix-linter;
    inherit rust;
  };

  # to be built by github actions
  ci = {
    pre-commit-check = pre-commit-hooks.run {
      inherit src;
      hooks = {
        shellcheck.enable = true;
        nixpkgs-fmt.enable = true;
        nix-linter.enable = true;
        # cargo-check.enable = true;
        # rustfmt.enable = true;
        # clippy.enable = true;
        html-tidy.enable = true;
      };
      # generated files
      excludes = [ "^nix/sources\.nix$" ];
    };
    yatima-native = import ../yatima.nix {
      inherit sources pkgs;
    };
    yatima-wasi = import ../yatima.nix {
      inherit sources pkgs;
      target = "wasm32-wasi";
    };
  };
}
