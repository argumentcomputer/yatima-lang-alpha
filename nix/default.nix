{ sources ? import ./sources.nix
}:
let
  # default nixpkgs
  nixpkgs = import sources.nixpkgs { overlays = [ (import ./rust-overlay.nix) ]; };

  # gitignore.nix
  gitignoreSource = (import sources."gitignore.nix" { inherit (nixpkgs) lib; }).gitignoreSource;

  pre-commit-hooks = (import sources."pre-commit-hooks.nix");

  rust = import ./rust.nix { inherit nixpkgs; };

  # grinProject = import sources.grin { };
  # grin = grinProject.grin.components.exes.grin;

  src = gitignoreSource ./..;
in
{
  inherit nixpkgs src;

  # provided by shell.nix
  devTools = import ./devTools.nix { inherit nixpkgs rust pre-commit-hooks; };

  # to be built by github actions
  ci = {
    pre-commit-check = pre-commit-hooks.run {
      inherit src;
      hooks = {
        shellcheck.enable = true;
        nixpkgs-fmt.enable = true;
        # nix-linter.enable = true;
        # cargo-check.enable = true;
        # rustfmt.enable = true;
        # clippy.enable = true;
        html-tidy.enable = true;
      };
      # generated files
      excludes = [ "^nix/sources\.nix$" ];
    };
    yatima-native = import ../yatima.nix {
      inherit sources nixpkgs rust;
    };
    yatima-wasi = import ../yatima.nix {
      inherit sources nixpkgs rust;
      target = "wasm32-wasi";
    };
  };
}
