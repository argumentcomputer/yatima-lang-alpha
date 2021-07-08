{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    utils.url = "github:numtide/flake-utils";
    # grin.url = "github:yatima-inc/grin";
    naersk-lib.url = "github:nmattia/naersk";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
    , utils
    , naersk-lib
      # , grin
    , pre-commit-hooks
    }:
    utils.lib.eachDefaultSystem (system:
    let
      overlays = [ (import ./nix/rust-overlay.nix) ];
      pkgs = import nixpkgs { inherit system overlays; };
      pre-commit = import pre-commit-hooks;
      lib = pkgs.lib;
      rust = import ./nix/rust.nix { nixpkgs = pkgs; };
      naersk = naersk-lib.lib."${system}".override {
        rustc = rust;
        cargo = rust;
      };

      crateName = "yatima";
      src = builtins.filterSource
        (path: type: type != "directory" || builtins.baseNameOf path != "target")
        ./.;

      project = import ./yatima.nix {
        inherit naersk rust;
        nixpkgs = pkgs;
      };
      run = name: command: derivation {
        inherit name system src;
        builder = "${pkgs.bash}/bin/bash";
        buildInputs = [ rust project ];
        args = [ "-c" command ];
      };
      devTools = import ./nix/devTools.nix { inherit rust; pre-commit-hooks = pre-commit; nixpkgs = pkgs; };

    in
    {
      packages.${crateName} = project;

      defaultPackage = self.packages.${system}.${crateName};

      # nix flake check
      checks.${crateName} = run "${crateName}-cargo-test" "${rust}/bin/cargo test";

      # `nix run`
      apps.${crateName} = utils.lib.mkApp {
        name = "yatima";
        drv = self.packages.${system}.${crateName};
      };
      defaultApp = self.apps.${system}.${crateName};

      # `nix develop`
      devShell = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.packages.${system};
        nativeBuildInputs = [ rust ];
        buildInputs = lib.attrsets.attrValues devTools;
      };
    });
}
