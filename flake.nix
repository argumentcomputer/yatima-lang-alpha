{
  inputs = {
    utils.url = "github:yatima-inc/nix-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , utils
    , pre-commit-hooks
    }:
    let
      flake-utils = utils.inputs.flake-utils;
    in
    flake-utils.lib.eachDefaultSystem (system:
    let
      pre-commit = import pre-commit-hooks { inherit system; };
      lib = utils.lib.${system};
      pkgs = utils.nixpkgs.${system};
      inherit (lib) buildRustProject testRustProject rustDefault filterRustProject naerskDefault;
      rust = rustDefault;
      naersk = naerskDefault;

      crateName = "yatima";
      src = ./.;

      yatima-nix = import ./yatima.nix;
      project = yatima-nix {
        inherit naersk rust src system;
        nixpkgs = pkgs;
      };
      devTools = import ./nix/devTools.nix { 
        inherit rust;
        pre-commit-hooks = pre-commit;
        nixpkgs = pkgs;
      };
    in
    {
      packages.${crateName} = project;

      defaultPackage = self.packages.${system}.${crateName};

      # nix flake check
      checks.${crateName} = yatima-nix {
        inherit naersk rust src system;
        nixpkgs = pkgs;
        doCheck = true;
      };

      # `nix run`
      apps.${crateName} = flake-utils.lib.mkApp {
        name = "yatima";
        drv = self.packages.${system}.${crateName};
      };
      defaultApp = self.apps.${system}.${crateName};

      # `nix develop`
      devShell = pkgs.mkShell {
        inherit system;
        inputsFrom = builtins.attrValues self.packages.${system};
        buildInputs = [ rust ];# ++ builtins.attrValues devTools;
      };
    });
}
