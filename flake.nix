{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    utils.url = "github:yatima-inc/nix-utils";
    utils.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
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
      pkgs = import nixpkgs { inherit system; };
      inherit (lib) getRust buildRustProject testRustProject rustDefault filterRustProject naerskDefault;
      rust = getRust { date = "2021-08-24"; sha256 = "30dHH53OlZt6h2OJxeVJ8IokaQrSaV7aGfhUiv2HU0Q="; };
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
      web = import ./web/default.nix { nixpkgs = pkgs; inherit system; buildInputs = [ project ]; };
    in
    {
      packages.${crateName} = project;
      packages."yatima-web" = web;

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
        buildInputs = [ rust ] ++ builtins.attrValues devTools;
      };
    });
}
