{
  inputs = {
    utils.url = "github:yatima-inc/nix-utils";
    # grin.url = "github:yatima-inc/grin";
  };

  outputs =
    { self
    , utils
      # , grin
    }:
    let
      flake-utils = utils.inputs.flake-utils;
    in
    flake-utils.lib.eachDefaultSystem (system:
    let
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
        inputsFrom = builtins.attrValues self.packages.${system};
        nativeBuildInputs = [ rust ];
        buildInputs = with pkgs; [
          rust-analyzer
          clippy
          rustfmt
          yarn
          # grin
        ];
      };
    });
}
