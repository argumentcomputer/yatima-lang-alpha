{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    utils.url = "github:numtide/flake-utils";
    nixpkgs-mozilla-src = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };
    grin.url = "github:yatima-inc/grin";
    naersk.url = "github:nmattia/naersk";
  };

  outputs =
    { self
    , nixpkgs
    , utils
    , nixpkgs-mozilla-src
    , naersk
    , grin
    }:
    utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      nixpkgs-mozilla = pkgs.callPackage (nixpkgs-mozilla-src + "/package-set.nix") { };
      rust = import ./nix/rust.nix { inherit nixpkgs-mozilla; };
      naersk-lib = naersk.lib."${system}".override {
        rustc = rust;
        cargo = rust;
      };

      crateName = "yatima";

      project = naersk-lib.buildPackage {
        buildInputs = with pkgs; [ openssl pkg-config ];
        PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
        targets = [ ];
        root = ./.;
        remapPathPrefix =
          true; # remove nix store references for a smaller output package
      };

    in
    {
      packages.${crateName} = project;

      defaultPackage = self.packages.${system}.${crateName};

      # `nix run`
      apps.yatima = utils.lib.mkApp {
        drv = self.packages.${system}.${crateName};
      };
      defaultApp = self.packages.${system}.${crateName};

      # `nix develop`
      devShell = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.packages.${system};
        nativeBuildInputs = [ rust ];
        buildInputs = with pkgs; [
          rust-analyzer
          clippy
          rustfmt
          grin
        ];
      };
    });
}
