{ nixpkgs, system, buildInputs }:
with nixpkgs;
let
  wasm = stdenv.mkDerivation {
    inherit system;
    name = "yatima-web-wasm";
    buildInputs = buildInputs ++ [ wasm-pack ];
    src = ../.;
    buildPhase = ''
      cd web
      wasm-pack build
    '';
    installPhase = ''
      ls -al
      cp -r ./pkg $out
    '';
  };
  yarn = mkYarnPackage {
    inherit system;
    name = "yatima-web";
    buildInputs = [ wasm ];
    yarnPreBuild = ''
      echo "Configure"
      ln -s ${wasm} ./pkg
    '';
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    yarnNix = ./yarn.nix;
  };
in
yarn
