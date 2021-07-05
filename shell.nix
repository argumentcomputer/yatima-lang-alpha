{ project ? import ./nix/default.nix { }
}:

project.nixpkgs.mkShell {
  buildInputs = builtins.attrValues project.devTools;
  PKG_CONFIG_PATH = "${project.nixpkgs.openssl.dev}/lib/pkgconfig";
  shellHook = ''
    ${project.ci.pre-commit-check.shellHook}
  '';
}
