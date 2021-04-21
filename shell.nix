{ project ? import ./nix/default.nix { }
}:

project.pkgs.mkShell {
  buildInputs = builtins.attrValues project.devTools;
  PKG_CONFIG_PATH = "${project.pkgs.openssl.dev}/lib/pkgconfig";
  shellHook = ''
    ${project.ci.pre-commit-check.shellHook}
  '';
}
