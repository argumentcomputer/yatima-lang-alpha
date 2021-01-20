# Yatima
A programming language for the decentralized web

# Development environment with nix

Set up dev environment assuming [nix](https://nixos.org) and direnv are installed.
```bash
direnv allow
```

To build yatima using naersk

```bash
nix-build yatima.nix
```

To run the CI

```bash
nix-build yatima.nix
```

## Niv manages dependencies

[Niv](https://github.com/nmattia/niv) fixes upgrading dependencies for development.

