{
  description = "Declarative, reproducible, statically typed build and deployment for Operating Systems.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nci.url = "github:yusdacra/nix-cargo-integration";
    nci.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, nci, ... }:
    nci.lib.makeOutputs {
      root = ./.;
      overrides = {
        shell = common: prev: {
          packages = prev.packages ++ (with common.pkgs; [
            rust-analyzer
            cargo-watch
          ]);
        };
      };
    };
}
