{
  inputs = {
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    naersk.url = "github:nix-community/naersk";

    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, fenix, naersk, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
          {
            packages.salo = (
              naersk.lib.${system}.override {
                inherit (fenix.packages.${system}.minimal) cargo rustc;
              }
            ).buildPackage { src = ./salo; };

            defaultPackage = self.packages.${system}.salo;

            devShell = pkgs.mkShell {
              buildInputs = with pkgs; [ cargo-watch ];
            };
          }
    );
}
