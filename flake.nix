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

          cargo = fenix.packages.${system}.minimal.cargo;
          rustc = fenix.packages.${system}.minimal.rustc;
        in
          {
            nixpkgs.overlays = [ fenix.overlay ];

            packages = {
              saloc = (
                naersk.lib.${system}.override {
                  inherit (fenix.packages.${system}.minimal) cargo rustc;
                }
              ).buildPackage { src = ./crates/saloc; };

              salo = (
                naersk.lib.${system}.override {
                  inherit (fenix.packages.${system}.minimal) cargo rustc;
                }
              ).buildPackage { src = ./crates/salo; };
            };

            defaultPackage = self.packages.${system}.saloc;

            devShell = pkgs.mkShell {
              buildInputs = with pkgs; [
                openssl
                pkg-config
                cargo-watch
              ];
            };
          }
    );
}
