{
  description = "Salo is a toolset to agnostically build and deploy OS images remotely.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    idris2-pkgs.url = "github:claymager/idris2-pkgs";
    nixpkgs.follows = "idris2-pkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, idris2-pkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" "i686-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ idris2-pkgs.overlay ]; };
        inherit (pkgs.idris2-pkgs._builders) idrisPackage devEnv;

        salo = idrisPackage ./. { };
        runTests = idrisPackage ./test { extraPkgs.salo = salo; };
      in
      {
        defaultPackage = salo;

        packages = { inherit salo runTests; };

        devShell = pkgs.mkShell {
          buildInputs = [ (devEnv salo) ];
        };
      }
    );
}