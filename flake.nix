{
  description = "My Idris 2 package";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.idris2-pkgs.url = "github:claymager/idris2-pkgs";

  outputs = { self, nixpkgs, idris2-pkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "i686-linux" ] (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ idris2-pkgs.overlay ]; };
          salo = pkgs.idris2.buildTOMLSource ./. ./salo.toml;
        in
          {
            packages.salo = salo;
            defaultPackage = self.packages.${system}.salo;

            devShell = pkgs.mkShell {
              buildInputs = with pkgs; [ idris2.packages.lsp ];
              buildInputsFrom = [ self.packages.${system}.salo ];
            };
          }
    );
}
