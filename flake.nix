{
  description = "Salo is a toolset to agnostically build and deploy OS images remotely.";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.idris2-pkgs.url = "github:claymager/idris2-pkgs/callToml-tweaks";

  outputs = { self, nixpkgs, idris2-pkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ idris2-pkgs.overlay ]; };
          extended = {};
          salo = pkgs.idris2.extendCallTOML extended ./salo.toml;
        in
          {
            packages.salo = salo;
            defaultPackage = self.packages.${system}.salo;

            pkgs = pkgs;

            devShell = pkgs.mkShell {
              buildInputs = with pkgs; [
                idris2.packages.lsp
              ];
              buildInputsFrom = [ self.packages.${system}.salo ];
            };
          }
    );
}
