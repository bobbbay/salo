{
  description = "Salo is a toolset to agnostically build and deploy OS images remotely.";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.idris2-pkgs.url = "github:claymager/idris2-pkgs/callToml-tweaks";

  outputs = { self, nixpkgs, idris2-pkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
        let
          overlay = final: prev: {
            idris2 = prev.idris2 // {
              packages = prev.idris2.packages // {
                effect = pkgs.idris2.callNix ./nix/packages/effect.nix {};
              };
            };
          };
          pkgs = import nixpkgs { inherit system; overlays = [ idris2-pkgs.overlay overlay ]; };
          salo = pkgs.idris2.buildTOMLSource ./. ./salo.toml;
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
