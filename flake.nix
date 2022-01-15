{
  description = "Salo is a toolset to agnostically build and deploy OS images remotely.";

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in rec {
      devShell.x86_64-linux = pkgs.mkShell {
        nativeBuildInputs = with pkgs; with haskellPackages; [ alex happy ghcid ];
      };
    };
}
