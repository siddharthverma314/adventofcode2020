{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
  };
  outputs = { self, nixpkgs, ...}:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };
    in {
      devShell."${system}" = pkgs.mkShell {
        buildInputs = [
          (pkgs.haskell.packages.ghc8102.ghcWithPackages (p: [
            p.haskell-language-server
          ]))
        ];
      };
    };
}
