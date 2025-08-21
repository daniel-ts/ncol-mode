
{
  description = "Dev shell with Emacs + dash.el from nixos stable";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux"; # adjust if not on x86_64
      pkgs = import nixpkgs { inherit system; };
      # define Emacs with dash.el
      myEmacs = pkgs.emacs.pkgs.withPackages (epkgs: [
        epkgs.dash
      ]);
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          myEmacs
        ];
      };
    };
}
