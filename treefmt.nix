{ pkgs, ... }:
{
  projectRootFile = "flake.nix";

  programs.fourmolu.enable = true;
  programs.cabal-fmt.enable = true;
  programs.nixpkgs-fmt.enable = true;
  programs.yamlfmt.enable = true;
  programs.prettier.enable = true;
}
