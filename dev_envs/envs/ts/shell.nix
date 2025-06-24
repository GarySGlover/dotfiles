{ pkgs }:
let
  myPackages = with pkgs; [
    typescript
    typescript-language-server
    prettier
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "typescript";
    packages = myPackages;
  };
}
