{ pkgs }:
let
  myPackages = with pkgs; [
    omnisharp-roslyn
    csharpier

    dotnet-sdk
  ];
in
{
  packages = myPackages;
  shell = pkgs.mkShell {
    name = "dotnet";
    packages = myPackages;
  };
}
