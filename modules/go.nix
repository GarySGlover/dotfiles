# [[file:../modules.org::*Go][Go:1]]
{
  flake.aspects = {
    go.homeManager = { pkgs, ... }: {
      home.packages = with pkgs; [
        gopls
      ];
    };
  };
}
# Go:1 ends here
