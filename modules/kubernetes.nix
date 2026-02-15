{
  flake.aspects.kubernetes = {
    homeManager =
      { pkgs, ... }:
      {
        config = {
          home.packages = with pkgs; [
            kubectl
            kubernetes-helm
            k9s
          ];
        };
      };
  };
}
