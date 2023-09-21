{
  programs.kitty = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font";
      size = 10;
    };
    shellIntegration.enableFishIntegration = true;
    extraConfig = ''
      background_opacity 0.8
      background #2e2735
    '';
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      nfhr = "~/dotfiles/scripts/nix-flake-home-build.sh";
      nfht = "~/dotfiles/scripts/nix-flake-home-test-build.sh";
      nft = "~/dotfiles/scripts/nix-flake-test-build.sh";
      nfr = "~/dotfiles/scripts/nix-flake-rebuild.sh";
      nfu = "~/dotfiles/scripts/nix-flake-update.sh";
    };
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };
}
