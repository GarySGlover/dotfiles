{
  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        opacity = 0.8;
      };
    };
  };

  programs.fish = {
    enable = true;
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };
}
