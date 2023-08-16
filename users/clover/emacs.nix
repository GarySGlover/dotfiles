{pkgs, ...}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      all-the-icons
      counsel
      doom-themes
      fira-code-mode
      hydra
      no-littering
      magit
      posframe
    ];
    extraConfig = ''
      (delete-file (expand-file-name "emacs-config.el" "~/.config/emacs"))
      (org-babel-load-file (expand-file-name "emacs-config.org" "~/.config/emacs"))
    '';
  };
  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  xdg.configFile."emacs/emacs-config.org".source = ./emacs-config.org;

  # Hide emacs from application menu
  xdg.desktopEntries.emacs = {
    name = "Emacs";
    noDisplay = true;
  };

  home.packages = with pkgs; [
    gtk3
    emacs-all-the-icons-fonts
  ];
}
