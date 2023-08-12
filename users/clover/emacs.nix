{pkgs, ...}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      all-the-icons
      doom-themes
      fira-code-mode
      hydra
      no-littering
      magit
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

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
  ];
}
