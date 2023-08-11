{pkgs, ...}:

let
  packages = with pkgs; [
    emacs-all-the-icons-fonts
  ];
  emacsPackages = with pkgs.emacsPackages; [
    hydra
    no-littering
    magit
  ];
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      all-the-icons
      doom-themes
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

  home.packages = packages ++ emacsPackages;
}
