{pkgs, config, ...}:

let
  secrets = import ../../secrets/clover-secrets.nix;
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      all-the-icons
      consult
      counsel
      doom-themes
      exec-path-from-shell
      forge
      hydra
      magit
      no-littering
      orderless
      popper
      posframe
      vertico
      which-key
      which-key-posframe
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

  home.file.authinfo = {
    target = ".authinfo";
    text = ''
      machine api.github.com login ${secrets.github_user}^forge password ${secrets.github_magit_forge_token}
    '';
  };

}
