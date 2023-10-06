{
  pkgs,
  config,
  ...
}: let
  secrets = import ../../secrets/clover-secrets.nix;
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-gtk3;
    extraPackages = epkgs:
      with epkgs; [
        all-the-icons
        consult
        counsel
        doom-themes
        exec-path-from-shell
        forge
        format-all
        helpful
        magit
        nix-mode
        no-littering
        orderless
        posframe
        treesit-grammars.with-all-grammars
        vertico
        vertico-posframe
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
    alejandra
    gtk3
    rnix-lsp
  ];

  home.file.authinfo = {
    target = ".authinfo";
    text = ''
      machine api.github.com login ${secrets.github_user}^forge password ${secrets.github_magit_forge_token}
    '';
  };
}
