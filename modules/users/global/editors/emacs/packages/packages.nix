{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) mkIf hasAttr;
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";

  epkgs = pkgs.emacsPackages;

  combobulate = pkgs.callPackage ./manual/combobulate.nix {
    inherit (pkgs) fetchFromGitHub writeText;
    inherit (epkgs) melpaBuild compat;
  };

  indent-bars = pkgs.callPackage ./manual/indent-bars.nix {
    inherit (pkgs) fetchFromGitHub writeText;
    inherit (epkgs) melpaBuild compat;
  };

  emacscodeium = pkgs.callPackage ./manual/codeium.nix {
    inherit (pkgs) fetchFromGitHub;
    inherit (epkgs) trivialBuild;
  };

  nim-ts-mode = pkgs.callPackage ./manual/nim-ts-mode.nix {
    inherit (pkgs) fetchFromGitHub writeText;
    inherit (epkgs) melpaBuild nim-mode;
  };

  org-modern-indent = pkgs.callPackage ./manual/org-modern-indent.nix {
    inherit (pkgs) fetchFromGitHub writeText;
    inherit (epkgs) melpaBuild compat;
  };

  emacsExtraPackagesLocal = [
    combobulate
    emacscodeium
    indent-bars
    nim-ts-mode
    org-modern-indent
  ];

  emacsExtraPackages = with epkgs; [
    # flymake
    # eldoc
    # eglot
    # project
    ace-window
    aggressive-indent
    # auto-yasnippet
    # avy
    # beframe
    # breadcrumb
    cape
    consult
    # consult-flyspell
    # consult-yasnippet
    corfu
    corfu-candidate-overlay
    denote
    # devdocs
    # direnv
    # dtrt-indent
    # eat
    # ef-themes
    elisp-autofmt
    embark
    embark-consult
    # exec-path-from-shell
    # format-all
    general
    # gcmh
    # git-timemachine
    gptel
    helpful
    hyperbole
    # hydra
    # json-mode
    # json-navigator
    # kubel
    keycast
    magit
    marginalia
    # markdown-mode
    meow
    # mini-frame
    # nerd-icons-completion
    # nerd-icons-dired
    nix-ts-mode
    # no-littering
    orderless
    org
    org-modern
    org-modern-indent
    # ox-gfm
    # pandoc-mode
    # pdf-tools
    # powershell
    # quickrun
    # rainbow-delimiters
    # sly
    standard-themes
    # terraform-doc
    # terraform-mode
    transient
    treesit-grammars.with-all-grammars
    # verb
    vertico
    # visual-fill-column
    # which-key
    # yaml-pro
    yasnippet
    yasnippet-capf
    yasnippet-snippets
    # zig-mode
  ];
in {
  config = mkIf config.wolf.roles.editing {
    programs.emacs.extraPackages = epkgs: emacsExtraPackages ++ emacsExtraPackagesLocal;

    home.packages = with pkgs; [
      # Dictionaries for use with flyspell
      (aspellWithDicts (ds: with ds; [en en-computers en-science]))

      # Codeium language server binary
      # codeium
    ];

    # Populate authinfo for gptel to use chatgpt api
    home.file.authinfo = {
      target = ".authinfo";
      text =
        if (hasAttr "openai_token" secrets)
        then ''
          machine api.openai.com login apikey password ${secrets.openai_token}
        ''
        else "";
    };

    home.file = {
      ".config/emacs/var/tree-sitter".source = "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib";
      ".config/emacs/codeium/codeium_language_server" = {
        source = config.lib.file.mkOutOfStoreSymlink "${pkgs.codeium}/bin/codeium_language_server";
        executable = true;
      };
    };
  };
}
