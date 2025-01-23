{
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf hasAttr;
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";

  epkgs = pkgs.emacsPackages;

  combobulate = pkgs.callPackage ./manual/combobulate.nix {
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

  kbd-mode = pkgs.callPackage ./manual/kbd-mode.nix {
    inherit (pkgs) fetchFromGitHub writeText;
    inherit (epkgs) melpaBuild compat;
  };

  eglot-booster = pkgs.callPackage ./manual/eglot-booster.nix {
    inherit (pkgs) fetchFromGitHub writeText;
    inherit (epkgs) melpaBuild;
  };

  emacsExtraPackagesLocal = [
    # combobulate
    # emacscodeium
    # nim-ts-mode
    eglot-booster
    kbd-mode
    org-modern-indent
  ];

  emacsExtraPackages = with epkgs; [
    # auto-yasnippet
    # beframe
    # breadcrumb
    # consult-flyspell
    # consult-yasnippet
    # devdocs
    # dtrt-indent
    # exec-path-from-shell
    # gcmh
    # git-timemachine
    # json-navigator
    # kubel
    # mini-frame
    # nerd-icons-completion
    # nerd-icons-dired
    # no-littering
    # ox-gfm
    # pandoc-mode
    # pdf-tools
    # powershell
    # quickrun
    # rainbow-delimiters
    # sly
    # terraform-doc
    # yaml-pro
    avy
    ace-window
    aggressive-indent
    cape
    chatgpt-shell
    consult
    corfu
    corfu-candidate-overlay
    copilot
    denote
    direnv
    dtrt-indent
    eat
    editorconfig
    ef-themes
    elisp-autofmt
    embark
    embark-consult
    exec-path-from-shell
    format-all
    general
    helpful
    hyperbole
    indent-bars
    keycast
    magit
    marginalia
    markdown-mode
    meow
    nix-ts-mode
    ob-async
    orderless
    org
    org-auto-tangle
    org-modern
    ox-pandoc
    rainbow-mode
    standard-themes
    telephone-line
    terraform-mode
    transient
    treesit-grammars.with-all-grammars
    verb
    vertico
    wgrep
    which-key
    ws-butler
    yasnippet
    yasnippet-capf
    yasnippet-snippets
    zig-mode
  ];
in
{
  config = mkIf config.wolf.roles.editing {
    programs.emacs.extraPackages = epkgs: emacsExtraPackages ++ emacsExtraPackagesLocal;

    home.packages = with pkgs; [
      # Dictionaries for use with flyspell
      (aspellWithDicts (
        ds: with ds; [
          en
          en-computers
          en-science
        ]
      ))

      pandoc

      emacs-lsp-booster
    ];

    # Populate authinfo for gptel to use chatgpt api
    home.file.authinfo = {
      target = ".authinfo";
      text =
        if (hasAttr "openai_token" secrets) then
          ''
            machine api.openai.com login apikey password ${secrets.openai_token}
          ''
        else
          "";
    };

    home.file = {
      ".config/emacs/var/tree-sitter".source =
        "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib";
      ".config/emacs/codeium/codeium_language_server" = {
        source = config.lib.file.mkOutOfStoreSymlink "${pkgs.codeium}/bin/codeium_language_server";
        executable = true;
      };
    };
  };
}
