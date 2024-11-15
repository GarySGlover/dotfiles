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

  emacsExtraPackagesLocal = [
    # combobulate
    # emacscodeium
    # nim-ts-mode
    org-modern-indent
  ];

  emacsExtraPackages = with epkgs; [
    # auto-yasnippet
    # avy
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
    ace-window
    aggressive-indent
    cape
    consult
    corfu
    corfu-candidate-overlay
    denote
    direnv
    eat
    editorconfig
    ef-themes
    elisp-autofmt
    embark
    embark-consult
    exec-path-from-shell
    format-all
    general
    gptel
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
    terraform-mode
    transient
    treesit-grammars.with-all-grammars
    verb
    vertico
    yasnippet
    yasnippet-capf
    yasnippet-snippets
    wgrep
    which-key
    ws-butler
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

      # Codeium language server binary
      # codeium

      pandoc
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
      ".config/emacs/var/tree-sitter".source = "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib";
      ".config/emacs/codeium/codeium_language_server" = {
        source = config.lib.file.mkOutOfStoreSymlink "${pkgs.codeium}/bin/codeium_language_server";
        executable = true;
      };
    };
  };
}
