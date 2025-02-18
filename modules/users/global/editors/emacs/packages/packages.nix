{
  lib,
  config,
  pkgs,
  ...
}:
let
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";

  epkgsl = with pkgs.emacsPackages; {
    kbd-mode = (
      melpaBuild {
        pname = "kbd-mode";
        version = "1";
        commit = "1";
        src = pkgs.kbd-mode;
        packageRequires = [ ];
        recipe = pkgs.writeText "recipe" ''
          (kbd-mode
            :repo "kmonad/kbd-mode"
            :fetcher github
            :files ("*.el"))
        '';
      }
    );
    transient-compile = (
      melpaBuild {
        pname = "transient-compile";
        version = "1";
        commit = "1";
        src = pkgs.transient-compile;
        packageRequires = [ f ];
        recipe = pkgs.writeText "recipe" ''
          (transient-compile
            :repo "gavv/transient-compile"
            :fetcher github
            :files ("*.el"))
        '';
      }
    );
    org-modern-indent = (
      melpaBuild {
        pname = "org-modern-indent";
        version = "1";
        commit = "1";
        src = pkgs.org-modern-indent;
        packageRequires = [ compat ];
        recipe = pkgs.writeText "recipe" ''
          (org-modern-indent
            :repo "jdtsmith/org-modern-indent"
            :fetcher github
            :files ("*.el"))
        '';
      }
    );
    eglot-booster = (
      melpaBuild {
        pname = "eglot-booster";
        version = "1";
        commit = "1";
        src = pkgs.eglot-booster;
        packageRequires = [ ];
        recipe = pkgs.writeText "recipe" ''
          (eglot-booster
            :repo "jdtsmith/eglot-booster"
            :fetcher github
            :files ("*.el"))
        '';
      }
    );
  };

  emacsExtraPackagesLocal = with epkgsl; [
    kbd-mode
    transient-compile
    org-modern-indent
    eglot-booster
  ];

  emacsExtraPackages = with pkgs.emacsPackages; [
    yaml
    # auto-yasnippet
    breadcrumb
    # consult-flyspell
    consult-yasnippet
    # devdocs
    # exec-path-from-shell
    # gcmh
    git-timemachine
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
    avy
    ace-window
    aggressive-indent
    cape
    chatgpt-shell
    consult
    corfu
    corfu-candidate-overlay
    copilot
    copilot-chat
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
  config = lib.mkIf config.wolf.roles.editing {
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

    home.file = {
      # Populate authinfo for gptel to use chatgpt api
      authinfo = {
        target = ".authinfo";
        text =
          if (lib.hasAttr "openai_token" secrets) then
            ''
              machine api.openai.com login apikey password ${secrets.openai_token}
            ''
          else
            "";
      };

      ".config/emacs/var/tree-sitter".source =
        "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib";
    };
  };
}
