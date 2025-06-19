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
    ws-butler = (
      melpaBuild {
        pname = "ws-butler";
        version = "1";
        commit = "1";
        src = pkgs.ws-butler;
        packageRequires = [ ];
        recipe = pkgs.writeText "recipe" ''
          (ws-butler
            :repo "lewang/ws-butler"
            :fetcher github
            :files ("*.el"))
        '';
      }
    );
  };

  emacsExtraPackagesLocal = with epkgsl; [
    kbd-mode
    transient-compile
    eglot-booster
    ws-butler
  ];

  emacsExtraPackages = with pkgs.emacsPackages; [
    terraform-doc
    yaml
    yaml-pro
    # auto-yasnippet
    breadcrumb
    # consult-flyspell
    consult-yasnippet
    # devdocs
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
    ace-window
    aggressive-indent
    avy
    cape
    consult
    copilot
    corfu
    corfu-candidate-overlay
    coterm
    denote
    direnv
    disproject
    dtrt-indent
    eat
    editorconfig
    ef-themes
    elisp-autofmt
    embark
    embark-consult
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
    ox-pandoc
    rainbow-mode
    standard-themes
    telephone-line
    terraform-mode
    transient
    treesit-grammars.with-all-grammars
    treesit-fold
    verb
    vertico
    wgrep
    yasnippet
    yasnippet-capf
    yasnippet-snippets
    zig-mode
  ];

  aspellEnglish = pkgs.aspellWithDicts (
    ds: with ds; [
      en
      en-computers
      en-science
    ]
  );

  dumpDict =
    dictName:
    builtins.readFile (
      pkgs.runCommand "aspell-${dictName}-dict.txt" { buildInputs = [ aspellEnglish ]; } ''
        ${aspellEnglish}/bin/aspell --lang=${dictName} dump master | sort -u > $out
      ''
    );
in
{
  config = lib.mkIf config.wolf.roles.editing {
    programs.emacs.extraPackages = epkgs: emacsExtraPackages ++ emacsExtraPackagesLocal;

    home.packages = with pkgs; [
      # Dictionaries for use with flyspell
      aspellEnglish

      pandoc

      emacs-lsp-booster
    ];

    home.file = {
      # Populate authinfo file.
      authinfo = {
        target = ".authinfo";
        text =
          if (lib.hasAttr "auth_tokens" secrets) then
            lib.concatStringsSep "\n" (
              map (token: ''
                machine ${token.machine} login ${token.login} password ${token.password}
              '') secrets.auth_tokens
            )
          else
            "";
      };

      ".config/emacs/var/tree-sitter".source =
        "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib";
    };
    xdg.configFile."emacs/dict.txt".text = (dumpDict "en");
  };
}
