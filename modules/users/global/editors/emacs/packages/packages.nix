{
  lib,
  config,
  pkgs,
  inputs,
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
        src = inputs.kbd-mode;
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
        src = inputs.transient-compile;
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
        src = inputs.eglot-booster;
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
        src = inputs.ws-butler;
        packageRequires = [ ];
        recipe = pkgs.writeText "recipe" ''
          (ws-butler
            :repo "lewang/ws-butler"
            :fetcher github
            :files ("*.el"))
        '';
      }
    );
    org-menu = (
      melpaBuild {
        pname = "org-menu";
        version = "1";
        commit = "1";
        src = inputs.org-menu;
        packageRequires = [ ];
        recipe = pkgs.writeText "recipe" ''
          (org-menu
            :repo "sheijk/org-menu"
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
    org-menu
  ];

  emacsExtraPackages = with pkgs.emacsPackages; [
    ace-window
    avy
    breadcrumb
    beframe
    cape
    consult
    consult-yasnippet
    copilot
    corfu
    corfu-candidate-overlay
    coterm
    denote
    disproject
    dtrt-indent
    eat
    editorconfig
    ef-themes
    elisp-autofmt
    embark
    embark-consult
    envrc
    format-all
    general
    git-timemachine
    gptel
    helpful
    hyperbole
    indent-bars
    keycast
    magit
    marginalia
    markdown-mode
    nix-ts-mode
    ob-async
    orderless
    org
    org-auto-tangle
    ox-pandoc
    rainbow-mode
    standard-themes
    telephone-line
    terraform-doc
    terraform-mode
    transient
    treesit-fold
    treesit-grammars.with-all-grammars
    verb
    vertico
    wgrep
    yaml
    yaml-pro
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
