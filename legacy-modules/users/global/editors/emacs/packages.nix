{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
let
  secrets = import ../../../../../secrets/${config.home.username}-secrets.nix;

  epkgsl = with pkgs.emacsPackages; {
    magit-worktrees = pkgs.emacsPackages.trivialBuild rec {
      pname = "magit-worktrees";
      version = "1.0";
      src = ./emacs-new/magit-worktrees.el;
      propagatedUserEnvPkgs = [
        magit
        dash
      ];
      buildInputs = propagatedUserEnvPkgs;
      nativeBuildInputs = with pkgs; [ git ];
    };
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
    gptel-quick = (
      melpaBuild {
        pname = "gptel-quick";
        version = "1";
        commit = "1";
        src = inputs.gptel-quick;
        packageRequires = [
          gptel
        ];
        recipe = pkgs.writeText "recipe" ''
          (gptel-quick
            :repo "karthink/gptel-quick"
            :fetcher github
            :files ("*.el"))
        '';
      }
    );
    local-packages = trivialBuild rec {
      pname = "local-packages";
      version = "1.0";
      src = ./local-packages;
      propagatedUserEnvPkgs = [ consult ];
      buildInputs = propagatedUserEnvPkgs;
      nativeBuildInputs = with pkgs; [ ];
      postBuild = ''
        emacs -L . --batch -l package --eval '(package-generate-autoloads "local-packages" ".")'
      '';
    };

  };

  emacsExtraPackagesLocal = with epkgsl; [
    eglot-booster
    gptel-quick
    local-packages
    magit-worktrees
    transient-compile
    ws-butler
    kbd-mode
  ];

  emacsExtraPackages =
    with pkgs.emacsPackages;
    [
      uuidgen
      kdl-mode
      avy
      beframe
      benchmark-init
      cape
      consult
      consult-yasnippet
      corfu
      corfu-candidate-overlay
      coterm
      denote
      dtrt-indent
      emacs-everywhere
      embark
      embark-consult
      git-auto-commit-mode
      git-timemachine
      gptel
      gptel-agent
      helpful
      indent-bars
      kele
      magit
      marginalia
      markdown-mode
      mcp
      meow
      ob-async
      orderless
      org
      org-auto-tangle
      org-present
      popper
      posframe
      rainbow-delimiters
      rainbow-mode
      standard-themes
      tempel
      transducers
      transient
      treesit-fold
      treesit-grammars.with-all-grammars
      vertico
      wgrep
    ]
    ++ [
      # Language modes
      nix-ts-mode
      terraform-doc
      terraform-mode
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
  config = {
    programs.emacs.extraPackages = _: emacsExtraPackages ++ emacsExtraPackagesLocal;

    home.packages = with pkgs; [
      # Dictionaries for use with flyspell
      aspellEnglish

      pandoc

      emacs-lsp-booster

      # MCP Servers
      # mcp-server-git
      # mcp-server-azuredevops

      # Emacs everywhere
      wl-clipboard
      wtype
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
