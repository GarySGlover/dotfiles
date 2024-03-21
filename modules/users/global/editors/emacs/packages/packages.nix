{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
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

  nim-ts-mode = pkgs.callPackage ./manual/nim-ts-mode.nix {
    inherit (pkgs) fetchFromGitHub writeText;
    inherit (epkgs) melpaBuild nim-mode;
  };
in {
  config = mkIf config.wolf.roles.editing {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        ace-window
        avy
        beframe
        breadcrumb
        combobulate
        consult
        consult-flyspell
        corfu
        devdocs
        direnv
        doom-modeline
        dtrt-indent
        eat
        ef-themes
        elisp-autofmt
        embark
        embark-consult
        exec-path-from-shell
        format-all
        gcmh
        git-timemachine
        gptel
        helpful
        indent-bars
        json-mode
        json-navigator
        kubel
        magit
        marginalia
        markdown-mode
        mini-frame
        nerd-icons-completion
        nerd-icons-dired
        nim-ts-mode
        nix-ts-mode
        no-littering
        orderless
        rainbow-delimiters
        terraform-doc
        terraform-mode
        transient
        treesit-grammars.with-all-grammars
        verb
        vertico
        visual-fill-column
        which-key
        yaml-pro
      ];

    # Dictionaries for use with flyspell
    home.packages = with pkgs; [
      (aspellWithDicts (ds: with ds; [en en-computers en-science]))
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

    home.file.".config/emacs/var/tree-sitter".source = "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib";
  };
}
