{
  lib,
  config,
  pkgs,
  ...
}:
let
  secrets = import ../../../../../secrets/${config.home.username}-secrets.nix;

  aspellEnglish = pkgs.aspellWithDicts (
    ds: with ds; [
      en
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
    home.packages = with pkgs; [
      # Dictionaries for use with flyspell
      aspellEnglish

      pandoc

      emacs-lsp-booster

      # Emacs everywhere
      wl-clipboard
      wtype

      # NOTE: no `azure-cli` here on purpose. The azure aspect
      # (`modules/azure.nix`) installs a single wrapped `azure-cli` with all the
      # extensions; adding a second, differently-configured one here makes
      # `buildEnv` fail on conflicting Python `site-packages`/`__pycache__` paths.
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
