let
  shellAliases = {
    nfc = "~/dotfiles/scripts/nix-flake-check.sh";
    nfhr = "~/dotfiles/scripts/nix-flake-home-build.sh";
    nfht = "~/dotfiles/scripts/nix-flake-home-test-build.sh";
    nft = "~/dotfiles/scripts/nix-flake-test-build.sh";
    nfr = "~/dotfiles/scripts/nix-flake-rebuild.sh";
    nfu = "~/dotfiles/scripts/nix-flake-update.sh";
    ed = "ps aux | grep -ie emacs | grep -v grep | grep -v emacsclient | awk '{print $2}' | xargs kill -SIGUSR2";
    cde = "cd $(e)";
  };

in
{
  config = {
    programs.bash = {
      enable = true;
      inherit shellAliases;
      bashrcExtra = ''
        if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi
        export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
      '';
    };
    programs.fish = {
      enable = true;
      inherit shellAliases;
    };
  };
}
