{lib, ...}:
with lib; {
  config = {
    programs.fish = {
      enable = true;
      shellAliases = {
        nfc = "~/dotfiles/scripts/nix-flake-check.sh";
        nfhr = "~/dotfiles/scripts/nix-flake-home-build.sh";
        nfht = "~/dotfiles/scripts/nix-flake-home-test-build.sh";
        nft = "~/dotfiles/scripts/nix-flake-test-build.sh";
        nfr = "~/dotfiles/scripts/nix-flake-rebuild.sh";
        nfu = "~/dotfiles/scripts/nix-flake-update.sh";
      };
    };
  };
}
