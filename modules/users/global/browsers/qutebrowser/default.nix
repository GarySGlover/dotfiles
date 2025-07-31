{ pkgs, inputs, ... }:
let
  userscripts = "${qutebrowser-src}/misc/userscripts";
  qutebrowser-src = inputs.qutebrowser-src;
  qutebrowserUserscriptsDirectory = ".local/share/qutebrowser/userscripts";

  qute-bitwarden = pkgs.writers.writePython3Bin "qute-bitwarden" {
    flakeIgnore = [
      "E265"
      "E402"
      "W503"
      "E501"
    ];
    libraries = with pkgs.python3.pkgs; [
      tldextract
    ];
  } (builtins.readFile "${userscripts}/qute-bitwarden");

in
{
  programs.qutebrowser = {
    enable = true;
    keyBindings = {
      normal = {
        "zb" = "spawn --userscript ${qute-bitwarden}/bin/qute-bitwarden";
      };
    };
    settings = {
      "qt.args" = [ "disable-features=PermissionElement" ];
    };
  };
  home.packages = with pkgs; [
    bitwarden-cli
    keyutils
    rofi
  ];
  # home.file."${qutebrowserUserscriptsDirectory}/qute-bitwarden".source =
  #   "${userscripts}/qute-bitwarden";
}

# This might be a better reference for scripts. Can run with their own python version and packages
# https://github.com/noib3/dotfiles/blob/6a72ec7023376c683bd5a529c504cfd51558b603/configs/qutebrowser/default.nix#L15

# Didn't work
# Maybe a simpler way
# https://github.com/tye-exe/nixos-config/blob/b20b4cd15ba93a77de1604b42a08c70664cd472f/home/preset/qutebrowser.nix#L9
