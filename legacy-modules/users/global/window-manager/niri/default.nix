{
  pkgs,
  lib,
  ...
}:
with lib;
{
  config = {
    home.packages = with pkgs; [
      (wl-kbptr.overrideAttrs (_: {
        src = fetchFromGitHub {
          owner = "moverest";
          repo = "wl-kbptr";
          rev = "1c6c9275a49f6def4c37707e741da47f5098be7c";
          sha256 = "sha256-UEVPeqD1Oj3cK2Hq2eLpGy6Jdjd9i0tQNXdiDWAUIM0=";
        };
        version = "unstable-local";
      }))
    ];

    niri.configFiles.legacy = {
      priority = 1;
      text = (builtins.readFile ./legacy.kdl);

    };
  };
}
