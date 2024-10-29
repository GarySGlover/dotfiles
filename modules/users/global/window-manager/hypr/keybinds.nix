{
  config,
  lib,
  ...
}:
with lib;
{
  config = mkIf config.wolf.roles.desktop {
    wayland.windowManager.hyprland = {
      settings = {
        exec-once = [ "waybar" ];
      };
    };
  };
}
