# [[file:../modules.org::*GUI][GUI:1]]
{
  flake.aspects =
    { aspects, ... }:
    {
      gui = {
        includes = with aspects; [
          audio
          video
        ];
        nixos = {
          services = {
            xserver = {
              enable = true;
              xkb.layout = "gb";
              videoDrivers = [ "modesetting" ];
            };
            displayManager.ly.enable = true;
            physlock = {
              enable = true;
              allowAnyUser = true;
            };
          };
        };
      };
    };
}
# GUI:1 ends here
