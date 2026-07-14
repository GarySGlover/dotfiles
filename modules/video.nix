# [[file:../modules.org::*Video][Video:1]]
{ lib, ... }:
{
  flake.aspects = {
    video = {
      homeManager = { pkgs, ... }: {
        home.packages = with pkgs; [
          obs-studio
          shotcut
          haruna
        ];
      };
      nixos =
        { pkgs, ... }:
        let
          v4lctl = lib.getExe' pkgs.v4l-utils "v4l2-ctl";
        in
        {
          # Fix webcam frame rate
          services.udev.extraRules = ''
            SUBSYSTEM=="video4linux", KERNEL=="video[0-9]*", ATTR{name}=="*camera*", RUN+="${v4lctl} --device=/dev/%k --set-ctrl=power_line_frequency=1"
            SUBSYSTEM=="video4linux", KERNEL=="video[0-9]*", ATTR{name}=="*webcam*", RUN+="${v4lctl} --device=/dev/%k --set-ctrl=power_line_frequency=1"
            SUBSYSTEM=="video4linux", KERNEL=="video[0-9]*", RUN+="${v4lctl} --device=/dev/%k --set-ctrl=power_line_frequency=1"
          '';
        };
    };
  };
}
# Video:1 ends here
