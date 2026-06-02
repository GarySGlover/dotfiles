# [[file:../modules.org::*Xreal one pro update][Xreal one pro update:1]]
{
  flake.aspects.entertainment.nixos = {
    # https://github.com/ianepreston/nixos/blob/979c358a70f64985dd122364588651981213e132/modules/hardware/xreal-headset.nix#L9
    services.udev.extraRules = ''
      SUBSYSTEM=="usb", ATTR{idVendor}=="3318", MODE="0666", TAG+="uaccess"
      KERNEL=="hidraw*", ATTRS{idVendor}=="3318", MODE="0666", TAG+="uaccess"
      KERNEL=="ttyUSB[0-9]*", ATTRS{idVendor}=="3318", MODE="0666", TAG+="uaccess"
      KERNEL=="ttyACM[0-9]*", ATTRS{idVendor}=="3318", MODE="0666", TAG+="uaccess"
    '';
  };
}
# Xreal one pro update:1 ends here
