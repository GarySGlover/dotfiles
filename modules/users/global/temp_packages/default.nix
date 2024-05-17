{pkgs, ...}: {
  config = {
    home.packages = with pkgs; [
      unzip
      jdk21
      zip
      glxinfo
      wlr-randr
      nwg-displays
      pandoc
    ];
    assertions = [
      {
        assertion = true;
        message = "Dude, you are still using temp packages";
      }
    ];
  };
}
