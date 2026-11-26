# [[file:../modules.org::*Terminal][Terminal:1]]
{
  flake.aspects = {
    terminal.homeManager =
      {
        pkgs,
        inputs,
        ...
      }:
      {
        programs.btop = {
          enable = true;
          settings = {
            force_tty = true;
          };
        };
        home.packages = with pkgs; [
          (inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.az-tui)
          lm_sensors
          radeontop
        ];
      };
  };
  perSystem =
    { pkgs, ... }:
    {
      packages.az-tui = pkgs.buildGoModule (finalAttrs: {
        pname = "az-tui";
        version = "0.4.0";

        src = pkgs.fetchFromGitHub {
          owner = "IAL32";
          repo = "az-tui";
          tag = "v${finalAttrs.version}";
          hash = "sha256-yWnduwyEwlRkU/aBoLWbqFVd3J/PvQV1XkBDAeX3/+s=";
        };

        vendorHash = "sha256-TkabA+dH00r00m4Ud/MCi5g75VTArl5fNSAasKRfceA=";
      });
    };
}
# Terminal:1 ends here
