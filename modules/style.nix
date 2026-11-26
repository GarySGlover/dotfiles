# [[file:../modules.org::*Style][Style:2]]
{
  inputs,
  ...
}:
{
  flake.aspects = {
    style = {
      homeManager =
        {
          pkgs,
          config,
          lib,
          ...
        }:
        {
          imports = [ inputs.stylix.homeModules.stylix ];
          config = {
            gtk.gtk4.theme = lib.mkOverride 999 null;
            home.packages = builtins.filter pkgs.lib.isDerivation (builtins.attrValues pkgs.nerd-fonts);
            niri.configFiles.style.text = ''
              layout {
                background-color "${config.lib.stylix.colors.withHashtag.base00}"
                border {
                  active-color "${config.lib.stylix.colors.withHashtag.base01}"
                  inactive-color "${config.lib.stylix.colors.withHashtag.base01}"
                  urgent-color "${config.lib.stylix.colors.withHashtag.base01}"
                }
              }
            '';
            stylix = {
              enable = true;
              # `useGlobalPkgs = true` on the NixOS home-manager module means
              # home-manager can't set its own `nixpkgs.overlays`/`nixpkgs.config`
              # (it must inherit the system-level pkgs). Stylix's per-app
              # targets (e.g. nixos-icons, gtksourceview) are implemented as
              # overlays applied via home-manager by default, which trips the
              # home-manager assertion. Disable that here; styling still
              # applies to everything that doesn't rely on package overlays.
              overlays.enable = false;
              # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
              base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-macchiato.yaml";
              fonts = {
                serif = config.stylix.fonts.sansSerif;
                sansSerif = {
                  package = pkgs.nerd-fonts.recursive-mono;
                  name = "RecMonoCasual Nerd Font";
                };

                monospace = {
                  package = pkgs.nerd-fonts.iosevka-term;
                  name = "IosevkaTerm Nerd Font";
                };
                emoji = config.stylix.fonts.monospace;
                sizes = {
                  desktop = 12;
                  applications = 12;
                };
              };
              targets.firefox.profileNames = [
                "home"
                "work"
              ];
            };
          };
        };
    };
  };
}
# Style:2 ends here
