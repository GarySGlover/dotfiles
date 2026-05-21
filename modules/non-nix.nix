# [[file:../modules.org::*Non-nix][Non-nix:4]]
{
  flake.aspects = {
    nonNix.homeManager =
      { pkgs, ... }:
      {
        nix = {
          package = pkgs.nix;
          extraOptions = ''
            experimental-features = nix-command flakes
          '';
        };
        home.packages = with pkgs; [
          nixgl.nixGLIntel
          niri
          xdg-desktop-portal
          xdg-desktop-portal-gtk
        ];
        editor.initFiles.non-nix = {
          text = ''
            (when (daemonp)
              (require 'exec-path-from-shell)
              (dolist (var
                       '("GIT_LOCATION"))
                (add-to-list 'exec-path-from-shell-variables var))
              (exec-path-from-shell-initialize))
          '';
          priority = 1010;
        };
        niri.configFiles.non-nix = {
          priority = 1010;
          text = ''
            environment {
              GIT_LOCATION "work"
            }
            // Export Wayland display and desktop session to D-Bus
            spawn-at-startup "dbus-update-activation-environment" "--systemd" "WAYLAND_DISPLAY" "XDG_CURRENT_DESKTOP=niri"
            // Restart portal-gnome after niri's ScreenCast D-Bus interface is ready
            spawn-at-startup "sh" "-c" "while ! busctl --user status org.gnome.Mutter.ScreenCast >/dev/null 2>&1; do sleep 0.2; done;"

            spawn-at-startup "systemctl" "--user" "restart" "emacs"
          '';
        };
        programs.bash.bashrcExtra = ''
          export GIT_LOCATION=work
        '';
        xdg.configFile."xdg-desktop-portal.portals.conf".text = ''
          [preferred]
          default=gtk
          org.freedesktop.impl.portal.ScreenCast=gnome
          org.freedesktop.impl.portal.Screenshot=gnome
        '';
      };
  };
}
# Non-nix:4 ends here
