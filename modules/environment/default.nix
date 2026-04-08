# [[file:../../modules.org::*Environment][Environment:2]]
{
  flake.aspects.environment.homeManager = {
    programs.emacs.extraPackages = epkgs: with epkgs; [ exec-path-from-shell ];
    editor.initFiles.environment.text = ''
      (when (daemonp)
        (require 'exec-path-from-shell)
        (dolist (var
                 '("DISPLAY"
                   "NIRI_SOCKET"
                   "WAYLAND_DISPLAY"
                   "XCURSOR_SIZE"
                   "XCURSOR_THEME"
                   "XDG_CONFIG_HOME"
                   "XDG_CURRENT_DESKTOP"
                   "XDG_SESSION_CLASS"
                   "XDG_SESSION_DESKTOP"
                   "XDG_SESSION_TYPE"))
          (add-to-list 'exec-path-from-shell-variables var))
        (exec-path-from-shell-initialize))
    '';
  };
}
# Environment:2 ends here
