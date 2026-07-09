# [[file:../../modules.org::*Mode/Header line][Mode/Header line:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.modeline.text = ''
      (setq-default mode-line-format
                    '("%e"
                      mode-line-front-space
                      mode-line-buffer-identification
                      (mode-line-process mode-line-process)
                      (vc-mode vc-mode)
                      (:eval (envrc--lighter))
                      (flymake-mode flymake-mode-line-format)
                      mode-line-format-right-align
                      (display-time-mode display-time-string)
                      mode-line-end-spaces))
      (setopt
       display-time-format "%H:%M"
       display-time-default-load-average nil)
      (display-time-mode)
    '';
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        breadcrumb
      ];
  };
}
# Mode/Header line:2 ends here
