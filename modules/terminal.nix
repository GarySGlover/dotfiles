# [[file:../modules.org::*Terminal][Terminal:2]]
{
  flake.aspects.terminal.homeManager = {
    programs.emacs.extraPackages = epkgs: with epkgs; [ ghostel ];
    editor.initFiles.terminal.text = ''
      (with-eval-after-load 'ghostel
        (with-eval-after-load 'project
          (bind-key "t" #'ghostel-project 'project-prefix-map)
          (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)))
      (add-hook 'after-init-hook
                (lambda ()
                  (require 'ghostel)
                  (ghostel-compile-global-mode t)))
    '';
  };
}
# Terminal:2 ends here
