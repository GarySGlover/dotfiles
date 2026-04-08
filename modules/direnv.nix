# [[file:../modules.org::*Direnv][Direnv:2]]
{
  flake.aspects.direnv.homeManager = {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    programs.starship.settings.direnv.disabled = false;

    home.sessionVariables = {
      DIRENV_LOG_FORMAT = "";
    };

    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        envrc
      ];
    editor.initFiles.direnv.text = ''
      (add-hook 'after-init-hook (lambda () (envrc-global-mode t)) 91)
      (with-eval-after-load 'envrc
        (setopt envrc-show-summary-in-minibuffer nil)
        (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
      (add-to-list
       'display-buffer-alist
       `(,(rx "*envrc*")
         (display-buffer-no-window)
         (allow-no-window . t)))
    '';
  };
}
# Direnv:2 ends here
