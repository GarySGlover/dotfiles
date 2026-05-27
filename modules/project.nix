# [[file:../modules.org::*Project][Project:1]]
{
  flake.aspects = {
    programming = {
      homeManager = {
        programs.git.signing.format = null;
        editor.initFiles.project.text = ''
          (with-eval-after-load 'magit
            (magit-add-section-hook 'magit-status-sections-hook
                                    'magit-insert-worktrees nil t)
            (magit-add-section-hook 'magit-status-sections-hook
                                    'magit-insert-modules nil t)
            (magit-add-section-hook 'magit-status-sections-hook
                                    'magit-insert-local-branches nil t)
            (magit-add-section-hook 'magit-status-sections-hook
                                    'magit-insert-remote-branches nil t))
        '';
      };
      nixos = { };
    };
  };
}
# Project:1 ends here
