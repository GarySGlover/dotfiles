{...}: {
  wolf = {
    browsers.nyxt.enable = true;
    editors.emacs.enable = true;
    git.precommit.enable = true;
    languages = {
      json.enable = true;
      nix.enable = true;
      python.enable = true;
    };
    terminal.kitty.enable = true;
    tools.azure.enable = false;
    user.interactive = true;
    window-manager.ags.enable = true;
  };
}
