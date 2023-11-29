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
    tools = {
      azure.enable = false;
      kubernetes.enable = true;
      qmk.enable = true;
    };
    user.interactive = true;
    window-manager.ags.enable = true;
  };
}
