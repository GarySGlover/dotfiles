{...}: {
  wolf = {
    editors.emacs.enable = true;
    languages = {
      nix.enable = true;
      python.enable = true;
    };
    git.precommit.enable = true;
  };
}
