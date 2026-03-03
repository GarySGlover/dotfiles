# [[file:../../modules.org::*+emacs][+emacs:2]]
{
  flake.aspects.yaml.homeManager = {
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        yaml
        yaml-pro
      ];
    editor.initFiles.yaml.text = ''

    '';
  };
}
# +emacs:2 ends here
