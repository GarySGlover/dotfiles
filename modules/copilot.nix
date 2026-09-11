# [[file:../modules.org::*Copilot][Copilot:2]]
{
  flake.aspects.editor.homeManager = {
    programs.emacs.extraPackages =
      epkgs: with epkgs; [
        copilot
      ];
    editor.initFiles.copilot.text = ''
      (with-eval-after-load 'copilot
        (setopt copilot-lsp-settings '(:github-enterprise (:uri "https://next-plc.ghe.com"))))
    '';
  };
}
# Copilot:2 ends here
