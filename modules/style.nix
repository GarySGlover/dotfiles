# [[file:../modules.org::*Style][Style:2]]
{
  inputs,
  ...
}:
{
  flake.aspects = {
    style = {
      homeManager =
        { pkgs, ... }:
        {
          imports = [ inputs.stylix.homeModules.stylix ];
          config = {
            gtk.gtk4.theme = null;
            home.packages = with pkgs; [
              nerd-fonts.monofur
              nerd-fonts.iosevka
              nerd-fonts.iosevka-term
              nerd-fonts.iosevka-term-slab
            ];
            editor.initFiles.style.text = ''
              (add-to-list 'default-frame-alist '(font . "Monofur Nerd Font-14"))
              (set-face-attribute 'variable-pitch nil :family "Monofur Nerd Font" :height 140)

              (add-hook 'prog-mode-hook
                        (lambda ()
                          (buffer-face-set '(:family "IosevkaTermSlab Nerd Font" :height 140))))

              (add-hook 'org-mode-hook
                        (lambda ()
                          (custom-set-faces
                           '(org-block ((t (:family "IosevkaTermSlab Nerd Font" :height 140)))))))
            '';
            stylix = {
              enable = true;
              base16Scheme = "${pkgs.base16-schemes}/share/themes/one-light.yaml";
              targets.firefox.profileNames = [
                "home"
                "work"
              ];
            };
          };
        };
    };
  };
}
# Style:2 ends here
