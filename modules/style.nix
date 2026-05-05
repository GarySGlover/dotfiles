# [[file:../modules.org::*Style][Style:2]]
{
  inputs,
  ...
}:
{
  flake.aspects = {
    style = {
      homeManager =
        { pkgs, config, ... }:
        {
          imports = [ inputs.stylix.homeModules.stylix ];
          config = {
            programs.emacs.extraPackages = epkgs: with epkgs; [ show-font ];
            gtk.gtk4.theme = null;
            home.packages = builtins.filter pkgs.lib.isDerivation (builtins.attrValues pkgs.nerd-fonts);
            editor.initFiles.style.text = ''
              (add-to-list 'default-frame-alist '(font . "RecMonoCasual Nerd Font-12"))
              (add-hook 'after-init-hook (lambda ()
                                           (custom-set-faces
                                            '(variable-pitch ((t (:family "RecMonoCasual Nerd Font" :height 120))))
                                            '(font-lock-comment-face ((t (:inherit error :foreground nil))))
                                            )))

              (add-hook 'prog-mode-hook
                        (lambda ()
                          (buffer-face-set '(:family "IosevkaTerm Nerd Font" :height 140))))

              (add-hook 'org-mode-hook
                        (lambda ()
                          (custom-set-faces
                           '(org-block ((t (:family "IosevkaTerm Nerd Font" :height 140)))))))
            '';
            stylix = {
              enable = true;
              # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
              base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-macchiato.yaml";
              fonts = {
                serif = config.stylix.fonts.sansSerif;
                sansSerif = {
                  package = pkgs.nerd-fonts.recursive-mono;
                  name = "RecMonoCasual Nerd Font";
                };

                monospace = {
                  package = pkgs.nerd-fonts.iosevka-term;
                  name = "IosevkaTerm Nerd Font";
                };
                emoji = config.stylix.fonts.monospace;
                sizes = {
                  desktop = 12;
                  applications = 12;
                };
              };
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
