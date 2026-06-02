# [[file:../modules.org::*Style][Style:3]]
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
              (add-to-list 'default-frame-alist '(font . "IosevkaTerm Nerd Font-14"))
              (add-hook 'after-init-hook (lambda ()
                                           (setq-default truncate-lines t)
                                           (custom-set-faces
                                            '(variable-pitch ((t (:family "RecMonoCasual Nerd Font" :height 140))))
                                            '(font-lock-comment-face ((t (:inherit error :foreground unspecified))))
                                            '(mode-line ((t (:background unspecified))))
                                            '(mode-line-inactive ((t (:background unspecified))))
                                            '(fringe ((t (:background unspecified))))
                                            '(line-number ((t (:background unspecified)))))))

              (add-hook 'text-mode-hook
                        (lambda ()
                          (buffer-face-set '(:family "RecMonoCasual Nerd Font" :height 140))))

              (add-hook 'yaml-mode-hook
                        (lambda ()
                          (buffer-face-set '(:family "IosevkaTerm Nerd Font" :height 140))))

              (add-hook 'org-mode-hook
                        (lambda ()
                          (custom-set-faces
                           '(org-block ((t (:family "IosevkaTerm Nerd Font" :height 140)))))))
            '';
            niri.configFiles.style.text = ''
              layout {
                background-color "${config.lib.stylix.colors.withHashtag.base00}"
                border {
                  active-color "${config.lib.stylix.colors.withHashtag.base01}"
                  inactive-color "${config.lib.stylix.colors.withHashtag.base01}"
                  urgent-color "${config.lib.stylix.colors.withHashtag.base01}"
                }
              }
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
# Style:3 ends here
