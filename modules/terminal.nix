# [[file:../modules.org::*Terminal][Terminal:2]]
{
  flake.aspects = {
    terminal.homeManager =
      { pkgs, inputs, ... }:
      {
        programs.btop = {
          enable = true;
          settings = {
            force_tty = true;
          };
        };
        home.packages = [
          (inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.az-tui)
          lm_sensors
        ];
        programs.emacs.extraPackages = epkgs: with epkgs; [ ghostel ];
        editor.initFiles.terminal.text = ''
          (defmacro ghostel-make-exec (app)
            "Define ghostel-exec-APP function to run APP in a new buffer."
            (let ((fname (intern (format "ghostel-exec-%s" app)))
                  (bname (format "*%s*" app))
                  (appstr (if (symbolp app) (symbol-name app) app)))
              `(defun ,fname ()
                 (interactive)
                 (require 'ghostel)
                 (when (fboundp 'ghostel-exec)
                   (with-current-buffer (generate-new-buffer ,bname)
                     (ghostel-exec (current-buffer) ,appstr)
                     (switch-to-buffer (current-buffer)))))))

          (add-hook 'after-init-hook
                    (lambda () (ghostel-make-exec btop)))

          (with-eval-after-load 'ghostel
            (with-eval-after-load 'project
              (bind-key "t" #'ghostel-project 'project-prefix-map)
              (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)))
          (add-hook 'after-init-hook
                    (lambda ()
                      (require 'ghostel)
                      (ghostel-compile-global-mode t)))
        '';
      };
  };
  perSystem =
    { pkgs, ... }:
    {
      packages.az-tui = pkgs.buildGoModule (finalAttrs: {
        pname = "az-tui";
        version = "0.4.0";

        src = pkgs.fetchFromGitHub {
          owner = "IAL32";
          repo = "az-tui";
          tag = "v${finalAttrs.version}";
          hash = "sha256-yWnduwyEwlRkU/aBoLWbqFVd3J/PvQV1XkBDAeX3/+s=";
        };

        vendorHash = "sha256-TkabA+dH00r00m4Ud/MCi5g75VTArl5fNSAasKRfceA=";
      });
    };
}
# Terminal:2 ends here
