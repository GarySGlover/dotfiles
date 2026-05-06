# [[file:../../modules.org::*Programming][Programming:2]]
{
  flake.aspects =
    { aspects, ... }:
    {
      programming = {
        includes = with aspects; [
          direnv
          docker
          go
          yaml
        ];
        homeManager = {
          programs.emacs.extraPackages =
            epkgs: with epkgs; [
              reformatter
            ];
          editor.initFiles.programming.text = ''
            (with-eval-after-load 'whitespace
              (setopt whitespace-style
                      '(face
                        tabs
                        trailing
                        space-before-tab
                        newline
                        indentation
                        empty
                        space-after-tab
                        missing-newline-at-eof)))

            (with-eval-after-load 'indent-bars
              (setopt indent-bars-treesit-support t))

            (add-hook 'prog-mode-hook
                      (lambda ()
                        (display-line-numbers-mode t)
                        (indent-bars-mode t)
                        (whitespace-mode t)))

            (add-hook 'yaml-ts-mode-hook
                      (lambda ()
                        (display-line-numbers-mode t)
                        (indent-bars-mode t)
                        (whitespace-mode t)))

            (global-set-key [remap imenu] #'consult-imenu)

            ;; Rebind indentation to use mnemonic movements
            (let ((km indent-rigidly-map))
              (define-key km (kbd "<right>") nil)
              (define-key km (kbd "<left>") nil)
              (define-key km (kbd "TAB") nil)
              (define-key km (kbd "S-<left>") nil)
              (define-key km (kbd "S-<right>") nil)
              (define-key km (kbd "f") #'indent-rigidly-right)
              (define-key km (kbd "b") #'indent-rigidly-left)
              (define-key km (kbd "F") #'indent-rigidly-right-to-tab-stop)
              (define-key km (kbd "B") #'indent-rigidly-left-to-tab-stop))

            (defun dtrt-indent-get ()
              "Local init function to get the indentation using dtrt-indent lookups."
              (let ((indent-variable (caddr (dtrt-indent--search-hook-mapping major-mode))))
                (when indent-variable
                  (eval indent-variable))))
          '';
          fonts.fontconfig.enable = true;
        };
        nixos = { };
      };
    };
}
# Programming:2 ends here
