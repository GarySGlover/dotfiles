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
                        spaces
                        trailing
                        lines
                        space-before-tab
                        newline indentation
                        empty space-after-tab
                        newline-mark
                        missing-newline-at-eof
                        )))

            (add-hook 'prog-mode-hook
                      (lambda ()
                        (display-line-numbers-mode t)
                        (indent-bars-mode t)
                        (whitespace-mode t)))
          '';
        };
        nixos = { };
      };
    };
}
# Programming:2 ends here
