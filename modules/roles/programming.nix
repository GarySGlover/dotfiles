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
        homeManager =
          { pkgs, ... }:
          {
            home.packages = with pkgs; [
              nerd-fonts.iosevka
              nerd-fonts.iosevka-term
              nerd-fonts.iosevka-term-slab
            ];
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
                          space-before-tab
                          newline
                          indentation
                          empty
                          space-after-tab
                          missing-newline-at-eof)))

              (add-hook 'prog-mode-hook
                        (lambda ()
                          (display-line-numbers-mode t)
                          (indent-bars-mode t)
                          (whitespace-mode t)))
            '';
            fonts.fontconfig.enable = true;
          };
        nixos = { };
      };
    };
}
# Programming:2 ends here
