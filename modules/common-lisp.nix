# [[file:../modules.org::*Common Lisp][Common Lisp:1]]
{
  flake.aspects.common-lisp = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
        ];
        programs.emacs.extraPackages =
          epkgs: with epkgs; [
            sly
          ];
        editor.initFiles.common-lisp.text = ''
          (with-eval-after-load 'sly
            (org-babel-do-load-languages
             'org-babel-load-languages
             (add-to-list 'org-babel-load-languages '(lisp . t)))
            (setopt
             inferior-lisp-program "sbcl"
             org-babel-lisp-eval-fn 'sly-eval))
        '';
      };
    nixos = { };
  };
}
# Common Lisp:1 ends here
