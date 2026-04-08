# [[file:../modules.org::*Docker][Docker:2]]
{
  flake.aspects.docker.homeManager = {
    editor.initFiles.docker.text = ''
      (with-eval-after-load 'org
        ;; format: off
        (defvar org-docker-templates
                '((dockerfile_ & "#+begin_src dockerfile :tangle " (p "no" tangle) n> r> n "#+end_src"
                               :post (org-edit-special))))
        ;; format: on
        (add-hook
         'org-mode-hook
         (lambda ()
           (add-hook 'tempel-template-sources 'org-docker-templates nil 'local)))
        (add-to-list
         'org-src-lang-modes '("dockerfile" . dockerfile-ts)))
    '';
  };
}
# Docker:2 ends here
