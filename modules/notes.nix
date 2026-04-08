# [[file:../modules.org::nix_org_mode][nix_org_mode]]
{
  flake.aspects = {
    notes.homeManager = {
      editor.initFiles.notes.text = ''
        (with-eval-after-load 'org

          ;; Org template definitions with tempel
          (defvar org-templates
            '((ttl & "#+title: " p n "#+author: " p n "#+language: " p n n)
              (nm & "#+name: " p n)
              (lnk & "[[" p "][" p "]]")
              (bgn & "#+begin_" (s name) n r n "#+end_" name)
              (exmpl & "#+begin_example" n> r> n> "#+end_example")
              (cntr & "#+begin_center" n> r> n> "#+end_center")
              (cmm & "#+begin_comment" n> r> n> "#+end_comment")
              (mrginnte & "#+begin_marginnote" n> r> n "#+end_marginnote")
              (qt & "#+begin_quote" n> r> n> "#+end_quote")
              (sdnt & "#+begin_sidenote" n> r> n "#+end_sidenote")
              (src & "#+begin_src " p n r n "#+end_src"
                   :post (org-edit-src-code))
              (elsp & "#+begin_src emacs-lisp" n r n "#+end_src"
                    :post (org-edit-src-code))
              (vrs & "#+begin_verse" n> r> n> "#+end_verse")
              (rdnly ":tangle yes :tangle-mode (identity #o444) :mkdirp yes" n)
              (cptn & "#+caption: ")
              (drwr & ":" p ":" n r ":end:")
              (inlsrc "src_" p "{" q "}")))
          (add-hook 'org-mode-hook (lambda () (add-hook 'tempel-template-sources 'org-templates nil 'local)))

          ;; Whitespace fix for tangled files. Required for noweb blocks with empty lines.
          (add-hook 'org-babel-post-tangle-hook #'whitespace-cleanup)
          (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)

          ;; Org source code black display rules
          (setopt org-src-window-setup 'plain)

          (keymap-set org-mode-map "M-o" #'casual-org-tmenu)
          (keymap-set org-table-fedit-map "M-o" #'casual-org-table-fedit-tmenu))
      '';
      programs.emacs.extraPackages =
        epkgs: with epkgs; [
          casual
        ];
    };
  };
}
# nix_org_mode ends here
