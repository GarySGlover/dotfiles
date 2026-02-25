# [[file:../../modules.org::*+emacs][+emacs:2]]
{
  flake.aspects = {
    notes.homeManager = {
      editor.initFiles.notes.text = ''
        (with-eval-after-load 'org
          ;; format: off
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
          ;;format: on
          (add-hook
           'org-mode-hook
           (lambda ()
             (add-hook 'tempel-template-sources 'org-templates nil 'local))))
      '';
    };
  };
}
# +emacs:2 ends here
