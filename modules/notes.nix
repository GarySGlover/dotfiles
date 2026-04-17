# [[file:../modules.org::nix_notes][nix_notes]]
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
        (add-hook 'dired-mode #'denote-dired-mode)
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "n") #'denote)
          (define-key map (kbd "r") #'denote-rename-file)
          (define-key map (kbd "l") #'denote-link-or-create)
          (define-key map (kbd "b") #'denote-backlinks)
          (define-key map (kbd "d") #'denote-dired)
          (define-key map (kbd "g") #'denote-grep)
          (global-set-key (kbd "C-c n") map))
        (with-eval-after-load 'denote
          (setopt denote-directory (expand-file-name "~/nook/notes/"))
          (denote-rename-buffer-mode 1))
      '';
      programs.emacs.extraPackages =
        epkgs: with epkgs; [
          casual
          denote
        ];
    };
  };
}
# nix_notes ends here
