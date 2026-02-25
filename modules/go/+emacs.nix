# [[file:../../modules.org::*+emacs][+emacs:2]]
{
  flake.aspects = {
    options = {
      homeManager =
        { lib, ... }:
        {
          options.languages.go.enable = lib.mkEnableOption "Enable go language and standard tools";
        };
    };
    go.homeManager = {
      programs.emacs.extraPackages = epkgs: with epkgs; [ ob-go ];
      editor.initFiles.go.text = ''
        (with-eval-after-load 'org
          ;; format: off
          (defvar org-go-templates
                  '((go_ & "#+begin_src go :tangle " (p "no" tangle) " :noweb " (p "no" noweb) n> r> n "#+end_src"
                         :post (org-edit-special) :doc "Org src block for Go with tangle argument")))
          ;; format: on
          (add-hook
           'org-mode-hook
           (lambda ()
             (add-hook 'tempel-template-sources 'org-go-templates
                       nil
                       'local)))
          (add-to-list 'org-src-lang-modes '("go" . go-ts))
          (org-babel-do-load-languages
           'org-babel-load-languages
           (add-to-list 'org-babel-load-languages '(go . t))))
        (with-eval-after-load 'go-ts-mode
          ;; format: off
          (defvar go-templates
                  '((imp "import " q)
                    (impn "import (" n> q n ")")
                    (pr "fmt.Printf(\"\\n" p "\\n%#v\\n\", " q ")")
                    (pl "fmt.Println(" q ")")
                    (db "Debug.Printf(\"\\n" p "\\n\\n%#v\\n\", " q ")")
                    (dl "Debug.Println(" q ")")
                    (lf "log.Printf(\"\\n%#v\\n\", " q ")")
                    (ln "log.Println(" q ")")
                    (stt "type " p " struct {" n> q n "}")
                    (inf "type " p " interface {" n> q n "}")
                    (cnt "const " p " = " q)
                    (cnst "const (" n> p " = " q n ")")
                    (vr "var " p " " q)
                    (mp "map[" p "]" q)
                    (if "if "
                        p
                        " {"
                        n>
                        p
                        n
                        "}"
                        >
                        q)
                    (el "if " p " {" n> p n "} else {" > n> p n "}" > q)
                    (elif "if " p " {" n> p n "} else if " > p " {" n> p n "}" > q)
                    (ifen "if err != nil {" n> q n "}" >)
                    (ifer "if err != " p " {" n> q n "}" >)
                    (sel "select {" n> "case " p ":" n> q n "}" >)
                    (swch "switch " p " {" n> "case " p ":" q n "}" >)
                    (fr "for " p "{" n> q n "}" >)
                    (rng "for " p ", " p " := range " p " {" n> q n "}" >)
                    (fnc "func " p "(" p ") {" n> q n "}" >)
                    (mn "func main() {" n> q n "}")
                    (in "func init() {" n> q n "}")
                    (tst "func Test" p " (t *testing.T) { " n> q n "}")))
          ;; format: on
          (add-hook
           'go-ts-mode-hook
           (lambda ()
             (add-hook 'tempel-template-sources 'go-templates nil 'local))))
      '';
    };
  };
}
# +emacs:2 ends here
