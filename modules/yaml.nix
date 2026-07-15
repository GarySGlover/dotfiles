# [[file:../modules.org::*YAML][YAML:1]]
{
  flake.aspects.yaml = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          yq
          yaml-language-server
        ];
        programs.emacs.extraPackages =
          epkgs: with epkgs; [
            yaml
            yaml-pro
          ];
        editor.initFiles.yaml.text = ''
          (with-eval-after-load 'yaml-ts-mode
            (require 'reformatter)
            (require 'editorconfig)
            (require 'dtrt-indent)
            (add-hook 'yaml-ts-mode-hook #'eglot-ensure)
            (defun find-yamlfmt-config ()
              "Return the path to the nearest yamlfmt config file, or nil."
              (let ((base (or (locate-dominating-file default-directory ".yamlfmt")
                              (locate-dominating-file default-directory ".yamlfmt.yaml")
                              (locate-dominating-file default-directory ".yamlfmt.yml"))))
                (when base
                  (cl-loop for f in '(".yamlfmt" ".yamlfmt.yaml" ".yamlfmt.yml")
                           for path = (expand-file-name f base)
                           when (file-exists-p path) return path))))
            (defun yamlfmt-indent-from-config (config-file)
              "Return the indent specified in yamlfmt CONFIG-FILE, or nil."
              (when (and config-file (executable-find "yq"))
                (with-temp-buffer
                  (let ((exit (call-process
                               "yq" nil t nil "-r" "-e" ".formatter.indent" config-file)))
                    (when (eq exit 0)
                      (goto-char (point-min))
                      (when (re-search-forward "\\([0-9]+\\)" nil t)
                        (string-to-number (match-string 1))))))))
            (defun yaml-get-indent ()
              "Get the correct YAML indent width via config, editorconfig, or dtrt."
              (or (yamlfmt-indent-from-config (find-yamlfmt-config))
                  (dtrt-indent-get)
                  2)) ;; 2 as base default
            (defun yamlfmt-args ()
              "Return args for yamlfmt (nil if config sets indent, args if not)."
              (let* ((config (find-yamlfmt-config))
                     (indent-arg (unless (and config (yamlfmt-indent-from-config config))
                                   (list "--indent" (number-to-string (yaml-get-indent))))))
                (append indent-arg '("/dev/stdin"))))
            (reformatter-define yaml-format
              :program "yamlfmt"
              :args (yamlfmt-args))
            (defun yaml-mode-setup ()
              (let ((indent (yaml-get-indent)))
                (setq-local tab-width indent)
                (setq-local indent-line-function #'indent-relative)
                (setq-local indent-tabs-mode nil)
                (yaml-format-on-save-mode 1)))
            (add-hook 'yaml-ts-mode-hook #'yaml-mode-setup))
          ;; TODO Decide on scalar editing (yaml-ts-pro?)
        '';
      };
  };
}
# YAML:1 ends here
