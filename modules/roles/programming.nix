# [[file:../../modules.org::*Programming][Programming:1]]
{
  flake.aspects =
    { aspects, ... }:
    {
      programming = {
        includes = with aspects; [
          direnv
          go
          yaml
          common-lisp
          guix
        ];
        homeManager = {
          programs.emacs.extraPackages =
            epkgs: with epkgs; [
              reformatter
              corfu
              corfu-candidate-overlay
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

            (autoload #'corfu--in-region "corfu")
            (setopt completion-in-region-function 'corfu--in-region)
            (with-eval-after-load 'corfu
              (corfu-candidate-overlay-mode t)
              (setopt
               corfu-cycle t
               corfu-auto nil))

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
            (bind-key "C-c a" #'transient-compile)
            (with-eval-after-load 'transient-compile
              (defmacro my-with-temp-process-buffer (&rest body)
                "Like `with-temp-buffer', but always propagate `process-environment'.
            When that var is buffer-local in the calling buffer, it is not
            propagated by `with-temp-buffer', so we explicitly ensure that
            happens, so that processes will be invoked consistently.  BODY is
            as for that macro."
                (declare (indent 0) (debug (body)))
                (let ((p (cl-gensym)))
                  `(let ((,p process-environment))
                     (with-temp-buffer
                       (setq-local process-environment ,p)
                       ,@body))))

              (defun transient-compile--shell-run (command)
                "Run shell command and return stdout as string."
                (transient-compile--log "Running command: %s" command)
                (my-with-temp-process-buffer
                  (let* ((process-environment
                          (cons "LC_ALL=C" process-environment))
                         (exit-code
                          (process-file-shell-command command nil (current-buffer) nil)))
                    (transient-compile--log "Command finished with status %s" exit-code)
                    (buffer-string))))

              (defun transient-compile-taskfile-targets (directory)
                "Get list of targets from a taskfile."
                (when-let* ((executable (transient-compile--tool-property 'task :exe))
                            (command (transient-compile--shell-join
                                      executable
                                      (unless (transient-compile--tool-property 'task :chdir)
                                        `("-d" , directory))
                                      "--json"
                                      "-l"))
                            (output (transient-compile--shell-run command))
                            (json (json-read-from-string output)))
                  (seq-map (lambda (task)
                             (cdr (assoc 'name task)))
                           (cdr (assoc 'tasks json)))))

              (defun transient-compile-enhanced-group-function (target)
                (if (string-match "^\\(.*?\\):\\(.*\\)$" target)
                    (match-string 1 target)
                  (transient-compile-default-group-function target)))

              (setopt transient-compile-interactive t
                      transient-compile-group-function #'transient-compile-enhanced-group-function))
          '';
          fonts.fontconfig.enable = true;
        };
        nixos = { };
      };
    };
}
# Programming:1 ends here
