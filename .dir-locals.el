((nil
  .
  ((eval .
         (progn
           (setq compile-command
                 "nix flake check ./. --show-trace --all-systems")
           (add-hook 'after-save-hook
                     (lambda ()
                       (let ((default-directory
                              (project-root (project-current t))))
                         (compile compile-command)))
                     nil 'local))))))
