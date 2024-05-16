((nil
  .
  ((eval
    .
    (progn
      (setq
       compile-command
       (concat
        "nix build .#homeConfigurations."
        (user-login-name)
        "@"
        (system-name)
        ".activationPackage --extra-experimental-features nix-command --extra-experimental-features flakes --show-trace && ./result/activate"))
      (add-hook 'after-save-hook
                (lambda ()
                  (let ((default-directory
                         (project-root (project-current t))))
                    (compile compile-command)))
                nil 'local))))))
