# [[file:../modules.org::*Project][Project:1]]
{
  flake.aspects = {
    programming = {
      homeManager = {
        programs.git.signing.format = null;
        editor.initFiles.project.text = ''
          (with-eval-after-load 'magit
            (magit-add-section-hook 'magit-status-sections-hook
                                    'magit-insert-worktrees nil t)
            (magit-add-section-hook 'magit-status-sections-hook
                                    'magit-insert-modules nil t)
            (magit-add-section-hook 'magit-status-sections-hook
                                    'magit-insert-local-branches nil t)
            (magit-add-section-hook 'magit-status-sections-hook
                                    'magit-insert-remote-branches nil t)
            (with-eval-after-load 'project
              (add-hook 'magit-post-clone-hook (lambda () (project-remember-projects-under default-directory))))
            (defun magit--clone-bare-around (orig-fun repository directory args)
              "Ensure DIRECTORY ends with /.git before calling ORIG-FUN."
              (let ((bare-directory
                     (if (string-suffix-p "/.git" directory)
                         directory
                       (concat (directory-file-name directory) "/.git"))))
                (apply orig-fun `(,repository ,bare-directory ,args))))

            (advice-add 'magit-clone-bare :around #'magit--clone-bare-around)

            (defun magit--clone-bare-after (repository directory args)
              "Run =git config +refs/heads/*:refs/remotes/origin/*=."
              (let ((default-directory (file-name-as-directory (expand-file-name directory))))
                (magit-call-git "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")))

            (advice-add 'magit-clone-bare :after #'magit--clone-bare-after))

          (when nil
            (require 'cl-lib)

            (defun my-get-azure-url (repo)
              (format "git@ssh.dev.azure.com:v3/%s" repo))

            (defun my-get-codeburg-url (repo)
              (format "ssh://git@codeberg.org/%s.git" repo))

            (defun my-get-github-url (repo)
              (format "git@github.com:%s.git" repo))

            (let ((repo '(azure "Next-Technology/Ecom.Infrastructure/Ecom.Account" home code)))
              (cl-destructuring-bind (platform name location type) repo
                (when-let* ((url
                             (pcase (car repo)
                               ('azure (my-get-azure-url name))
                               ('github (my-get-github-url name))
                               ('codeburg (my-get-codeburg name))))
                            (dir (file-name-concat
                                  (expand-file-name "~/src")
                                  (concat
                                   (string-replace "/" "--" name)
                                   "__" (symbol-name location)
                                   "__" (symbol-name type)))))
                  (magit-clone-bare url dir nil)))))
        '';
      };
      nixos = { };
    };
  };
}
# Project:1 ends here
