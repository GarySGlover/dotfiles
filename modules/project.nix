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

            (defun gitr-azure-url (repo)
              "Return SSH Azure DevOps URL for REPO string.
          REPO should be formatted as 'ORG/PROJECT/REPO'."
              (format "git@ssh.dev.azure.com:v3/%s" repo))

            (defun gitr-codeberg-url (repo)
              "Return SSH Codeberg URL for REPO string.
          REPO should be formatted as 'user/repo'."
              (format "ssh://git@codeberg.org/%s.git" repo))

            (defun gitr-github-url (repo)
              "Return SSH GitHub URL for REPO string.
          REPO should be formatted as 'user/repo'."
              (format "git@github.com:%s.git" repo))

            (defvar gitr-db-file (expand-file-name "~/nook/repo-db")
              "File path where the Git repo database plist is stored.")

            (defvar gitr-db nil
              "Plist of known repositories, used as an in-memory Git repo database.")

            (defun gitr-db-save (filename)
              "Save `gitr-db' to FILENAME as a plist."
              (with-temp-buffer
                (prin1 gitr-db (current-buffer))
                (write-region (point-min) (point-max) filename)))

            (defun gitr-db-load (filename)
              "Load a plist from FILENAME into `gitr-db'."
              (setq gitr-db
                    (with-temp-buffer
                      (insert-file-contents filename)
                      (read (current-buffer)))))

            (defun gitr-make-repo (platform name location type)
              `(:platform ,platform :name ,name :location ,location :type ,type))

            (defun gitr-add-record (repo)
              (push repo gitr-db))

            (defun gitr-add-record (repo)
              (unless (member repo gitr-db)
                (push repo gitr-db)))

            (defun gitr-think-of-name1 (db locations)
              (-non-nil
               (-map (lambda (repo)
                (when-let* ((location (seq-contains locations (plist-get repo :location)))
                            (name (plist-get repo :name))
                            (url (pcase (plist-get repo :platform)
                                   ('azure (gitr-azure-url name))
                                   ('github (gitr-github-url name))
                                   ('codeberg (gitr-codeberg-url name))))
                            (dir (file-name-concat
                                  (expand-file-name "~/src")
                                  (concat
                                   (string-replace "/" "--" name)
                                   "__" (symbol-name location)
                                   "__" (symbol-name (plist-get repo :type))))))
                  `(,url ,dir))) db)))

            (when nil
              (when (file-exists-p gitr-db-file)
                (gitr-db-load gitr-db-file))

              (-filter (lambda (x) (not (file-exists-p (cadr x))))
                               (gitr-think-of-name1 gitr-db '(home))) ))
        '';
      };
      nixos = { };
    };
  };
}
# Project:1 ends here
