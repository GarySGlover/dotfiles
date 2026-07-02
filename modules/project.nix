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
              (add-hook 'magit-post-clone-hook (lambda () (project-remember-projects-under default-directory)))))

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
              (pp gitr-db (current-buffer))
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

          (defun gitr-filter-repos-by-location (db locations)
            "Return repos from DB that have :name, :type, :platform and whose :location is in LOCATIONS."
            (cl-remove-if-not
             (lambda (repo)
               (and (plist-get repo :name)
                    (plist-get repo :type)
                    (plist-get repo :platform)
                    (seq-contains locations (plist-get repo :location))))
             db))

          (defun gitr-repo-url-and-dir (repo)
            "Make (url dir) pair for REPO if platform and relevant keys are present."
            (let* ((name (plist-get repo :name))
                   (type (plist-get repo :type))
                   (plat (plist-get repo :platform))
                   (location (plist-get repo :location))
                   (url (pcase plat
                          ('azure   (gitr-azure-url name))
                          ('github  (gitr-github-url name))
                          ('codeberg (gitr-codeberg-url name))))
                   (dir (file-name-concat
                         (expand-file-name "~/src")
                         (concat (string-replace "/" "--" name)
                                 "__" (symbol-name location)
                                 "__" (symbol-name type)))))
              (when (and url dir)
                (list url dir))))

          (defun gitr-map-repos-to-url-dirs (repos)
            "Map each REPO to a (url dir) list, omitting nils."
            (cl-loop for repo in repos
                     for result = (gitr-repo-url-and-dir repo)
                     when result
                     collect result))

          (defun gitr-pull-missing-repos (repos)
            "Clone all REPOS (a (url dir) list) as bare repositories if missing.
          Runs as a single background job; output in a buffer.
          On completion, calls `gitr-remember-projects' for cloned repos."
            (when-let* ((todo (cl-remove-if (lambda (x) (file-exists-p (cl-second x))) repos))
                        (commands (mapcar (lambda (x)
                                            (let ((url (cl-first x))
                                                  (dir (cl-second x)))
                                              (format "git clone --bare %s %s/.git && git -C %s/.git config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'"
                                                      url dir dir)))
                                          todo))
                        (cmd-str (string-join commands " && ")))
              (async-start-process
               "gitr-clone-repos"
               shell-file-name
               (lambda (_process)
                 (gitr-remember-projects (mapcar #'cadr todo)))
               shell-command-switch cmd-str)))

          (defun gitr-pull-missing-repos (repos)
            "Clone all REPOS (a (url dir) list) as bare repositories if missing.
          Runs as a single background job; output in a buffer.
          On completion, calls `gitr-remember-projects' for cloned repos."
            (when-let* ((todo (cl-remove-if (lambda (x) (file-exists-p (cl-second x))) repos))
                        (commands (mapcar (lambda (x)
                                            (let ((url (cl-first x))
                                                  (dir (cl-second x)))
                                              (format "git clone --bare %s %s/.git && git -C %s/.git config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'"
                                                      url dir dir)))
                                          todo))
                        (cmd-str (string-join commands " && "))
                        (buf (get-buffer-create "*gitr-clone-repos*")))
              (with-current-buffer buf (erase-buffer))
              (let ((proc (start-process-shell-command
                           "gitr-clone-repos"
                           buf
                           cmd-str)))
                (set-process-sentinel
                 proc
                 (lambda (process event)
                   (when (memq (process-status process) '(exit signal))
                     (with-current-buffer (process-buffer process)
                       (goto-char (point-max))
                       (insert (format "\nProcess %s finished: %s" process event)))
                     (gitr-remember-projects (mapcar #'cadr todo))))))))

          (defun gitr-remember-projects (dirs)
            "Call `project-remember-project' for each directory in DIRS."
            (dolist (dir dirs)
              (when (file-exists-p dir)
                (project-remember-projects-under dir))))

          (defun gitr-pull-or-remember (repos)
            (when-let* ((todo (cl-remove-if-not (lambda (x) (file-exists-p (cl-second x))) repos)))
              (gitr-remember-projects (mapcar #'cadr todo)))
            (gitr-pull-missing-repos repos))

          (when nil
            (when (file-exists-p gitr-db-file)
              (gitr-db-load gitr-db-file))

            (gitr-pull-or-remember
             (gitr-map-repos-to-url-dirs
              (gitr-filter-repos-by-location gitr-db '(home)))))
        '';
      };
      nixos = { };
    };
  };
}
# Project:1 ends here
