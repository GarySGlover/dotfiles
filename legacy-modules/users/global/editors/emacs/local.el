;;; local.el --- Locally-kept Emacs config  -*- lexical-binding: t; -*-

;; The only elisp still written here (everything else now comes from
;; upstream's config via `pkgs.mkEmacsWith').  Kept: the `gitr-*'
;; repo-tracking helpers and the Azure/Kubernetes commands.
;;
;; The Azure code was flattened out of the old `local-packages/' Emacs
;; package directory: `azure-cli.el', `azure-account.el',
;; `azure-kubernetes.el' and `kube-config.el', keeping only what is
;; reachable from `azure-kubernetes-get-credentials' and
;; `kube-config-clear'.

;;; Code:

(require 'cl-lib)
(require 'dash)    ; `-when-let*', `-each', `-sort'
(require 'consult) ; `consult--read', `consult--lookup-candidate'

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

(defun gitr-github-next-url (repo)
  "Return SSH GitHub URL for REPO string.
REPO should be formatted as 'org/repo'."
  (format "next-plc@next-plc.ghe.com:%s.git" repo))

(defvar gitr-db-file (expand-file-name "~/nook/repo-db")
  "File path where the Git repo database plist is stored.")

(defvar gitr-db nil
  "Plist of known repositories, used as an in-memory Git repo database.")

(defun gitr-db-save (filename)
  "Save `gitr-db' to FILENAME as a plist."
  (with-temp-buffer
    (pp (-sort (lambda (a b)
                 (-when-let* (((&plist :location la :platform pa :name na) a)
                              ((&plist :location lb :platform pb :name nb) b))
                   (cond
                    ((not (eq la lb)) (string< (symbol-name la) (symbol-name lb)))
                    ((not (eq pa pb)) (string< (symbol-name pa) (symbol-name pb)))
                    (t (string< na nb)))))
               gitr-db)
        (current-buffer))
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
  (unless (member repo gitr-db)
    (push repo gitr-db)))

(defun gitr-filter-repos-by-location (db locations)
  "Return repos from DB whose :location is in LOCATIONS.
Only repos that also have :name, :type and :platform are returned."
  (cl-remove-if-not
   (lambda (repo)
     (and (plist-get repo :name)
          (plist-get repo :type)
          (plist-get repo :platform)
          (seq-contains-p locations (plist-get repo :location))))
   db))

(defun gitr-repo-url-and-dir (repo)
  "Make (url dir) pair for REPO if platform and relevant keys are present."
  (let* ((name (plist-get repo :name))
         (type (plist-get repo :type))
         (plat (plist-get repo :platform))
         (location (plist-get repo :location))
         (url (funcall (intern (format "gitr-%s-url" plat)) name))
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

(defun gitr-sync ()
  "Loads db and downloads repos."
  (interactive)
  (when (file-exists-p gitr-db-file) (gitr-db-load gitr-db-file))
  (-each gitr-db #'gitr-migrate-repo)
  (let ((repos (gitr-filter-repos-by-location gitr-db '(home work))))
    (gitr-pull-or-remember (gitr-map-repos-to-url-dirs repos))))

(defun gitr-migrate-repo (repo)
  (-when-let* (((&plist
                 :name name
                 :type type
                 :Platform plat
                 :location location
                 :mplatform mplat
                 :mname mname)
                repo)
               (mrepo `(:platform ,mplat :name ,mname :type ,type :location ,location))
               ((url dir) (gitr-repo-url-and-dir repo))
               ((murl mdir) (gitr-repo-url-and-dir mrepo)))
    (rename-file dir mdir)
    (let ((default-directory (file-name-as-directory mdir)))
      (call-process "git" nil nil nil "remote" "add" "legacy" url)
      (call-process "git" nil nil nil "remote" "set-url" "origin" murl))
    (setq gitr-db (mapcar (lambda (r) (if (equal r repo) mrepo r)) gitr-db))
    (gitr-db-save gitr-db-file)))

;;; Azure CLI

(defcustom azure-executable "az"
  "The azure cli executable."
  :type 'string)

(defun azure-shell (output-buffer &rest args)
  "Run azure shell synchronously, logging command to *az:log*."
  (let* ((log-buffer (get-buffer-create "*az:log*"))
         (cmdargs
          (string-join (append args '("--output" "json")) " "))
         (cmdstr (format "az %s" cmdargs)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (format "\n$ %s\n" cmdstr)))
    (save-window-excursion
      (shell-command (format "%s %s" azure-executable cmdargs)
                     output-buffer log-buffer))))

(defun azure-json-parse-buffer (buffer)
  "Parse JSON buffer for Azure work."
  (with-current-buffer buffer
    (goto-char (point-min))
    (json-parse-buffer
     :object-type 'plist
     :array-type 'list
     :null-object nil
     :false-object nil)))

(defun azure--prepare-consult-table (item-property plist)
  "Turn PLIST into a list of `consult--read' candidates on ITEM-PROPERTY."
  (mapcar
   (lambda (x)
     (propertize (plist-get x item-property) 'consult--candidate x))
   plist))

(defun azure--center-annotate (annotation)
  "Center ANNOTATION in the minibuffer."
  (concat
   (propertize " " 'display '(space :align-to center)) annotation))

;;; Azure account

(defun azure-account--list ()
  "Get a list of subscriptions for the logged in account."
  (let ((buf (get-buffer-create "*az:accounts*")))
    (when (= (buffer-size buf) 0)
      (azure-shell buf "account" "list"))
    buf))

(defun azure-account--select ()
  "Select an Azure subscription."
  (consult--read
   (azure--prepare-consult-table
    :name (azure-json-parse-buffer (azure-account--list)))
   :annotate
   (lambda (x)
     (azure--center-annotate
      (plist-get (get-text-property 0 'consult--candidate x) :id)))
   :lookup #'consult--lookup-candidate
   :category 'azure-subscription))

;;; Azure Kubernetes

(defun azure-kubernetes--list (subscription)
  "Get a list of kubernetes clusters in SUBSCRIPTION."
  (when-let* ((sub-name (plist-get subscription :name))
              (buf
               (get-buffer-create
                (format "*az:%s:kubernetes*" sub-name))))
    (when (= (buffer-size buf) 0)
      (azure-shell buf "aks" "list" "--subscription" sub-name))
    buf))

(defun azure-kubernetes--select (&optional subscription)
  "Select an Azure Kubernetes cluster."
  (when-let* ((subscription
               (or subscription (azure-account--select))))
    (consult--read
     (azure--prepare-consult-table
      :name
      (azure-json-parse-buffer (azure-kubernetes--list subscription)))
     :lookup #'consult--lookup-candidate
     :category 'azure-kubernetes-cluster)))

(defun azure-kubernetes-get-credentials (subscription cluster)
  "Get credentials for the CLUSTER in SUBSCRIPTION.
If called interactively, prompt for both using `azure-account--select' and `azure-kubernetes--select'."
  (interactive (let ((sub (azure-account--select)))
                 (list sub (azure-kubernetes--select sub))))
  (when-let* ((name (plist-get cluster :name))
              (rg (plist-get cluster :resourceGroup))
              (buf
               (get-buffer-create
                (format "*az:kubernetes:credentials:%s*" name))))
    (azure-shell
     buf
     "aks"
     "get-credentials"
     "--name"
     name
     "--resource-group"
     rg
     "--subscription"
     (plist-get subscription :name))
    buf))

;;; Kubernetes config

(defun kube-config-clear ()
  "Clear contents of kubeconfig file."
  (interactive)
  (let ((file (expand-file-name ".kube/config" "~/")))
    (when (file-exists-p file)
      (with-temp-file file))))

;; Confirmed den aspect required settings
(use-package files
  :custom
  (dir-locals-trusted-dirs
   '("~/nook/" "~/projects/work/" "~/projects/personal/")))
(use-package denote
  :custom (denote-directory (expand-file-name "~/nook/notes/")))
(use-package org
  :custom (org-directory "~/nook/agenda"))

(provide 'local)

;;; local.el ends here
