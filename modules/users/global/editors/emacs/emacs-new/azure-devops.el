;;; azure-devops.el --- Azure Devops Integration   -*- lexical-binding: t; -*-

;;; Commentary:
;; Integrations between Emacs and the Azure Devops platform.

;;; Code:

(defcustom azure-devops-git-protocol 'ssh
  "Protocol to use for git origin operations."
  :type 'symbol
  :options '('ssh 'https)
  :group 'azure-devops)

(defcustom azure-devops-org nil
  "Default azure devops org url to use.")

;; Azure Devops Projects
(defun azure-devops--fetch-projects (&optional org)
  "Return list of projects in ORG.
If ORG is nil, return nil."
  (when-let* ((org (or org azure-devops-org))
              (json
               (shell-command-to-string
                (format
                 "%s devops project list --org %s --output json"
                 cnit-az-executable org)))
              (projects
               (gethash
                "value" (json-parse-string json :null-object nil))))
    projects))

(defun azure-devops--project-annotation (cand)
  (let ((proj (get-text-property 0 'azure-project cand)))
    (when proj
      (format "  %s" (gethash "description" proj)))))

(defun azure-devops--project-annotation (cand)
  (when-let* ((proj (get-text-property 0 'azure-project cand))
              (description (gethash "description" proj)))
    (concat
     (propertize " " 'display '(space :align-to center))
     (format "%s" description))))

(defun azure-devops--project-candidates (projects)
  (mapcar
   (lambda (proj)
     (propertize (gethash "name" proj) 'azure-project proj))
   projects))

(defun azure-devops--project-collection (collection)
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . azure-devops-project)
          (annotation-function . azure-devops--project-annotation))
      (complete-with-action action collection string pred))))

(defun azure-devops--read-project ()
  (let* ((minibuffer-allow-text-properties t)
         (projects (azure-devops--fetch-projects))
         (canditates (azure-devops--project-candidates projects))
         (collection-function
          (azure-devops--project-collection canditates)))
    (completing-read "Project: " collection-function nil t)))

(let ((azure-devops-org "https://dev.azure.com/Next-Technology"))
  (azure-devops--read-project))

(provide 'azure-devops)

;;; azure-devops.el ends here

;; Example embark integrations with embark-keymap and functions that act on a project.
(defvar-keymap azure-devops-project-embark-map
  :doc "Keymap for Embark actions for Azure DevOps projects."
  :parent
  embark-general-map
  "i"
  #'azure-devops-project-info
  "l"
  #'azure-devops-project-list-repos)
(add-to-list
 'embark-keymap-alist
 '(azure-devops-project . azure-devops-project-embark-map))

(defun azure-devops-project-info (project)
  "Show info about the Azure DevOps PROJECT."
  (interactive (list (azure-devops--read-project)))
  (message "Project: %s\nDescription: %s\nID: %s"
           (gethash "name" project)
           (gethash "description" project)
           (gethash "id" project)))

(defun azure-devops-project-list-repos (project)
  "List repositories for the Azure DevOps PROJECT."
  (interactive (list (azure-devops--read-project)))
  (let ((repos (azure-devops-repos-list project)))
    (message "Repos: %s"
             (mapconcat (lambda (repo) (gethash "name" repo)) repos
                        ", "))))

;; Build plan
;; List projects -> select project -> list repos -> select 1 or many repos -> git clone bare repos
;; > prefix arg -> list projects/repos -> select 1 or many repos -> git clone bare repos
