;;; azure-kubernetes.el --- Azure Kubernetes   -*- lexical-binding: t; -*-

;; Package-requires
;; (emacs "31.0.50")
;; (azure-cli "1.0")
;; (azure-account "1.0")

;;; Commentary:
;;

;;; Code:

(require 'azure-cli)
(require 'azure-account)

(defvar azure-kubernetes--list (make-hash-table)
  "Hash table of cached Kubernetes clusters")

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

;;;###autoload
(defun azure-kubernetes-get-credentials (subscription cluster)
  "Get credentials for the CLUSTER in SUBSCRIPTION.
If called interactively, prompt for both using `azure-account--select` and `azure-kubernetes--select`."
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

(provide 'azure-kubernetes)

;;; azure-kubernetes.el ends here
