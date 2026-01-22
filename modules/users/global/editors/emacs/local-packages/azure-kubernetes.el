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

(defun azure-kubernetes--list (subscription &optional refresh)
  "Get a list of kubernetes clusters in SUBSCRIPTION."
  (let* ((subscription-id (gethash "id" subscription)))
    (if-let* ((clusters
               (and (not refresh)
                    (gethash
                     subscription-id azure-kubernetes--list))))
      clusters
      (when-let* ((json
                   (azure-shell-json-parse
                    "aks" "list" "--subscription" subscription-id)))
        (puthash subscription-id json azure-kubernetes--list)
        json))))

(provide 'azure-kubernetes)

;;; azure-kubernetes.el ends here
