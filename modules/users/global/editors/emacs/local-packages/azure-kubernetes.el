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

(provide 'azure-kubernetes)

;;; azure-kubernetes.el ends here
