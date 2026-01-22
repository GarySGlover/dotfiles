;;; azure-account.el --- Kubernetes config management   -*- lexical-binding: t; -*-

;; Package-requires
;; (emacs "31.0.50")
;; (azure-cli "1.0")
;; (consult "20251129.758")

;;; Commentary:
;;


;;; Code:

(require 'azure-cli)
(require 'consult)

(defvar azure-account--list nil
  "Azure account list cache.")

(defun azure-account--list (&optional refresh)
  "Get a list of subscriptions for the logged in account."
  (when (or (not azure-account--list) refresh)
    (when-let* ((json (azure-shell-json-parse "account" "list")))
      (setq azure-account--list json)))
  azure-account--list)

(defun azure-account--select (&optional refresh)
  "Select an Azure subscription."
  (consult--read
   (seq-map
    (lambda (x)
      (propertize (gethash "name" x) 'consult--candidate x))
    (azure-account--list))
   :annotate
   (lambda (x)
     (concat
      (propertize " " 'display '(space :align-to center))
      (gethash "id" (get-text-property 0 'consult--candidate x))))
   :lookup #'consult--lookup-candidate
   :category 'azure-subscription))

(provide 'azure-account)

;;; azure-account.el ends here
