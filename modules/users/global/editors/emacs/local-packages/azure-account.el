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

(provide 'azure-account)

;;; azure-account.el ends here
