;;; azure-cli.el --- Azure CLI   -*- lexical-binding: t; -*-

;; Package-requires
;; (emacs "31.0.50")

;;; Commentary:
;;

;;; Code:

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
  "Parse JSON buffer for Azure work"
  (with-current-buffer buffer
    (goto-char (point-min))
    (json-parse-buffer
     :object-type 'plist
     :array-type 'list
     :null-object nil
     :false-object nil)))

(defun azure--prepare-consult-table (item-property plist)
  (mapcar
   (lambda (x)
     (propertize (plist-get x item-property) 'consult--candidate x))
   plist))

(defun azure--center-annotate (annotation)
  (concat
   (propertize " " 'display '(space :align-to center)) annotation))

(provide 'azure-cli)

;;; azure-cli.el ends here
