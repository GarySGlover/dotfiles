;;; azure-cli.el --- Azure CLI   -*- lexical-binding: t; -*-

;; Package-requires
;; (emacs "31.0.50")

;;; Commentary:
;;

;;; Code:

(defcustom azure-executable "az"
  "The azure cli executable."
  :type 'string)

(defun azure-shell-to-string (&rest args)
  "Run azure shell synchronously and return the string output.
ARGS are the arguments to `azure-executable`."
  (shell-command-to-string
   (string-join (cons azure-executable args) " ")))

(defun azure-shell-json-parse (&rest args)
  "Run azure shell syncronously returning the parsed json."
  (json-parse-string (apply #'azure-shell-to-string
                            (append
                             args
                             '("--output"
                               "json"
                               "--only-show-errors")))
                     :null-object nil))

(provide 'azure-cli)

;;; azure-cli.el ends here
