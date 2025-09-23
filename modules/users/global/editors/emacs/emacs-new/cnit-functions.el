;;; Package --- cnit functions file  -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
;; cnit Functions
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle cnit-functions.el
;; :END:

;; This section is for defining custom funcions used throughout init and
;; my usage of Emacs. Each function should include requires for any
;; libraries it depends on. This ensures that the libraries are loaded,
;; but not until they are required.

;; Checking if the system is using dark or light mode.

(defun cnit-system-colour-scheme ()
  "Return 'dark or 'light based on the system color scheme."
  (if (and (boundp 'cnit-gdbus-executable) cnit-gdbus-executable)
      (let*
          ((destination "org.freedesktop.portal.Desktop")
           (object-path "/org/freedesktop/portal/desktop")
           (method "org.freedesktop.portal.Settings.Read")
           (schema "org.freedesktop.appearance")
           (key "color-scheme")
           (command
            (format
             "%s call --session --dest %s --object-path %s --method %s %s %s"
             cnit-gdbus-executable
             destination
             object-path
             method
             schema
             key))
           (output (string-trim (shell-command-to-string command))))
        (if (string-match "(<<uint32 1>>,)" output)
            'dark
          'light))
    'light)) ; Default to light if gdbus is not available.

(defmacro cnit-with-system-colour-scheme (dark-body light-body)
  "Execute DARK-BODY or LIGHT-BODY based on the system color scheme."
  `(let ((scheme (cnit-system-colour-scheme)))
     (cond
      ((eq scheme 'light)
       ,light-body)
      ((eq scheme 'dark)
       ,dark-body)
      (t
       (user-error "Unexpected colour scheme %s"
                   (symbol-name scheme))))))


;; Format all default formatter helper

(defun cnit-update-format-all-formatter (mode formatter)
  "Set FORMATTER for MODE in `format-all-default-formatters'."
  (require 'format-all)
  (let ((entry (assoc mode format-all-default-formatters)))
    (if entry
        (setcdr entry (list formatter))
      (push (list mode formatter) format-all-default-formatters))))
(provide 'init)

;;; cnit-functions.el ends here
