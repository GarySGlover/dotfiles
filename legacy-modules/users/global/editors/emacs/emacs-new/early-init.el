;;; Package --- early init file  -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
;; Early Init
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle early-init.el
;; :END:

;; Startup time recording and reporting. As Emacs records the variables
;; can be called at any time, but for now is configured as an after
;; startup hook to report automatically.

(defun display-startup-time ()
  "Display a message with the time it took to load Emacs."
  (message "Emacs loaded in %s."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract
                     after-init-time before-init-time)))))
(provide 'early-init)

;;; early-init.el ends here
