;;; Package --- early init file  -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
;; Early Init
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle early-init.el
;; :END:

;; Ensure loading of latest package version rather than the first
;; found. Will ensure doesn't load an older byte compiled version when
;; there is a new package version installed.

(setopt load-prefer-newer t)


;; Temporary set garbage collection to high value and set after-init-hook
;; to revert to normal. Improves startup performance whilst maintaining
;; resonable memory utilistation during normal operation.

(let ((original-gc-cons-threshold gc-cons-threshold))
  (setopt gc-cons-threshold most-positive-fixnum)
  (add-hook 'after-init-hook
            (lambda ()
              (setopt gc-cons-threshold original-gc-cons-threshold))
            91))


;; Startup time recording and reporting. As Emacs records the variables
;; can be called at any time, but for now is configured as an after
;; startup hook to report automatically.

(defun cnit-display-startup-time ()
  "Display a message with the time it took to load Emacs."
  (message "Emacs loaded in %s."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract
                     after-init-time before-init-time)))))

(add-hook 'emacs-startup-hook #'cnit-display-startup-time)


;; Disable package installs

(with-eval-after-load 'package
  (fmakunbound 'package-install)
  (fmakunbound 'package-install-file)
  (fmakunbound 'package-install-from-buffer)
  (fmakunbound 'package-install-from-file)
  (fmakunbound 'package-install-selected-packages)
  (fmakunbound 'package-reinstall)
  (fmakunbound 'package-upgrade)
  (fmakunbound 'package-upgrade-all)
  (fmakunbound 'package-vc-install)
  (fmakunbound 'package-vc-install-from-checkout)
  (fmakunbound 'package-vc-install-selected-packages)
  (fmakunbound 'package-vc-upgrade)
  (fmakunbound 'package-vc-upgrade-all))


;; Enable melpa for package searching

(with-eval-after-load 'package
  (add-to-list
   'package-archives '("melpa" . "https://melpa.org/packages/")
   t))


;; Prevent loading of prog mode for scratch buffer. Should improve
;; startup time when adding prog-mode hooks.

(setopt initial-major-mode 'fundamental-mode)


;; Disable UI elements as using Emacs only with keyboard.

(setopt
 inhibit-startup-screen t
 inhibit-splash-screen t)

(when (fboundp 'tool-bar-setup)
  (advice-add 'tool-bar-setup :override #'ignore))
(push '(tool-bar-lines . 0) default-frame-alist)

(push '(menu-bar-lines . 0) default-frame-alist)

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))


;; Disable external UI components as they are not consistent accross
;; different environments.

(setopt
 use-file-dialog nil
 use-dialog-box nil)


;; Set early background colour to help reduce startup flash

(when (fboundp 'cnit-with-system-colour)
  (cnit-with-system-colour-scheme
   (set-face-attribute
    'default nil
    :background "#000000"
    :foreground "#FFFFFF")
   (set-face-attribute
    'default nil
    :background "#FFFFFF"
    :foreground "#000000")))
(provide 'early-init)

;;; early-init.el ends here
