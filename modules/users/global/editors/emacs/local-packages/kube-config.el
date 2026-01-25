;;; kube-config.el --- Kubernetes config management   -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defcustom kube-config-session-root
  (expand-file-name "kube-sessions/" temporary-file-directory)
  "Root directory for ephemeral kubeconfig sessions."
  :type 'directory)

(defvar kube-config--current-session nil
  "Path to the active kubeconfig session file, or nil if none.")

(defvar kube-config--dummy-file
  (expand-file-name "empty-kubeconfig" temporary-file-directory)
  "Dummy kubeconfig used when no session is active.")

(defun kube-config--reset-dummy-file ()
  "Clear contents of kubeconfig file."
  (with-temp-file kube-config--dummy-file
    (insert ""))

  (setenv "KUBECONFIG"
          (or kube-config--current-session kube-config--dummy-file)))

;;;###autoload
(defun kube-config-session-start ()
  "Start a new kubeconfig session.

Creates an empty kubeconfig file and makes it current.
Signals an error if a session is already active."
  (interactive)
  (when kube-config--current-session
    (user-error "A kubeconfig session is already active"))

  (let* ((id (format-time-string "kube-%Y%m%dT%H%M%S"))
         (dir (expand-file-name id kube-config-session-root))
         (config (expand-file-name "config" dir)))
    (make-directory dir t)
    (with-temp-file config
      (insert ""))

    (setq kube-config--current-session config)

    (setenv "KUBECONFIG" kube-config--current-session)

    config))

;;;###autoload
(defun kube-config-session-current ()
  "Return the path of the current kubeconfig session file, or nil."
  kube-config--current-session)

;;;###autoload
(defun kube-config-session-end ()
  "End the current kubeconfig session.

Deletes the session directory and clears session state."
  (interactive)
  (unless kube-config--current-session
    (user-error "No kubeconfig session is active"))

  (let ((dir (file-name-directory kube-config--current-session)))
    (when (and dir (file-directory-p dir))
      (delete-directory dir t)))

  (setq kube-config--current-session nil)

  (kube-config--reset-dummy-file)
  t)

;;;###autoload
(defmacro kube-config-with-session (&rest body)
  "Execute BODY with the current kubeconfig session.

Signals an error if no session is active.
Temporarily binds `process-environment` to include KUBECONFIG
for the duration of BODY."
  `(let ((session (kube-config-session-current)))
     (unless session
       (user-error "No active kubeconfig session"))
     (let ((process-environment
            (cons
             (concat "KUBECONFIG=" session) process-environment)))
       ,@body)))

(provide 'kube-config)

;;; kube-config.el ends here
