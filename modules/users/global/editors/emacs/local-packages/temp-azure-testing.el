;; -*- lexical-binding: t; -*-

(load
 (expand-file-name "azure-cli.el"
                   (file-name-directory buffer-file-name)))
(load
 (expand-file-name "azure-account.el"
                   (file-name-directory buffer-file-name)))
(load
 (expand-file-name "azure-kubernetes.el"
                   (file-name-directory buffer-file-name)))


(setopt azure-executable cnit-az-executable)

(azure-account--list)


(let ((s (azure-account--select)))
  (azure-kubernetes--list s))


;; Define a prefix key for kube-config commands
(defvar kube-config-keymap (make-sparse-keymap)
  "Keymap for kube-config commands, bound to C-c k.")

;; Bind commands to the keymap
(bind-key "s" #'kube-config-session-start kube-config-keymap) ;; s = start session
(bind-key "e" #'kube-config-session-end kube-config-keymap) ;; e = end session

;; Bind the prefix key globally
(bind-key "C-c k" kube-config-keymap)

(kube-config-with-session
 (message "Current kubeconfig: %s" (getenv "KUBECONFIG")))

(require 'consult)

;; Local Variables:
;; read-symbol-shorthands: (("t-" . "transducers-"))
;; End:
