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


(require 'consult)

;; Local Variables:
;; read-symbol-shorthands: (("t-" . "transducers-"))
;; End:
