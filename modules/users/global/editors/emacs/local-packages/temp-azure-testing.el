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

(require 'consult)
(let ((s
       (consult--read
        (seq-map
         (lambda (x)
           (propertize (gethash "name" x) 'consult--candidate x))
         (azure-account--list))
        :annotate
        (lambda (x)
          (concat
           (propertize " " 'display '(space :align-to center))
           (gethash
            "id" (get-text-property 0 'consult--candidate x))))
        :lookup #'consult--lookup-candidate
        :category 'azure-subscription)))
  (azure-kubernetes--list s))
