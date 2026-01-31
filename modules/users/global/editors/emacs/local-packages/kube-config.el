;;; kube-config.el --- Kubernetes config management   -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun kube-config-clear ()
  "Clear contents of kubeconfig file."
  (interactive)
  (let ((file (expand-file-name ".kube/config" "~/")))
    (when (file-exists-p file)
      (with-temp-file file))))

(provide 'kube-config)

;;; kube-config.el ends here
