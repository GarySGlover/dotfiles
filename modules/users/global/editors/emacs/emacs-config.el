(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; -*- lexical-binding: t -*-

(use-package
 comp
 :custom native-comp-async-report-warnings-errors 'silent)

(use-package package
  :custom
  (package-archives nil "Remove all package download locations"))

(use-package window
  :config
  (defun split-window-max-pixels (&optional window)
    "Split WINDOW based on which directon has the most pixels."
  (let ((window (or window (selected-window))))
    (if (> (window-pixel-height window) (window-pixel-width window))
	(or (and (window-splittable-p window)
		 (with-selected-window window (split-window-below)))
	    (and (window-splittable-p window t)
		 (with-selected-window window (split-window-right)))
	    (let ((split-height-threshold 0))
	      (with-selected-window window (split-window-below))))
      (or (and (window-splittable-p window t)
	       (with-selected-window window (split-window-right)))
	  (and (window-splittable-p window)
	       (with-selected-window window (split-window-below)))
	  (let ((split-width-threshold 0))
	    (with-selected-window window (split-window-right)))))))

  (setopt split-window-preferred-function #'split-window-max-pixels))

(setq-default indent-tabs-mode nil)

(setq-default auto-revert-mode 1)

(save-place-mode 1)

(unbind-key "<f11>" 'global-map)
(unbind-key "<pinch>" 'global-map)

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  (exec-path-from-shell-copy-env "https_proxy")
  (exec-path-from-shell-copy-env "http_proxy"))

(use-package emacs
  :config (setopt select-active-regions nil))

(use-package devil
  :init
  (global-devil-mode)
  :config
  (add-to-list 'devil-special-keys `(", ," . ,(devil-key-executor ",")))
  (setopt devil-key ".")
  (setopt devil-lighter " \U0001F608")
  (setopt devil-prompt "\U0001F608 %t")
  (setopt devil-all-keys-repeatable t)
  (setopt devil-translations
          '((". m m" . "C-M-")
            (". m ." . "M-,")
            (". m z" . "M-")
            (". m" . "M-")
            (". ." . ".")
            (". z" . "C-")
            ("." . "C-")
            (", ," . ",")
            ("," . "C-c")))
  :bind (("C-," . global-devil-mode)
         :map devil-mode-map
         ("," . devil)))

(use-package general
  :init
  (general-define-key
   :keymaps 'global-map
   "<f5>" #'standard-themes-toggle
   "M-S" #'vertico-suspend
   "C-c ." #'embark-act
   "C-c a" #'cnit/global-generic-dispatch
   "C-c c" #'cnit/consult-dispatch
   "C-c g" #'magit-dispatch
   "C-c G" #'cnit/magit-status
   "C-c l" #'gptel-menu
   "C-c n" #'cnit/denote-dispatch
   "C-c p" #'cnit/project-dispatch
   "C-c m" #'cnit/modes-dispatch
   "C-c w" #'cnit/window-dispatch)
  (with-eval-after-load 'org
    (general-define-key
     :keymaps 'org-mode-map
     "C-M-<return>" #'org-meta-return))
  (with-eval-after-load 'corfu-candidate-overlay
    (general-define-key
     :keymaps 'corfu-candidate-overlay-completion-map
     "C-n" #'completion-at-point
     "<tab>" #'corfu-candidate-overlay-complete-at-point))
  (with-eval-after-load 'transient
    (general-define-key
     :keymaps 'transient-base-map
     "<escape>" 'transient-quit-all))
  (with-eval-after-load 'dired
    (general-define-key
     :keymaps 'dired-mode-map
     "?" 'which-key-show-major-mode))
  (with-eval-after-load 'vertico
    (general-define-key
     :keymap 'vertico-map
     "C-<return>" #'vertico-exit-input)))

(use-package transient
  :demand t)

(transient-define-prefix cnit/window-dispatch ()
  "Transient for managing windows"
  [["Windows"
    ("o" "Delete others" ace-delete-other-windows)
    ("d" "Delete" ace-delete-window)
    ("w" "Other" ace-select-window)
    ("s" "Swap" ace-swap-window)
    ]])

(transient-define-prefix cnit/global-generic-dispatch ()
  "Global generic transient"
  [["Narrowing"
    ("n" "Narrow" narrow-to-region)
    ("w" "Widen" widen :if buffer-narrowed-p)]
   ["Editing"
    ("f" "Format" format-all-region-or-buffer)]])

(defun cnit/modes-highlight (mode-symbol text)
  "Return a colored TEXT based on the status of MODE-SYMBOL."
  (if (if (fboundp mode-symbol) (symbol-value mode-symbol) nil)
      (propertize text 'face '(:foreground "green"))
    (propertize text 'face '(:foreground "red"))))

(transient-define-prefix cnit/modes-dispatch ()
  "Transient for toggling minor modes."
  :transient-suffix 'transient--do-stay
  [["Modes"
    ("c" (lambda () (cnit/modes-highlight 'flymake-mode "Flymake"))
     flymake-mode)
    ("d" (lambda () (cnit/modes-highlight 'display-fill-column-indicator-mode "Fill Column Indicator"))
     display-fill-column-indicator-mode)
    ("f" (lambda () (cnit/modes-highlight 'format-all-mode "Format all"))
     format-all-mode)
    ("h" (lambda () (cnit/modes-highlight 'hl-line-mode "Highlight Line"))
     hl-line-mode)
    ("l" (lambda () (cnit/modes-highlight 'display-line-numbers-mode "Line Numbers"))
     display-line-numbers-mode)
    ("m" (lambda () (cnit/modes-highlight 'word-wrap-whitespace-mode "Word Wrap"))
     word-wrap-whitespace-mode)
    ("n" (lambda () (cnit/modes-highlight 'column-number-mode "Column Number"))
     column-number-mode)
    ("o" (lambda () (cnit/modes-highlight 'auto-revert-mode "Auto Revert Mode"))
     auto-revert-mode)
    ("s" (lambda () (cnit/modes-highlight 'flyspell-mode "Flyspell"))
     flyspell-mode)
    ("t" (lambda () (cnit/modes-highlight 'prettify-symbols-mode "Prettify Symbols"))
     prettify-symbols-mode)
    ]
   ["Indent"
    ("a" (lambda () (cnit/modes-highlight 'aggressive-indent-mode "Aggressive Indent"))
     aggressive-indent-mode)
    ("e" (lambda () (cnit/modes-highlight 'electric-indent-mode "Electric Indent"))
     electric-indent-mode)
    ("i" (lambda () (cnit/modes-highlight 'indent-tabs-mode "Indent tabs"))
     indent-tabs-mode)
    ("j" (lambda () (cnit/modes-highlight 'indent-bars-mode "Indent bars"))
     indent-bars-mode)
    ]
   ["Whitespace"
    ("u" (lambda () (cnit/modes-highlight 'ws-butler-mode "WS Butler"))
     ws-butler-mode)
    ("w" (lambda () (cnit/modes-highlight 'whitespace-mode "Whitespace"))
     whitespace-mode)
    ]
   ["Parens"
    ("b" (lambda () (cnit/modes-highlight 'rainbow-mode "Rainbow mode"))
     rainbow-mode)
    ("p" (lambda () (cnit/modes-highlight 'electric-pair-mode "Electric Pair"))
     electric-pair-mode)
    ("r" (lambda () (cnit/modes-highlight 'show-paren-mode "Show Paren"))
     show-paren-mode)
    ]])

(setopt
 scroll-bar-mode nil
 tool-bar-mode nil
 menu-bar-mode nil)

(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :config
  (setopt whitespace-style '(face tab-mark trailing)))

(use-package vertico
  :commands (vertico-mode vertico-suspend)
  :init (vertico-mode)
  :config
  (setopt
   enable-recursive-minibuffers t
   vertico-cycle t
   vertico-buffer-display-action '(display-buffer-in-side-window (side . left))))

(use-package vertico-multiform
  :after vertico
  :commands (vertico-multiform-mode)
  :init (vertico-multiform-mode)
  :config
  (setopt vertico-multiform-commands
          '((consult-line buffer)))
  (setopt vertico-multiform-categories
          '((consult-grep buffer))))

(use-package orderless
  :config
  (setopt
   completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :config
  (setopt
   corfu-auto t
   corfu-cycle t)
  :bind (:map corfu-map
              ("RET" . nil))
  :init (global-corfu-mode t))

(use-package corfu-popupinfo
  :after corfu
  :init (corfu-popupinfo-mode t))

(use-package consult
  :init
  (setopt
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref))

(transient-define-prefix cnit/consult-dispatch ()
  "Transient for Consult commands"
  [["Buffers"
    ("b" "Switch" consult-buffer)
    ("o" "Other window" consult-buffer-other-window)
    ("j" "Project" consult-project-buffer)]
   ["Editing"
    ("y" "Yank" consult-yank-from-kill-ring)
    ("p" "Pop" consult-yank-pop)
    ("r" "Replace" consult-yank-replace)
    ("k" "KMacro" consult-kmacro)]
   ["Navigation"
    ("t" "Goto line" consult-goto-line)
    ("m" "Mark" consult-mark)
    ("M" "Global mark" consult-global-mark)
    ("i" "imenu" consult-imenu :if-not-derived org-mode)
    ("i" "Org Heading" consult-org-heading :if-derived org-mode)
    ("n" "imenu multi" consult-imenu-multi)]
   ["Search"
    ("l" "Line" consult-line)
    ("L" "Line multi" consult-line-multi)
    ("e" "Keep lines" consult-keep-lines)
    ("c" "Focus" consult-focus-lines)] ; Need to account for showing again, call with C-u prefix
   ["Find"
    ("g" "Grep" consult-ripgrep)
    ("G" "Git grep" consult-git-grep)
    ("f" "Find" consult-fd)]
   ])

(use-package keycast
  :commands  (keycast-header-line-mode)
  :init (keycast-header-line-mode))

(use-package embark
  :bind ("C-." . embark-act)
  :config
  (setopt
   embark-cycle-key "."
   embark-verbose-indicator-display-action '(display-buffer-in-side-window (side . left))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package indent-bars
  :config
  (setopt indent-bars-treesit-support t)
  :commands indent-bars-mode)

(use-package hyperbole
  :init (hyperbole-mode 1))

(use-package org
  :hook
  (org-mode . (lambda ()
                (add-hook 'after-save-hook
                          (lambda () (when (eq major-mode 'org-mode) (org-babel-tangle))))))
  :config
  (require 'dash)
  (setopt
   org-pretty-entities t
   org-startup-indented t
   org-src-window-setup 'other-window
   org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a!)" "SCHEDULED(s@)" "HOLD(h@)" "|" "DONE(d@)" "CANCELED(c@)")))
  (-each
      '(("yaml" . "yaml-ts")
	("nix" . "nix-ts"))
    (lambda (x) (add-to-list 'org-src-lang-modes x))))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-modern-indent
  :hook (org-mode . org-modern-indent-mode))

(use-package org-agenda
  :after org
  :config
  (setopt org-agenda-files `(,(expand-file-name "agenda/" "~/"))))

(use-package ob-core
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))

  (defun cnit/org-confirm-babel-evaluate (lang body)
    "Custom confirmation function for evaluating code blocks.
Check if `org-confirm-babel-evaluate` is set for the buffer.
If not, prompt the user whether to allow running all code blocks silently."
    (unless (local-variable-p 'org-confirm-babel-evaluate)
      (if (yes-or-no-p "Run buffer code blocks without confirmation?")
          (setq-local org-confirm-babel-evaluate nil)
	(setq-local org-confirm-babel-evaluate t)))
    org-confirm-babel-evaluate)

  (setopt org-confirm-babel-evaluate 'cnit/org-confirm-babel-evaluate))

(use-package ob-async)

(use-package denote
  :demand t
  :config
  (denote-rename-buffer-mode t)
  (setopt
   denote-directory (expand-file-name "notes/" "~/")
   denote-file-type 'org
   denote-date-prompt-use-org-read-date t)
  :hook (dired-mode . denote-dired-mode))

(transient-define-prefix cnit/denote-dispatch ()
  "Transient for Denote commands"
  [["Notes"
    ("n" "New" denote)
    ("c" "Region" denote-region)
    ("N" "Type" denote-type)
    ("d" "Date" denote-date)
    ("z" "Signature" denote-signature)
    ("t" "Template" denote-template)]
   ["Links"
    ("i" "Link" denote-link)
    ("I" "Add" denote-add-links)
    ("b" "Backlinks" denote-backlinks)
    ("f" "Find" denote-find-link)
    ("F" "Find Backlink" denote-find-backlink)]]
  [["File"
    ("r" "Rename" denote-rename-file)
    ("R" "Rename from front matter" denote-rename-file-using-front-matter)]
   ["Folder"
    ("s" "Search" cnit/find-file-in-notes)
    ("p" "Dired" (lambda () (interactive) (dired denote-directory)))]])

(defun cnit/find-file-in-notes ()
  (interactive)
  "Open file from the denote notes directory"
  (let* ((vc-dirs-ignores (mapcar
                           (lambda (dir)
                             (concat dir "/"))
                           vc-directory-exclusion-list))
         (file (completing-read "Note:" (project--files-in-directory denote-directory vc-dirs-ignores))))
    (when file (find-file file))))

(use-package yasnippet
  :init (yas-global-mode 1))

(use-package yasnippet-capf)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package gptel
  :commands
  (gptel
   gptel-send
   gptel-menu)
  :config
  (setopt
   gptel-model 'gpt-4o-mini
   gptel-default-mode 'org-mode))

(use-package format-all
  :commands format-all-mode
  :config
  (add-to-list 'format-all-default-formatters '("Nix" nixfmt))
  :hook
  ((prog-mode . format-all-mode)
   (format-all-mode . format-all-ensure-formatter)))

(use-package treesit
  :defer t
  :init
  (defun cloveynit/report-unused-ts-modes ()
    "Report TreeSitter modes that are not mapped in
major-mode-remap-alist or auto-mode-alist."
    (let ((ts-modes (apropos-internal "-ts-mode$" 'functionp)))
      (dolist (ts-mode ts-modes)
        (let ((used-in-major-mode-remap-alist
               (seq-some (lambda (entry)
                           (equal ts-mode (cdr entry)))
                         major-mode-remap-alist))
              (used-in-auto-mode-alist
               (seq-some (lambda (entry)
                           (equal ts-mode (cdr entry)))
                         auto-mode-alist))
	      (excluded
	       (seq-some (lambda (entry) (equal ts-mode entry))
			 '(sh--redirect-bash-ts-mode indent-bars--ts-mode))))
          (unless (or used-in-major-mode-remap-alist used-in-auto-mode-alist excluded)
            (warn "TS Mode not mapped: %s" ts-mode))))))

  :config
  (setopt
   treesit-font-lock-level 4
   treesit-extra-load-path `(,(expand-file-name "~/.config/emacs/var/tree-sitter"))
   major-mode-remap-alist '((sh-mode . bash-ts-mode)
  			    (c++-mode . c++-ts-mode)
  			    (c-or-c++-mode . c-or-c++-ts-mode)
  			    (c-mode . c-ts-mode)
  			    (cmake-mode . cmake-ts-mode)
  			    (csharp-mode . csharp-ts-mode)
  			    (css-mode . css-ts-mode)
  			    (indent-bars-mode . indent-bars-ts-mode)
  			    (java-mode . java-ts-mode)
  			    (javascript-mode . js-ts-mode)
  			    (js-json-mode . json-ts-mode)
  			    ;; (nim-mode . nim-ts-mode)
  			    (python-mode . python-ts-mode)
  			    (ruby-mode . ruby-ts-mode)
  			    (conf-toml-mode . toml-ts-mode)))
  (dolist (mode-assoc
  	   '(("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
  	      . dockerfile-ts-mode)
  	     ("/go\\.mod\\'" . go-mod-ts-mode)
  	     ("\\.go\\'" . go-ts-mode)
	     ("\\.nix\\'" . nix-ts-mode)
	     ("\\.rs\\'" . rust-ts-mode)
	     ("\\.ts\\'" . typescript-ts-mode)
	     ("\\.tsx\\'" . tsx-ts-mode)
	     ("\\.ya?ml\\'" . yaml-ts-mode)))
    (add-to-list 'auto-mode-alist mode-assoc))

  (cloveynit/report-unused-ts-modes))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Need to update insert-pair-alist
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-`") 'insert-pair)
(global-set-key (kbd "M-~") 'insert-pair)
(global-set-key (kbd "M-=") 'insert-pair)

(setopt next-line-add-newlines t)

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package eglot
  :init
  (defun cnit/reorder-eldoc-functions ()
    "Fix the order of the eldoc functions so that flymake comes first"
    (setq eldoc-documentation-functions
	  (cons #'flymake-eldoc-function
		(remove #'flymake-eldoc-function eldoc-documentation-functions))))
  :commands (eglot-ensure)
  :hook
  ((prog-mode . eglot-ensure)
   (eglot-managed-mode . cnit/reorder-eldoc-functions))
  :config
  (add-to-list 'eglot-server-programs `(nix-ts-mode . ,(cdr (assoc 'nix-mode eglot-server-programs))))
  (setopt completion-category-defaults nil)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package eldoc
  :config
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package sh-script
  :init
  (setopt
   sh-shell "bash"
   sh-shell-file "bash"))

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

(use-package language-id
  :config
  (setopt language-id--definitions
	  (append
	   '(("Nix" nix-ts-mode)) language-id--definitions)))

(use-package compilation
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package magit)

(use-package
  project
  :config (project-forget-projects-under "~/git-clones" t))

(defun cnit/project--dispact-wrap-command (cmd)
  "Wrap command CMD to optionally display buffer in another window."
  (interactive)
  (let ((display-buffer-overriding-action
         (if (transient-arg-value "other window" (transient-args transient-current-command))
             '(display-buffer-reuse-window (inhibit-same-window . t))
           display-buffer-overriding-action)))
    (call-interactively cmd)))

(transient-define-prefix cnit/project-dispatch ()
  "Transient for project.el commands."
  [["Buffers and Files"
    ("B" "List Buffers" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-list-buffers)))
    ("b" "Consult Buffer" (lambda () (interactive) (cnit/project--dispact-wrap-command 'consult-project-buffer)))
    ("s" "Switch to Buffer" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-switch-to-buffer)))
    ("f" "Find File" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-find-file)))
    ("d" "Dired" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-dired)))
    ("F" "Find Directory" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-find-dir)))]
   ["Search and Replace"
    ("r" "Find Regexp" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-find-regexp)))
    ("q" "Query Replace" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-query-replace-regexp)))]
   ["Project Actions"
    ("c" "Compile" project-compile)
    ("e" "Eshell" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-eshell)))
    ("t" "Shell" (lambda () (interactive) (cnit/project--dispact-wrap-command 'project-shell)))
    ("x" "Shell Command" project-shell-command)
    ("a" "Async Shell Command" project-async-shell-command)
    ("v" "VC-Dir" project-vc-dir)
    ("m" "Magit Status" magit-project-status)
    ("M" "Magit Projects" cnit/magit-status)]
   ["Manage Projects"
    ("S" "Switch Project" project-switch-project)
    ("k" "Kill Buffers" project-kill-buffers)
    ("p" "Forget Project" project-forget-project)
    ("P" "Forget Projects Under" project-forget-projects-under)
    ("z" "Forget Zombie Projects" project-forget-zombie-projects)
    ("R" "Remember Projects Under" project-remember-projects-under)]
   ["Options"
    ("o" "Force Display in Other Window" "other window")]])

(use-package
 direnv
 :config (setopt direnv-always-show-summary nil)
 :init (direnv-mode))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package helpful
  :commands
  (helpful-callable
   helpful-function
   helpful-macro
   helpful-command
   helpful-key
   helpful-variable
   helpful-at-point)
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package which-key
  :demand t
  :init
  (declare-function which-key-mode "which-key")
  :config
  (setopt which-key-idle-delay 1.0)
  (which-key-mode 1))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode))

(use-package ediff
  :defer t
  :config
  (defun cnit/ediff-new-frame ()
    (select-frame (make-frame)))
  (setopt
   ediff-window-setup-function #'ediff-setup-windows-plain
   ediff-keep-variants nil)
  :hook
  ((ediff-before-setup . cnit/ediff-new-frame)
   (ediff-quit . delete-frame)))

(use-package ace-window
  :init
  (setopt display-buffer-base-action
	  '((display-buffer--maybe-same-window
	     display-buffer-reuse-window
	     display-buffer-ace-window)))
  (advice-add 'corfu-popupinfo--show :around #'safe-corfu-popupinfo--show)
  :commands (ace-window aw-select display-buffer-ace-window safe-corfu-popupinfo--show)
  :config
  (defun safe-corfu-popupinfo--show (f candidate)
    (let ((display-buffer-base-action nil))
      (funcall f candidate)))

  (defun cnit/aw-select-force ()
    (let ((window nil))
      (while (not window)
	(condition-case nil
	    (setq window (aw-select nil))
	  (error nil)))
      window))

  (defun display-buffer-ace-window (buffer alist)
    (let ((initial-window-count (length (window-list))))
      (if (eq initial-window-count 1)
	  nil
	(let* ((aw-dispatch-always t)
	       (aw-scope 'frame)
	       (original-window (selected-window))
	       (window (progn
			 (message (format "Switching to: %s" buffer))
			 (cnit/aw-select-force)))
	       (new-window-p (> (length (window-list)) initial-window-count))
	       (window-type (if new-window-p 'window 'reuse)))
	  (progn
	    (select-window original-window)
	    (window--display-buffer buffer window window-type alist)))))))

(use-package esh-mode
  :config
  (defun cnit/eshell-ansi-color ()
    (setenv "TERM" "xterm-256color"))
  :hook ((eshell-mode . cnit/eshell-ansi-color)
         (eshell-mode . eat-eshell-visual-command-mode)))

(defun cnit/get-ticket-numbers ()
  (let ((feature-dir (expand-file-name "~/feature/")))
    (delete-dups
     (mapcar (lambda (dir)
               (let* ((name (file-name-nondirectory dir))
                      (ticket-number (car (split-string name "-"))))
                 ticket-number))
             (directory-files feature-dir t "^[0-9]+-.*")))))

(defun cnit/read-ticket-number ()
  (completing-read "Select ticket number: " (cnit/get-ticket-numbers)))

(defun cnit/get-ticket-name (ticket-number)
  (let* ((feature-dir (expand-file-name "~/feature/"))
         (folders (directory-files feature-dir nil (format "^%s-.*" ticket-number)))
         (existing-names (mapcar (lambda (dir)
                                   (let* ((name (file-name-nondirectory dir))
                                          (ticket-name (replace-regexp-in-string "-" " " (replace-regexp-in-string (format "^%s-\\(.*\\)__.*$" ticket-number) "\\1" name))))
                                     (if ticket-name
                                         (string-trim ticket-name))))
                                 folders)))
    (if existing-names
        (completing-read "Select ticket name: " (delete-dups existing-names))
      (read-string "Enter ticket name: "))))

(defun cnit/magit-repo-name ()
  (replace-regexp-in-string
   "\\." "-"
   (replace-regexp-in-string
    "\\.git$"
    ""
    (file-name-nondirectory
     (magit-get "remote" "origin" "url")))))

(defun cnit/magit-worktree-extract-ticket-number (name)
  (when (string-match "^[0-9]+" name)
    (match-string 0 name)))

(defun cnit/magit-worktree-ticket-number (&optional name)
  (if (and name (cnit/magit-worktree-extract-ticket-number name))
      name
    (cnit/read-ticket-number)))

(defun cnit/magit-repo-name-formatted ()
  (replace-regexp-in-string
   "-" "_"
   (denote-sluggify-title
    (read-string "Enter repository name: " (cnit/magit-repo-name)))))

(defun cnit/magit-worktree-names-format (name repo)
  (let ((kebab-name (denote-sluggify-title name)))
    `(,(format "feature/%s" name)
      ,(format "~/feature/%s__%s" name repo))))

(defun cnit/magit-worktree-names ()
  (let* ((ticket (cnit/read-ticket-number))
         (kebab-name (denote-sluggify-title (cnit/get-ticket-name ticket))))
    (cnit/magit-worktree-names-format (format "%s-%s" ticket kebab-name) (cnit/magit-repo-name-formatted))))

(defun cnit/magit-worktree-new ()
  (interactive)
  (let* ((worktree (cnit/magit-worktree-names))
         (branch (car worktree))
         (path (cadr worktree))
         (starting-point (magit-read-starting-point "Create and checkout branch starting at: ")))
    (magit-worktree-branch path branch starting-point)))

(defun cnit/magit-worktree-checkout ()
  (interactive)
  (let* ((branch (magit-read-branch-or-commit "Checkout"))
         (branch-short (file-name-nondirectory branch))
         (repo-name (cnit/magit-repo-name-formatted))
         (path (cadr (cnit/magit-worktree-names-format branch-short repo-name))))
    (magit-worktree-checkout path branch)))

(eval-after-load 'magit
  (progn
    (require 'magit)
    (require 'transient)
    (require 'denote)
    (transient-append-suffix 'magit-worktree "c" '("f" "Feature worktree" cnit/magit-worktree-new))
    `(transient-append-suffix 'magit-worktree "c" '("w" "Feature checkout" cnit/magit-worktree-checkout))))

(require 'f)
(require 'dash)

(defun cnit/magit-status ()
  "Opens 'magit-status' in the directory selected.
Selection is by organisation under the git-clones root directory"
  (interactive)
  (let* ((root (expand-file-name "~/git-clones"))
         (org (completing-read "Select organisation: " (-map (lambda (f) (f-filename f)) (f-directories root))))
         (project-root (format "%s/" (expand-file-name org root))))
    (magit-status
     (completing-read
      "Project: "
      (mapcan
       (lambda (d)
         (directory-files (concat project-root d) t "\\`[^.]"))
       (-filter
        (lambda (d) (file-directory-p (concat project-root d)))
        (directory-files project-root nil "\\`[^.]")))))))

(defvar cloveynit-pairs-alist
  '((?\( ?\))
    (?\[ ?\])
    (?\{ ?\})
    (?\< ?\>)
    (?\" ?\")
    (?\' ?\')
    (?\` ?\`)
    (?\= ?\=)
    (?\~ ?\~)
    (?\_ ?\_)
    (?\+ ?\+)
    (?\* ?\*)
    (?\/ ?\/)))

(defun cloveynit/surround-region ()
  "Surround active region with paired characters."
  (interactive)
  (when (region-active-p)
    (let ((pair (or (assq last-command-event cloveynit-pairs-alist)
                    (assq (event-basic-type last-command-event) cloveynit-pairs-alist))))
      (when pair
        (insert-pair nil (car pair) (cadr pair))))))
