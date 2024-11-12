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

(use-package
  meow
  :init (meow-global-mode 1)
  :config
  (add-to-list 'meow-selection-command-fallback '(meow-replace . meow-yank))
  (setopt
   meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
   meow-use-clipboard t)
  ;; Set keys for MOTION state. This is the state used in read-only style buffers like dired/help/magit
  (meow-motion-overwrite-define-key
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("r" . cloveynit-global-dispatch)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Allow SPC h/j/k/l to run the original command that will be bound to H-<h/j/k/l>
   '("h" . "H-h")
   '("j" . "H-j")
   '("k" . "H-k")
   '("l" . "H-l")
   '("r" . "H-r")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   ;; Use SPC //? for accessing meow help
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-replace)
   '("q" . meow-quit)
   '("r" . cloveynit-global-dispatch)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package general
  :init
  (general-define-key
   :keymaps 'global-map
   "<f5>" #'standard-themes-toggle
   "M-S" #'vertico-suspend
   "C-." #'embark-act)
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
  :init
  (transient-define-prefix cloveynit-global-dispatch ()
    "Global transient menu"
    [["Default"
      ("a" "Generic" cloveynit-global-generic-dispatch)
      ("c" "Consult" cloveynit-consult-dispatch)
      ("g" "Magit" magit-dispatch)
      ("h" "Help" cloveynit-helpful-dispatch)
      ("l" "Gptel" gptel-menu)
      ("n" "Denote" cloveynit-denote-dispatch)
      ("p" "Project" cloveynit-project-dispatch)
      ("m" "Modes" cloveynit-modes-dispatch)
      ("w" "Window" cloveynit-window-dispatch)
      ]]))

(transient-define-prefix cloveynit-window-dispatch ()
  "Transient for managing windows"
  [["Windows"
    ]])

(transient-define-prefix cloveynit-global-generic-dispatch ()
  "Global generic transient"
  [["Narrowing"
    ("n" "Narrow" narrow-to-region)
    ("w" "Widen" widen :if buffer-narrowed-p)]
   ["Editing"
    ("f" "Format" format-all-region-or-buffer)]])

(defun cloveynit-modes-highlight (mode-symbol text)
  "Return a colored TEXT based on the status of MODE-SYMBOL."
  (if (if (fboundp mode-symbol) (symbol-value mode-symbol) nil)
      (propertize text 'face '(:foreground "green"))
    (propertize text 'face '(:foreground "red"))))

(transient-define-prefix cloveynit-modes-dispatch ()
  "Transient for toggling minor modes."
  :transient-suffix 'transient--do-stay
  [["Modes"
    ("c" (lambda () (cloveynit-modes-highlight 'flymake-mode "Flymake"))
     flymake-mode)
    ("d" (lambda () (cloveynit-modes-highlight 'display-fill-column-indicator-mode "Fill Column Indicator"))
     display-fill-column-indicator-mode)
    ("f" (lambda () (cloveynit-modes-highlight 'format-all-mode "Format all"))
     format-all-mode)
    ("h" (lambda () (cloveynit-modes-highlight 'hl-line-mode "Highlight Line"))
     hl-line-mode)
    ("l" (lambda () (cloveynit-modes-highlight 'display-line-numbers-mode "Line Numbers"))
     display-line-numbers-mode)
    ("m" (lambda () (cloveynit-modes-highlight 'word-wrap-whitespace-mode "Word Wrap"))
     word-wrap-whitespace-mode)
    ("n" (lambda () (cloveynit-modes-highlight 'column-number-mode "Column Number"))
     column-number-mode)
    ("o" (lambda () (cloveynit-modes-highlight 'auto-revert-mode "Auto Revert Mode"))
     auto-revert-mode)
    ("s" (lambda () (cloveynit-modes-highlight 'flyspell-mode "Flyspell"))
     flyspell-mode)
    ("t" (lambda () (cloveynit-modes-highlight 'prettify-symbols-mode "Prettify Symbols"))
     prettify-symbols-mode)
    ]
   ["Indent"
    ("a" (lambda () (cloveynit-modes-highlight 'aggressive-indent-mode "Aggressive Indent"))
     aggressive-indent-mode)
    ("e" (lambda () (cloveynit-modes-highlight 'electric-indent-mode "Electric Indent"))
     electric-indent-mode)
    ("i" (lambda () (cloveynit-modes-highlight 'indent-tabs-mode "Indent tabs"))
     indent-tabs-mode)
    ("j" (lambda () (cloveynit-modes-highlight 'indent-bars-mode "Indent bars"))
     indent-bars-mode)
    ]
   ["Whitespace"
    ("u" (lambda () (cloveynit-modes-highlight 'ws-butler-mode "WS Butler"))
     ws-butler-mode)
    ("w" (lambda () (cloveynit-modes-highlight 'whitespace-mode "Whitespace"))
     whitespace-mode)
    ]
   ["Parens"
    ("b" (lambda () (cloveynit-modes-highlight 'rainbow-mode "Rainbow mode"))
     rainbow-mode)
    ("p" (lambda () (cloveynit-modes-highlight 'electric-pair-mode "Electric Pair"))
     electric-pair-mode)
    ("r" (lambda () (cloveynit-modes-highlight 'show-paren-mode "Show Paren"))
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

(transient-define-prefix cloveynit-consult-dispatch ()
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
  :config
  (setopt embark-verbose-indicator-display-action '(display-buffer-in-side-window (side . left))))

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

  (defun cloveynit-org-confirm-babel-evaluate (lang body)
    "Custom confirmation function for evaluating code blocks.
Check if `org-confirm-babel-evaluate` is set for the buffer.
If not, prompt the user whether to allow running all code blocks silently."
    (unless (local-variable-p 'org-confirm-babel-evaluate)
      (if (yes-or-no-p "Run buffer code blocks without confirmation?")
          (setq-local org-confirm-babel-evaluate nil)
	(setq-local org-confirm-babel-evaluate t)))
    org-confirm-babel-evaluate)

  (setopt org-confirm-babel-evaluate 'cloveynit-org-confirm-babel-evaluate))

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

(transient-define-prefix cloveynit-denote-dispatch ()
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
    ("s" "Search" cloveynit-find-file-in-notes)
    ("p" "Dired" (lambda () (interactive) (dired denote-directory)))]])

(defun cloveynit-find-file-in-notes ()
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

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package eglot
  :init
  (defun cloveynit-reorder-eldoc-functions ()
    "Fix the order of the eldoc functions so that flymake comes first"
    (setq eldoc-documentation-functions
	  (cons #'flymake-eldoc-function
		(remove #'flymake-eldoc-function eldoc-documentation-functions))))
  :commands (eglot-ensure)
  :hook
  ((prog-mode . eglot-ensure)
   (eglot-managed-mode . cloveynit-reorder-eldoc-functions))
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

(defun cloveynit-project--dispact-wrap-command (cmd)
  "Wrap command CMD to optionally display buffer in another window."
  (interactive)
  (let ((display-buffer-overriding-action
         (if (transient-arg-value "other window" (transient-args transient-current-command))
             '(display-buffer-reuse-window (inhibit-same-window . t))
           display-buffer-overriding-action)))
    (call-interactively cmd)))

(transient-define-prefix cloveynit-project-dispatch ()
  "Transient for project.el commands."
  [["Buffers and Files"
    ("B" "List Buffers" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-list-buffers)))
    ("b" "Consult Buffer" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'consult-project-buffer)))
    ("s" "Switch to Buffer" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-switch-to-buffer)))
    ("f" "Find File" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-find-file)))
    ("d" "Dired" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-dired)))
    ("F" "Find Directory" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-find-dir)))]
   ["Search and Replace"
    ("r" "Find Regexp" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-find-regexp)))
    ("q" "Query Replace" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-query-replace-regexp)))]
   ["Project Actions"
    ("c" "Compile" project-compile)
    ("e" "Eshell" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-eshell)))
    ("t" "Shell" (lambda () (interactive) (cloveynit-project--dispact-wrap-command 'project-shell)))
    ("x" "Shell Command" project-shell-command)
    ("a" "Async Shell Command" project-async-shell-command)
    ("v" "VC-Dir" project-vc-dir)
    ("m" "Magit Status" magit-project-status)
    ("M" "Magit Projects" cloveynit-magit-status)]
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
  :init
  (transient-define-prefix cloveynit-helpful-dispatch ()
    "Transient for Help commands"
    ["Helpful"
     [("c" "Callable" helpful-callable)
      ("f" "Function" helpful-function)
      ("x" "Command" helpful-command)
      ("m" "Macro" helpful-macro)
      ("k" "Key" helpful-key)
      ("v" "Variable" helpful-variable)
      ("p" "At point" helpful-at-point)]]))

(use-package which-key
  :commands which-key-show-major-mode)

(use-package dired-x
  :hook (dired-mode . dired-omit-mode))

(use-package ediff
  :defer t
  :config
  (defun cloveynit-ediff-new-frame ()
    (select-frame (make-frame)))
  (setopt
   ediff-window-setup-function #'ediff-setup-windows-plain
   ediff-keep-variants nil)
  :hook
  ((ediff-before-setup . cloveynit-ediff-new-frame)
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

  (defun cloveynit-aw-select-force ()
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
			 (cloveynit-aw-select-force)))
	       (new-window-p (> (length (window-list)) initial-window-count))
	       (window-type (if new-window-p 'window 'reuse)))
	  (progn
	    (select-window original-window)
	    (window--display-buffer buffer window window-type alist)))))))

(use-package esh-mode
  :config
  (defun cloveynit-eshell-ansi-color ()
    (setenv "TERM" "xterm-256color"))
  :hook ((eshell-mode . cloveynit-eshell-ansi-color)
         (eshell-mode . eat-eshell-visual-command-mode)))

(defun cloveynit-get-ticket-numbers ()
  (let ((feature-dir (expand-file-name "~/feature/")))
    (delete-dups
     (mapcar (lambda (dir)
               (let* ((name (file-name-nondirectory dir))
                      (ticket-number (car (split-string name "-"))))
                 ticket-number))
             (directory-files feature-dir t "^[0-9]+-.*")))))

(defun cloveynit-read-ticket-number ()
  (completing-read "Select ticket number: " (cloveynit-get-ticket-numbers)))

(defun cloveynit-get-ticket-name (ticket-number)
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

(defun cloveynit-read-repository-name (default)
  (read-string "Enter repository name: " default))

(defun cloveynit-get-new-branch-and-worktree-names ()
  (let* ((ticket-number (cloveynit-read-ticket-number))
         (ticket-name (cloveynit-get-ticket-name ticket-number))
         (kebab-name (denote-sluggify-title ticket-name))
         (default-repo (replace-regexp-in-string "\\." "-" (replace-regexp-in-string "\\.git$" "" (file-name-nondirectory (magit-get "remote" "origin" "url")))))
         (repository-name (replace-regexp-in-string "-" "_" (denote-sluggify-title (cloveynit-read-repository-name default-repo)))))
    `(,(format "feature/%s-%s" ticket-number kebab-name)
      ,(format "~/feature/%s-%s__%s" ticket-number kebab-name repository-name))))


(defun cloveynit-new-branch-and-worktree ()
  (interactive)
  (let* ((worktree (cloveynit-get-new-branch-and-worktree-names))
         (branch (car worktree))
         (path (cadr worktree))
         (starting-point (magit-read-starting-point "Create and checkout branch starting at: ")))
    (magit-worktree-branch path branch starting-point)))

(eval-after-load 'magit
  (progn
    (require 'magit)
    (require 'transient)
    (require 'denote)
    `(transient-append-suffix 'magit-worktree "c" '("f" "Feature worktree" cloveynit-new-branch-and-worktree))))

(require 'f)
(require 'dash)

(defun cloveynit-magit-status ()
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
