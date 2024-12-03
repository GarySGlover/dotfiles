(use-package server
  :commands (server-running-p)
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
  :functions split-window-max-pixels
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

(use-package emacs
  :config (setopt select-active-regions nil))

(use-package devil
  :hook (after-init . global-devil-mode)
  :functions devil-key-executor
  :defines devil-special-keys devil-mode-map
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
            ("," . "C-c z")
            (", ." . "C-c z C-")))
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
  :hook (after-init . vertico-multiform-mode)
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
  :defines corfu-map
  :config
  (setopt
   corfu-auto t
   corfu-cycle t)
  :bind (:map corfu-map
              ("RET" . nil))
  :hook (after-init . global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :hook (after-init . corfu-popupinfo-mode))

(use-package consult
  :functions consult-xref
  :init
  (setopt
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref))

(transient-define-prefix cnit/consult-dispatch ()
  "Transient for Consult commands."
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
  :hook (after-init . keycast-header-line-mode))

(use-package embark
  :commands
  (embark--truncate-target
   embark-completing-read-prompter
   embark-which-key-indicator
   embark-hide-which-key-indicator)
  :bind ("C-c z e" . embark-act)
  :config
  (defvar embark-indicators)
  (declare-function which-key--hide-popup-ignore-command "which-key")
  (declare-function which-key--show-keymap "which-key")
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the
completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  (setopt
   embark-cycle-key "."
   embark-verbose-indicator-display-action '(display-buffer-in-side-window (side . bottom))
   embark-indicators '(embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator)))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package indent-bars
  :config
  (setopt indent-bars-treesit-support t)
  :commands indent-bars-mode)

(use-package hyperbole
  :hook (after-init . hyperbole-mode))

(use-package org
  :after (elec-pair dash)
  :init
  (defun cnit/org-save-babel-tangle ()
    (add-hook 'after-save-hook
              (lambda () (when (eq major-mode 'org-mode) (org-babel-tangle)))))
  (defun cnit/org-electric-pairs ()
    (setq-local electric-pair-pairs
                (append `((?\* . ?\*)
                          (?\/ . ?\/)
                          (?\_ . ?\_)
                          (?\= . ?\=)
                          (?\+ . ?\+))
                        electric-pair-pairs)))
  :hook
  ((org-mode . cnit/org-save-babel-tangle)
   (org-mode . cnit/org-electric-pairs))
  :config
  (declare-function -each "dash")
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
  :functions denote-rename-buffer-mode
  :config
  (denote-rename-buffer-mode t)
  (setopt
   denote-directory (expand-file-name "notes/" "~/")
   denote-file-type 'org
   denote-date-prompt-use-org-read-date t)
  :hook (dired-mode . denote-dired-mode))

(transient-define-prefix cnit/denote-dispatch ()
  "Transient for Denote commands."
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

(use-package emacs
  :functions (project--files-in-directory)
  :defines (denote-directory)
  :init
  (defun cnit/find-file-in-notes ()
    "Open file from the denote notes directory."
    (interactive)
    (let* ((vc-dirs-ignores (mapcar
                             (lambda (dir)
                               (concat dir "/"))
                             vc-directory-exclusion-list))
           (file (completing-read "Note:" (project--files-in-directory denote-directory vc-dirs-ignores))))
      (when file (find-file file)))))

(use-package yasnippet
  :hook (after-init . yas-global-mode))

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
  :defines format-all-default-formatters
  :config
  (add-to-list 'format-all-default-formatters '("Nix" nixfmt))
  :hook
  ((prog-mode . format-all-mode)
   (format-all-mode . format-all-ensure-formatter)))

(use-package treesit
  :defer t
  :functions cloveynit/report-unused-ts-modes
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

(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :config
  (setopt electric-pair-open-newline-between-pairs t))

(setopt next-line-add-newlines t)

(use-package avy
  :functions (ring-ref
              cnit/avy-keys-builder
              helpful-at-point
              embark-act)
  :defines (avy-ring avy-goto-char avy-dispatch-alist)
  :commands (avy-action-copy-region
             avy-action-copy-whole-line
             avy-action-kill-whole-line
             avy-action-yank-region
             avy-action-kill-region
             avy-goto-char
             avy-process
             avy--regex-candidates
             avy-action-with-region
             avy-with)
  :config
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-new "")
      (kill-whole-line))
    (select-window (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (let ((start (move-beginning-of-line 1))
            (end (progn (move-end-of-line 1) (point))))
        (kill-new (buffer-substring-no-properties start (+ end 1)))))
    (select-window (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (yank)
    t)

  (defun avy-action-transport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (yank)
    t)

  (defun avy-action-with-region (pt action)
    (save-excursion
      (avy-with avy-goto-char
        (let ((avy-all-windows nil))
          (when-let*
              ((char2 (read-char "char: "))
               (pt2 (cdr (avy-process
                          (avy--regex-candidates
                           (regexp-quote (string char2))
                           pt)))))
            (funcall action pt pt2)))))
    (select-window (cdr (ring-ref avy-ring 1)))
    t)

  (defun avy-action-copy-region (pt)
    (avy-action-with-region pt 'copy-region-as-kill)
    t)

  (defun avy-action-yank-region (pt)
    (avy-action-copy-region pt)
    (yank)
    t)

  (defun avy-action-kill-region (pt)
    (avy-action-with-region pt 'kill-region))

  (defun avy-action-transport-region (pt)
    (avy-action-kill-region pt)
    (yank)
    t)

  (defun embark-act-region (start end)
    (goto-char end)
    (set-mark start)
    (activate-mark)
    (embark-act))

  (defun avy-action-embark-act-region (pt)
    (avy-action-with-region pt 'embark-act-region)
    t)

  (defun avy-action-embark-act (pt)
    (save-excursion
      (goto-char pt)
      (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-hyprbole (pt)
    (save-excursion
      (goto-char pt)
      (hkey-either)))

  (setq-default avy-dispatch-alist
                '((?e . avy-action-embark-act)
                  (?E . avy-action-embark-act-region)
                  (?H . avy-action-helpful)
                  (?k . avy-action-kill-whole-line)
                  (?K . avy-action-kill-region)
                  (?t . avy-action-transport-whole-line)
                  (?T . avy-action-transport-region)
                  (?w . avy-action-copy-whole-line)
                  (?W . avy-action-copy-region)
                  (?y . avy-action-yank-whole-line)
                  (?Y . avy-action-yank-region)
                  (?\r . avy-action-hyprbole)))

  (defun cnit/avy-keys-builder ()
    "Generate the `avy-keys' list.
Keys will be all from a-z excluding those used in `avy-dispatch-alist'"
    (let ((dispatch-keys (mapcar 'car avy-dispatch-alist))
          (keys))
      (dolist (char (number-sequence ?a ?z))
        (unless (member char dispatch-keys)
          (push char keys)))
      (setopt avy-keys keys)))
  (cnit/avy-keys-builder)

  :bind (("C-c z a" . avy-goto-char)
         ("C-c z A" . avy-goto-char-timer)
         ("C-c z C-a" . avy-goto-line)))

(use-package re-builder
  :commands (reb-update-regexp reb-target-value reb-quit)
  :init
  (defvar cnit/re-builder-positions nil
    "Store point and region bounds before calling `re-builder'.")
  (advice-add 're-builder
              :before
              (defun cnit/re-builder-save-state (&rest _)
                "Save into `cnit/re-builder-positions' the point and region
positions before calling `re-builder'."
                (setq cnit/re-builder-positions
                      (cons (point)
                            (when (region-active-p)
                              (list (region-beginning)
                                    (region-end)))))))

  (defun reb-replace-regexp (&optional delimited)
    "Run `query-replace-regexp' with the contents of `re-builder'.
With non-nil optinoal argument DELIMITED, only replace matches
surrounded by word boundaries."
    (interactive "P")
    (reb-update-regexp)
    (let* ((re (reb-target-value 'reb-regexp))
           (replacement (query-replace-read-to
                         re
                         (concat "Query replace"
                                 (if current-prefix-arg
                                     (if (eq current-prefix-arg '-) " backward" " word")
                                   "")
                                 " regexp"
                                 (if (with-selected-window reb-target-window
                                       (region-active-p)) " in region" ""))
                         t))
           (pnt (car cnit/re-builder-positions))
           (beg (cadr cnit/re-builder-positions))
           (end (caddr cnit/re-builder-positions)))
      (with-selected-window reb-target-window
        (goto-char pnt)
        (setq cnit/re-builder-positions nil)
        (reb-quit)
        (query-replace-regexp re replacement delimited beg end))))
  :config
  (setopt reb-re-syntax 'string)
  :bind (("C-c z s" . re-builder)
         :map reb-mode-map
         ("RET" . reb-replace-regexp)
         :map reb-lisp-mode-map
         ("RET" . reb-replace-regexp)))

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package eglot
  :functions (flymake-eldoc-function cape-wrap-buster)
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

(use-package  project
  :commands (project-forget-projects-under)
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
  :hook (after-init . direnv-mode))

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

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

(declare-function -filter "dash")
(declare-function project-files "project")

(defun clovnit/run-file (buffer)
  "Run current BUFFER.
Runs inside comint if the file is executable."
  (interactive
   (list (if (project-current)
             (completing-read "Run file: " (-filter #'file-executable-p (project-files (project-current))))
           (read-file-name "Run file: "))))
  (let* ((executable-p (and buffer (file-executable-p buffer))))
    (when executable-p (switch-to-buffer (make-comint (format "run-%s" (file-name-base buffer)) buffer)))))

(defun clovnit/run-current-file ()
  (interactive)
  (when (buffer-file-name)
    (clovnit/run-file (buffer-file-name))))

(bind-key "C-c z x" #'clovnit/run-current-file)
(bind-key "C-c z X" #'clovnit/run-file)

(defun cnit/browse-url-quesiton (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let* ((browser (read-char-choice "Browser: 'p' personal 'w' work: " '(?p ?w)))
         (browse-url-firefox-program
          (cond
           ((eq browser ?p) "firefox")
           ((eq browser ?w) "floorp"))))
    (browse-url-firefox url new-window)))

(setopt browse-url-browser-function #'cnit/browse-url-quesiton)
