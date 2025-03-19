(use-package server
	:commands (server-running-p)
	:config
	(unless (server-running-p)
		(server-start)))

(setopt visible-bell t)

(setopt confirm-kill-emacs #'yes-or-no-p)

;; -*- lexical-binding: t -*-

(use-package comp
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
(unbind-key "C-x m" 'global-map)

(use-package emacs
	:config (setopt select-active-regions nil))

;; Remapping modes to new treesitter modes.
(setopt major-mode-remap-alist
	'((sh-mode . bash-ts-mode)
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
  		 (python-mode . python-ts-mode)
  		 (ruby-mode . ruby-ts-mode)
  		 (conf-toml-mode . toml-ts-mode)
		 (html-mode . html-ts-mode)))

;; Associating filename regex lookups to major modes.
(dolist (mode-assoc
			'(("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
				  . dockerfile-ts-mode)
				 ("/go\\.mod\\'" . go-mod-ts-mode)
				 ("\\.go\\'" . go-ts-mode)
				 ("\\.nix\\'" . nix-ts-mode)
				 ("\\.rs\\'" . rust-ts-mode)
				 ("\\.ts\\'" . typescript-ts-mode)
				 ("\\.tsx\\'" . tsx-ts-mode)
				 ("\\.ya?ml\\'" . yaml-ts-mode)
				 ("\\.[hl]?eex\\'" . heex-ts-mode)
				 ("mix\\.lock" . elixir-ts-mode)
				 ("\\.exs\\'" . elixir-ts-mode)
				 ("\\.ex\\'" . elixir-ts-mode)
				 ("\\.elixir\\'" . elixir-ts-mode)
				 ("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-ts-mode)
				 ("\\.\\(?:php\\|inc\\|stub\\)\\'" . php-ts-mode)
				 ("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-ts-mode)
				 ("\\.lua\\'" . lua-ts-mode)))
	(add-to-list 'auto-mode-alist mode-assoc))

;; Language names default formatters.
(defvar cnit/languages--default-formatters-alist
	'(("Nix" nixfmt)))

;; Org language mode
(defvar cnit/languages--org-src-lang-modes
	'(("yaml" . "yaml-ts")
		 ("nix" . "nix-ts")
		 ("go" . "go-ts")))

;; Major mode default extension
(defvar cnit/major-mode--default-file-extenson
	'((emacs-lisp-mode . ".el")
		 (bash-ts-mode . ".sh")))

;; Excluded modes from ts warning
(defvar cnit/languages--excluded-ts-warning-modes
	'(sh--redirect-bash-ts-mode
		 indent-bars--ts-mode
		 yaml-pro-ts-mode))

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
		 ("fm" (lambda () (cnit/modes-highlight 'flymake-mode "Flymake"))
			 flymake-mode)
		 ("fc" (lambda () (cnit/modes-highlight 'display-fill-column-indicator-mode "Fill Column Indicator"))
			 display-fill-column-indicator-mode)
		 ("fa" (lambda () (cnit/modes-highlight 'format-all-mode "Format all"))
			 format-all-mode)
		 ("hl" (lambda () (cnit/modes-highlight 'hl-line-mode "Highlight Line"))
			 hl-line-mode)
		 ("ln" (lambda () (cnit/modes-highlight 'display-line-numbers-mode "Line Numbers"))
			 display-line-numbers-mode)
		 ("ww" (lambda () (cnit/modes-highlight 'word-wrap-whitespace-mode "Word Wrap"))
			 word-wrap-whitespace-mode)
		 ("cn" (lambda () (cnit/modes-highlight 'column-number-mode "Column Number"))
			 column-number-mode)
		 ("ar" (lambda () (cnit/modes-highlight 'auto-revert-mode "Auto Revert"))
			 auto-revert-mode)
		 ("fs" (lambda () (cnit/modes-highlight 'flyspell-mode "Flyspell"))
			 flyspell-mode)
		 ("ps" (lambda () (cnit/modes-highlight 'prettify-symbols-mode "Prettify Symbols"))
			 prettify-symbols-mode)
		 ("cp" (lambda () (cnit/modes-highlight 'copilot-mode "Copilot"))
			 copilot-mode)
		 ]
		["Indent"
			("ai" (lambda () (cnit/modes-highlight 'aggressive-indent-mode "Aggressive Indent"))
				aggressive-indent-mode)
			("ei" (lambda () (cnit/modes-highlight 'electric-indent-mode "Electric Indent"))
				electric-indent-mode)
			("it" (lambda () (cnit/modes-highlight 'indent-tabs-mode "Indent tabs"))
				indent-tabs-mode)
			("ib" (lambda () (cnit/modes-highlight 'indent-bars-mode "Indent bars"))
				indent-bars-mode)
			]
		["Whitespace"
			("wb" (lambda () (cnit/modes-highlight 'ws-butler-mode "WS Butler"))
				ws-butler-mode)
			("ws" (lambda () (cnit/modes-highlight 'whitespace-mode "Whitespace"))
				whitespace-mode)
			]
		["Parens"
			("rb" (lambda () (cnit/modes-highlight 'rainbow-mode "Rainbow"))
				rainbow-mode)
			("ep" (lambda () (cnit/modes-highlight 'electric-pair-mode "Electric Pair"))
				electric-pair-mode)
			("sp" (lambda () (cnit/modes-highlight 'show-paren-mode "Show Paren"))
				show-paren-mode)
			]])

(bind-key "C-c m" #'cnit/modes-dispatch)

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
		corfu-cycle t
		corfu-on-exact-match 'show)
	:bind (:map corfu-map
              ("RET" . nil)
              ("C-<tab>" . corfu-complete))
	:hook (after-init . global-corfu-mode))

(use-package corfu-popupinfo
	:after corfu
	:hook (after-init . corfu-popupinfo-mode))

(use-package consult
	:functions consult-xref
	:bind ("C-c c" . consult-line)
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

(use-package embark
	:commands
	(embark--truncate-target
		embark-completing-read-prompter
		embark-which-key-indicator
		embark-hide-which-key-indicator)
	:bind ("C-c e" . embark-act)
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

(use-package telephone-line
	:init
	(telephone-line-defsegment* cnit/telephone-line-magit-segment ()
		(require 'magit)
		(require 's)
		(telephone-line-raw
			(when (fboundp #'magit-get-current-branch)
				(when-let* ((max-length 20)
							   (branch (s-left max-length (magit-get-current-branch))))
					`(:propertize ,(format " %s" branch)
						 mouse-face mode-line-highlight
						 help-echo (magit-get-current-branch)
						 local-map ,(let ((map (make-sparse-keymap)))
										(define-key map [mode-line mouse-1]
                                            #'magit-status)
										map)
						 face ,face)))))
	(telephone-line-defsegment* cnit/telephone-line-buffer-name ()
		(telephone-line-raw
			(if-let* ((name (buffer-file-name))
						 (shortname (shorten-file-path name)))
				`(:propertize ,shortname
                     help-echo ,name
                     face ,face)
				(buffer-name))))
	(telephone-line-defsegment cnit/telephone-line-project-segment ()
		"Displays the project name, according to magit or project.el"
		(if (project-current)
			(propertize (cond ((stringp telephone-line-project-custom-name) telephone-line-project-custom-name)
							((cnit/magit-repo-name) (cnit/magit-repo-name))
							(file-name-nondirectory (directory-file-name (project-root (project-current)))))
                'face 'telephone-line-projectile
                'display '(raise 0.0)
                'help-echo (file-name-nondirectory (directory-file-name (project-root (project-current))))
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map
                               'mouse-1 #'project-switch-project))))
	(telephone-line-defsegment cnit/telephone-line-mode-segment ()
		"Displays tree icon if major-mode is a treesitter mode."
		(if (string-match-p "-ts-" (format "%s" major-mode))
			" %[%m%]"
			"%[%m%]"))
	(telephone-line-mode nil)
	(setq telephone-line-lhs
        '((accent . (cnit/telephone-line-mode-segment))
			 (evil . (cnit/telephone-line-project-segment))
			 (accent . (cnit/telephone-line-magit-segment
						   telephone-line-process-segment))
			 (evil . ((cnit/telephone-line-buffer-name 20))))
        telephone-line-rhs
        '((accent . (telephone-line-flymake-segment))
			 (evil . (telephone-line-airline-position-segment))
			 (accent . (telephone-line-misc-info-segment))))
	(telephone-line-mode t))

(defun shorten-file-path (file-path &optional max-length)
	"Shorten FILE-PATH according to the following rules:
1. If within a `project.el` project, remove the project root from the start.
2. If within the user's home directory, replace the home directory with `~`.
3. If the path length exceeds MAX-LENGTH (default 30), shorten directories from the beginning."
	(let* ((max-length (or max-length 10))
			  (home-dir (expand-file-name "~"))
			  (project-root (when (fboundp 'project-root)
								(ignore-errors
									(let ((project (project-current)))
										(when project
											(expand-file-name (project-root project)))))))
			  ;; Step 1: Shorten to project-relative path
			  (relative-path (if (and project-root (string-prefix-p project-root file-path))
								 (substring file-path (length project-root))
								 file-path)))
		;; Step 2: Shorten to home-relative path
		(setq relative-path
			(if (string-prefix-p home-dir relative-path)
				(concat "~" (substring relative-path (length home-dir)))
				relative-path))
		;; Step 3: Shorten further if the path exceeds max-length
		(if (<= (length relative-path) max-length)
			relative-path
			(let* ((components (split-string relative-path "/" t))
					  (lastdir (if (> (length components) 1) (nth (- (length components) 2) components) ""))
					  (filename (or (car (last components)) ""))
					  (dirs (butlast components 2))
					  (shortened-dirs (mapcar (lambda (dir) (substring dir 0 1)) dirs)))
				(concat (string-join shortened-dirs "/")
					(if shortened-dirs "/")
					lastdir
					"/"
					filename)))))

(use-package indent-bars
	:config
	(setopt indent-bars-treesit-support t)
	:commands indent-bars-mode)

(use-package winner
	:init
	(winner-mode 1))

(use-package hyperbole
	:bind (("C-M-RET" . hkey-either)
			  ("C-M-<return>" . hkey-either)
			  ("ESC <return>". hkey-either))
	:hook (after-init . hyperbole-mode))

(use-package org
	:after (elec-pair dash)
	:init
	(defun cnit/org-save-babel-tangle ()
		(add-hook 'after-save-hook
            (lambda () (when (eq major-mode 'org-mode) (org-babel-tangle)))))
	(defun cnit/exclude-electric-pair ()
		"Disable electric pair mode."
		(when electric-pair-mode (electric-pair-mode -1)))
	:hook
	((org-mode . cnit/org-save-babel-tangle)
		(org-mode . cnit/exclude-electric-pair))
	:config
	(declare-function -each "dash")
	(setopt
		org-pretty-entities t
		org-startup-indented t
		org-src-window-setup 'other-window
		org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a!)" "SCHEDULED(s@)" "HOLD(h@)" "|" "DONE(d@)" "CANCELED(c@)")))
	(modify-syntax-entry ?* "\"" org-mode-syntax-table)
	(modify-syntax-entry ?_ "\"" org-mode-syntax-table)
	(-each  cnit/languages--org-src-lang-modes (lambda (x) (add-to-list 'org-src-lang-modes x))))

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
	:config
	(setq-default yas-keymap-disable-hook (lambda ()
											  (and (frame-live-p corfu--frame)
												  (frame-visible-p corfu--frame))))
	:hook (after-init . yas-global-mode))

(use-package yasnippet-capf)

(use-package aggressive-indent
	:hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package format-all
	:defines format-all-default-formatters
	:config
	(dolist (formatter-assoc cnit/languages--default-formatters-alist)
		(add-to-list 'format-all-default-formatters formatter-assoc))
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
								 cnit/languages--excluded-ts-warning-modes)))
					(unless (or used-in-major-mode-remap-alist used-in-auto-mode-alist excluded)
						(warn "TS Mode not mapped: %s" ts-mode))))))

	:config
	(setopt
		treesit-font-lock-level 4
		treesit-extra-load-path `(,(expand-file-name "~/.config/emacs/var/tree-sitter")))
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
				   embark-act
				   hkey-either
				   eglot-current-server
				   eglot-find-declaration)
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

	(defun avy-action-mark-region (pt)
		(push-mark pt t t)
		(let ((avy-all-windows nil))
			(call-interactively 'avy-goto-char)
			(forward-char)))

	(defun avy-action-copy-region (pt)
		(kill-new "")
		(avy-action-with-region pt 'copy-region-as-kill)
		t)

	(defun avy-action-yank-region (pt)
		(avy-action-copy-region pt)
		(yank)
		t)

	(defun avy-action-kill-region (pt)
		(kill-new "")
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
			(if (eglot-current-server)
				(eglot-find-declaration)
				(helpful-at-point)))
		t)

	(defun avy-action-hyprbole (pt)
		(save-excursion
			(goto-char pt)
			(hkey-either)))

	(setq-default avy-dispatch-alist
		'((?E . avy-action-embark-act)
			 (?e . avy-action-embark-act-region)
			 (?h . avy-action-helpful)
			 (?K . avy-action-kill-whole-line)
			 (?k . avy-action-kill-region)
			 (?T . avy-action-transport-whole-line)
			 (?t . avy-action-transport-region)
			 (?W . avy-action-copy-whole-line)
			 (?w . avy-action-copy-region)
			 (?Y . avy-action-yank-whole-line)
			 (?y . avy-action-yank-region)
			 (?z . avy-action-zap-to-char)
			 (?m . avy-action-mark-region)
			 (?\r . avy-action-hyprbole)))

	(setopt
		avy-single-candidate-jump nil
		avy-all-windows 'all-frames)

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

	:bind (("C-c A" . avy-goto-char)
			  ("C-c a" . avy-goto-char-timer)
			  ("C-c C-a" . avy-goto-line)))

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
	:bind (("C-c s" . re-builder)
			  :map reb-mode-map
			  ("RET" . reb-replace-regexp)
			  :map reb-lisp-mode-map
			  ("RET" . reb-replace-regexp)))

(use-package gptel
	:commands
	(gptel
		gptel-send
		gptel-menu)
	:hook (gptel-post-stream . gptel-auto-scroll)
	:config
	(require 'gptel-org)
	(defun cnit/retrieve-anthropic-api-key ()
		"Retrieve the API key for the machine `api.anthropic.com` with login `apikey` using `auth-source-search`.
Throw a `user-error` if the key is not found."
		(let ((secret (plist-get (car (auth-source-search :max 1
										  :host "api.anthropic.com"
										  :user "apikey"
										  :require '(:secret)))
                          :secret)))
			(cond
				((null secret)
					(user-error "API key for api.anthropic.com with login apikey not found"))
				((functionp secret)
					(funcall secret))
				(t secret))))
	(setopt
		gptel-model 'claude-3-7-sonnet-20250219
		gptel-default-mode 'org-mode
		gptel-backend (gptel-make-anthropic "Claude"
						  :stream t
						  :key #'cnit/retrieve-anthropic-api-key)))

(use-package copilot
	:demand t
	:init
	(defun cnit/copilot-enable ()
		(when (and (cnit/repo-org)
				  (member (intern (cnit/repo-org)) cnit/copilot-enabled-organisations))
			(progn
				(copilot-mode 1))))
	:hook ((prog-mode yaml-ts-mode) . cnit/copilot-enable)
	:config
	(setopt copilot-indent-offset-warning-disable t)
	(defvar-keymap cnit/copilot-completion-repeat-map
		:repeat t
		"w" #'copilot-accept-completion-by-word
		"l" #'copilot-accept-completion-by-line
		"p" #'copilot-accept-completion-by-paragraph
		"f" #'copilot-next-completion
		"b" #'copilot-previous-completion)
	:bind (:map copilot-completion-map
              ("M-<tab>" . copilot-accept-completion)
              ("M-c" . copilot-accept-completion)
              ("M-w" . copilot-accept-completion-by-word)
              ("M-l" . copilot-accept-completion-by-line)
              ("M-p" . copilot-accept-completion-by-paragraph)
              ("M-f" . copilot-next-completion)
              ("M-b" . copilot-previous-completion)))

(use-package copilot-chat
	:commands (copilot-chat-transient))

(use-package emacs
	:init
	(defun cnit/llm-chat (&optional prefix)
		"Decide which LLM chat interface to use based on copilot-mode and prefix argument.

If `copilot-mode` is enabled, use `copilot-chat-transient` for interactive chatting with the Copilot LLM.
Otherwise, use `gptel-menu` for alternative LLMs.

With a prefix argument (C-u), override the decision:
- If `copilot-mode` is enabled, use `gptel-menu`.
- Otherwise, use `copilot-chat-transient`.

PREFIX: Optional argument to override the default behavior."
		(interactive "P")
		(if copilot-mode
			(if prefix (gptel-menu) (copilot-chat-transient))
			(if prefix (copilot-chat-transient) (gptel-menu))))
	:bind ("C-c l" . cnit/llm-chat))

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

(use-package eglot-booster
	:functions eglot-booster-mode
	:after eglot
	:config
	(eglot-booster-mode))

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

(use-package yaml-ts-mode
	:config
	(defcustom yaml-indent-offset 2
		"Amount of offset per level of indentation."
		:type 'integer
		:local t)
	(defun cnit/last-line-indentation-offset (&optional offset)
		"Find the nearest OFFSET rounded "
		(interactive "*")
		(if-let* ((indent (save-excursion
							  (beginning-of-line)
							  (if (re-search-backward "^[^\n]" nil t)
								  (current-indentation))))
					 (offset (or offset 1)))
			(* (truncate (/ indent offset)) offset)
			0))
	(defun cnit/yaml-ts-tab ()
		(interactive "*")
		(unless (memq this-command '(newline))
			(let* ((offset (or yaml-indent-offset 2))
					  (previous-line-offset (cnit/last-line-indentation-offset 2))
					  (max (+ previous-line-offset offset))
					  (min (max 0 (- previous-line-offset offset)))
					  (current (current-indentation)))
				(if (memq this-command '(newline-and-indent))
					(indent-line-to previous-line-offset)
					(if (>= current max)
						(indent-line-to min)
						(indent-line-to (+ offset (* (truncate (/ current offset)) offset))))))))
	(defun cnit/yaml-ts-mode-config ()
		(unless dtrt-indent-mode
			(setq-local standard-indent yaml-indent-offset))
		(setq-local indent-line-function #'cnit/yaml-ts-tab)
		(setq-local tab-width standard-indent))
	:bind (:map yaml-ts-mode-map
              ("RET" . newline-and-indent))
	:hook
	(yaml-ts-mode . cnit/yaml-ts-mode-config)
	(yaml-ts-mode . yaml-pro-ts-mode))

(use-package language-id
	:config
	(setopt language-id--definitions
		(append
			'(("Nix" nix-ts-mode)) language-id--definitions)))

(use-package compilation
	:hook (compilation-filter . ansi-color-compilation-filter))

(require 'yaml)
(require 'json)
(require 'dash)
(require 'transient-compile)

(defun cnit/transient-compile--tool-property (tool property)
	"Get PROPERTY for TOOL, suppressing errors.
This function retrieves the specified PROPERTY for the given TOOL
using `transient-compile--tool-property'.  If an error occurs during
the retrieval, it is suppressed and nil is returned instead.

Arguments:
TOOL -- The tool for which the property is being retrieved.
PROPERTY -- The property to retrieve for the specified tool.

This function temporarily overrides `user-error' to prevent it from
raising an error, ensuring that any issues encountered during the
property retrieval process do not interrupt the program flow.

Example usage:
\(let ((property (`cnit/transient-compile--tool-property'
                  \\='some-tool :some-property)))
  (if property
      (message \"Property found: %s\" property)
    (message \"Property not found or error occurred\")))

In this example, the function attempts to retrieve `:some-property'
for `some-tool'. If the property is not found or an error occurs,
nil is returned, allowing the program to handle this case gracefully."
	(cl-letf (((symbol-function 'user-error) (lambda (&rest _) nil)))
		(transient-compile--tool-property tool property)))

(defun cnit/compile--pre-commit-targets (directory)
	"Get list of targets from a .pre-commit-config.yaml file in DIRECTORY.
Targets are the id values under all hooks.
If the file does not exist, return an empty list."
	(let ((file-path (expand-file-name ".pre-commit-config.yaml" directory)))
		(if (not (file-exists-p file-path))
			'() ;; Return empty list as gaurd clause for file not existing.
			(let* ((yaml-string (with-temp-buffer
									(insert-file-contents file-path)
									(buffer-string)))
					  (pre-commit-config (yaml-parse-string yaml-string))
					  (ids '("all")))
				(cl-loop for repo across (gethash 'repos pre-commit-config) do
					(cl-loop for hook across (gethash 'hooks repo) do
						(push (gethash 'id hook) ids)))
				ids))))

(defun cnit/compile--pre-commit-command (directory target)
	"Format build command for pre-commit.
DIRECTORY not used as pre-commit always runs in project root.
TARGET is the pre-commit id to run."
	(when-let* ((executable (cnit/transient-compile--tool-property 'pre-commit :exe))
				   (target (if (string= "all" target) "" target)))
		(transient-compile--shell-join
			executable
			"run" target "--all-files")))

(defun cnit/compile--nix-command (args)
	"Run nix with ARGS in DIRECTORY."
	(when-let* ((executable (or (cnit/transient-compile--tool-property 'nix :exe) "nix"))
				   (nix-bin (executable-find executable))
				   (shell-result (shell-command (string-join `(,nix-bin ,args) " ") "*nix-command*" "*nix-error*")))
		(if (eq shell-result 0)
			(with-current-buffer "*nix-command*" (buffer-string))
			nil)))

(defun cnit/compile--nix-flake-targets (directory)
	"Get list of Nix Flake targets in DIRECTORY."
	(when-let* ((json-raw (cnit/compile--nix-command (string-join `("flake show" ,directory "--json") " ")))
				   (json-data (json-parse-string json-raw))
				   (nixos-configs (gethash "nixosConfigurations" json-data)))
		(-map (lambda (config-name) (string-join `("os "  ,config-name))) (hash-table-keys nixos-configs))))

(cnit/compile--nix-flake-targets "~/dotfiles/")

(defun cnit/compile--nix-flake-command (directory target)
	"Format build command for Nix Flake check.
DIRECTORY containes flake.nix
TARGET is the derivation to check"
	(when (string-match "\\(\\S-+\\) \\(.*\\)" target)
		(let ((type (match-string 1 target))
				 (target (match-string 2 target)))
			(cond
				((string= type "os")
					(string-join `("nix eval .#nixosConfigurations." ,target ".config.system.build.toplevel")))
				(t "")))))

(defun cnit/compile--combine-tools-matchers (&rest tools)
	"Combine matchers for multiple TOOLS.
This function retrieves the match properties for each tool given in
TOOLS.  It returns a flat list of unique match strings/functions
combining all the provided tools.
Each TOOL can be a symbol representing a tool."
	(let ((combined-matches '()))
		(dolist (tool tools)
			(let ((match (transient-compile--tool-property tool :match)))
				(cond
					((null match)
						nil)
					((listp match)
						(setq combined-matches (append combined-matches match)))
					(t
						(setq combined-matches (append combined-matches (list match)))))))
		(delete-dups combined-matches)))

(defmacro cnit/compile--combine-targets (&rest tools)
	"Create a function that combines targets for the given TOOLS."
	(let ((function-name (intern (format "cnit/compile--%s-targets" (mapconcat 'symbol-name tools "-")))))
		`(defun ,function-name (directory)
			 (cl-letf (((symbol-function 'user-error) (lambda (&rest _) nil)))
				 (append
					 ,@(mapcar (lambda (tool)
								   `(when-let* ((transient-compile-tool ',tool)
												   (tool-and-dir (funcall transient-compile-detect-function))
												   (detected-dir (cdr tool-and-dir)))
										(if (string= detected-dir directory)
											(-map (lambda (target) (format "[%s] %s" ',tool target))
												(transient-compile--tool-targets transient-compile-tool directory)))))
						   tools))))))

(defun cnit/compile--combine-tools (&rest tools)
	"Combine TOOLS for `transient-compile'.
Allows for the simultaneous discovery and dispatch of multiple tools
into one transient menu."
	(eval `(cnit/compile--combine-targets ,@tools))
	(let ((combined-name (intern (mapconcat 'symbol-name tools " & ")))
			 (target (intern (apply #'cnit/compile--combine-function-name tools))))
		`(,combined-name :match ,(apply #'cnit/compile--combine-tools-matchers tools)
			 :chdir t
			 :targets ,target
			 :command cnit/compile--combine-command)))

(defun cnit/compile--combine-function-name (&rest tools)
	"Create the target function name from the TOOLS."
	(format "cnit/compile--%s-targets"
        (mapconcat (lambda (tool) (format "%s" tool)) tools "-")))

(defun cnit/compile--combine-command (directory target)
	"Combine and execute a compile command based on the TARGET string.

DIRECTORY is the directory in which the compile command should be executed.
TARGET is a string that includes the tool and the actual target, formatted as
\"[tool] target\".  The function extracts the tool and target from this string,
retrieves the corresponding compile command, and executes it with DIRECTORY
and TARGET as arguments.

For example, if TARGET is \"[gcc] main.c\", the function will:
1. Extract \\='gcc\\=' as the tool and \\='main.c\\=' as the target.
2. Retrieve the compile command associated with \\='gcc\\='.
3. Execute the compile command with DIRECTORY and \\='main.c\\='.

The compile command is retrieved using the `transient-compile--tool-property`
function, which should return a function that accepts DIRECTORY and TARGET as
arguments."
	(when (string-match "\\[\\([^]]*\\)\\] \\(.*\\)" target)
		(let* ((tool (intern (match-string 1 target)))
				  (target (match-string 2 target))
				  (compile-command (transient-compile--tool-property tool :command)))
			(funcall compile-command directory target))))

;; Known issues:
;; Combining tools with functions as matchers fails. I think it's due to the way closures appear as a list rather than single item.

(use-package transient-compile
	:bind (("C-c b" . transient-compile))
	:config
	(add-to-list 'transient-compile-tool-alist
		'(nix :match ("flake.nix")
			 :exe "nix"
			 :chdir t
			 :targets cnit/compile--nix-flake-targets
			 :command cnit/compile--nix-flake-command))
	(add-to-list 'transient-compile-tool-alist
		'(pre-commit :match (".pre-commit-config.yaml")
			 :exe "pre-commit"
			 :chdir t
			 :targets cnit/compile--pre-commit-targets
			 :command cnit/compile--pre-commit-command))
	(add-to-list 'transient-compile-tool-alist (cnit/compile--combine-tools 'nix 'pre-commit 'make)))

(use-package magit
	:demand t
	:commands (magit-get-current-branch)
	:bind (("C-c g" . magit-dispatch)
			  ("C-c G" . cnit/magit-status)))

(use-package  project
	:commands (project-forget-projects-under)
	:config
	(project-forget-projects-under "~/git-clones" t)
	(project-forget-zombie-projects))

(use-package disproject
	:config
	(setopt
		disproject-shell-command #'project-shell)
	:bind (:map ctl-x-map
			  ("p" . disproject-dispatch)))

(use-package direnv
	:config (setopt direnv-always-show-summary nil)
	:hook (after-init . direnv-mode))

(use-package editorconfig
	:init
	(defun cnit/org-src-editorconfig ()
		(when-let* ((ext (alist-get major-mode cnit/major-mode--default-file-extenson))
					   (buffer-file-name (concat default-directory (make-temp-name "") (or ext ""))))
			(editorconfig-mode-apply)))
	:hook ((after-init . editorconfig-mode)
			  (org-src-mode . cnit/org-src-editorconfig)))

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

(use-package dired
	:config
	(setopt dired-dwim-target t))

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
	(setopt aw-keys '(?i ?s ?r ?t ?g ?p ?n ?e ?a ?o))
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
	(when-let* ((repo-name (magit-get "remote" "origin" "url"))
				   (git-removed (replace-regexp-in-string "\\.git$" "" (file-name-nondirectory repo-name))))
		(replace-regexp-in-string "\\." "-" git-removed)))

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
(declare-function project-name "project")

(defun clovnit/run-file (buffer &optional prefix)
	"Run current BUFFER.
Runs inside comint if the file is executable.
If PREFIX provided then clear old result from buffer first"
	(interactive
		(list (if (project-current)
				  (completing-read "Run file: " (-filter #'file-executable-p (project-files (project-current))))
				  (read-file-name "Run file: "))
			current-prefix-arg))
	(let* ((executable-p (and buffer (file-executable-p buffer)))
			  (run-buffer-name (if (project-current)
								   (format "run-%s-[%s]" (file-name-base buffer) (project-name (project-current)))
								   (format "run-%s" (file-name-base buffer))))
			  (run-buffer-full-name (format "*%s*" run-buffer-name)))
		(when executable-p
			(when (and prefix (get-buffer run-buffer-full-name))
				(kill-buffer run-buffer-full-name))
			(switch-to-buffer (make-comint run-buffer-name buffer)))))

(defun clovnit/run-current-file ()
	(interactive)
	(when (buffer-file-name)
		(clovnit/run-file (buffer-file-name))))

(bind-key "C-c x" #'clovnit/run-current-file)
(bind-key "C-c X" #'clovnit/run-file)

(defun cnit/browse-url-quesiton (url &optional new-window)
	(interactive (browse-url-interactive-arg "URL: "))
	(let* ((browser (read-char-choice "Browser: 'p' personal 'w' work: " '(?p ?w)))
			  (browse-url-firefox-program
				  (cond
					  ((eq browser ?p) "firefox")
					  ((eq browser ?w) "floorp"))))
		(browse-url-firefox url new-window)))

(setopt browse-url-browser-function #'cnit/browse-url-quesiton)

(defvar-keymap cnit/navigation-repeat-map
	:repeat t
	"n" #'next-line
	"p" #'previous-line
	"f" #'forward-char
	"b" #'backward-char
	"a" #'move-beginning-of-line
	"e" #'move-end-of-line
	"v" #'scroll-up-command)

(defvar-keymap cnit/alt-navigation-repeat-map
	:repeat t
	"f" #'forward-word
	"b" #'backward-word
	"v" #'scroll-down-command)

(defvar-keymap cnit/undo-repeat-map
	:repeat t
	"/" #'undo)

(defvar-keymap cnit/kill-repeat-map
	:repeat t
	"k" #'kill-line)

(defvar-keymap cnit/org-kill-repeat-map
	:repeat t
	"k" #'org-kill-line)

(defvar-keymap cnit/delete-char-repeat-map
	:repeat t
	"d" #'delete-char)

(defvar-keymap cnit/recenter-top-bottom
	:repeat t
	"l" #'recenter-top-bottom)

(require 'magit)
(require 'project)

(defun cnit/repo-org (&optional path)
	"Return the organisaton of the repo or nil if no org.
Use current directory if PATH not provided."
	(interactive)
	(when-let* ((path (or path (and (project-current) (project-root (project-current)))))
				   (default-directory (if (file-directory-p path) path (file-name-directory path)))
				   (origin (magit-get "remote.origin.url")))
		(cond ((string-match "dev.azure.com[^/]+/\\([^/]+\\)" origin) (match-string 1 origin))
			((string-match "github.com:\\([^/]+\\)" origin) (match-string 1 origin)))))
