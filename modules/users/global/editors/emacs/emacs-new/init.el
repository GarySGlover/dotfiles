;;; Package --- init file  -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
;; Basic Emacs Settings
;; Start emacs server after when there is spare time.

(defun cnit-emacs-server ()
  "Start Emacs server if not already running."
  (unless (and (fboundp 'server-running-p) (server-running-p))
    (setopt server-name "emacs-new")
    (server-start)))
(run-with-idle-timer 5 nil #'cnit-emacs-server)


;; Short responses rather than the longer yes/no.

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))


;; Increase undo limit to help ensure not running out of undo information.

(setopt
 undo-limit (* 13 160000)
 undo-strong-limit (* 13 240000)
 undo-outer-limit (* 13 24000000))


;; Recursive minibuffers allows minibuffer commands in the minibuffer.

(setopt enable-recursive-minibuffers t)


;; Disable bell functionality.

(setopt
 visible-bell nil
 ring-bell-function #'ignore)


;; Default command filtering for ~execute-extended-command~. As the
;; filtering can be useful for command discovery I have setup key binds
;; for all the available filtering options.

(setopt read-extended-command-predicate
        #'command-completion-default-include-p)

(defun cnit-command-execute-extended-command (prefixarg)
  "Execute extended command with PREFIXARG w using default completion."
  (interactive "P")
  (let ((read-extended-command-predicate nil))
    (with-no-warnings (execute-extended-command prefixarg))))

(defun cnit-command-execute-extended-command-using-mode (prefixarg)
  "Execute extended command with PREFIXARG using modes for completion."
  (interactive "P")
  (let ((read-extended-command-predicate
         #'command-completion-using-modes-p))
    (with-no-warnings (execute-extended-command prefixarg))))

(defun cnit-command-execute-extended-command-using-modes-and-keymaps
    (prefixarg)
  "Execute extended command with PREFIXARG using modes and keymaps for completion."
  (interactive "P")
  (let ((read-extended-command-predicate
         #'command-completion-using-modes-and-keymaps-p))
    (with-no-warnings (execute-extended-command prefixarg))))

(defun cnit-command-execute-extended-command-default (prefixarg)
  "Execute extended command with PREFIXARG using default completion."
  (interactive "P")
  (let ((read-extended-command-predicate
         #'command-completion-default-include-p))
    (with-no-warnings (execute-extended-command prefixarg))))

(bind-key
 "s-x m"
 #'cnit-command-execute-extended-command-using-mode
 global-map)
(bind-key
 "s-x k"
 #'cnit-command-execute-extended-command-using-modes-and-keymaps
 global-map)
(bind-key
 "s-x d" #'cnit-command-execute-extended-command-default global-map)
(bind-key "s-x x" #'cnit-command-execute-extended-command global-map)


;; Which key for showing menu of keybinds.

(run-with-idle-timer 10 nil #'require 'which-key)
(keymap-set help-map "C-h" #'which-key-C-h-dispatch) ; Fix for which key in the help map. Otherwise C-h would run help for help.
(keymap-set help-map "M-t" #'which-key-show-top-level)
(keymap-set help-map "M-m" #'which-key-show-major-mode)
(keymap-set help-map "M-M" #'which-key-show-full-major-mode)
(keymap-set help-map "M-k" #'which-key-show-keymap)
(keymap-set help-map "M-K" #'which-key-show-full-keymap)
(keymap-set help-map "M-n" #'which-key-show-minor-mode-keymap)
(keymap-set help-map "M-N" #'which-key-show-full-minor-mode-keymap)
(with-eval-after-load 'which-key
  (setopt
   which-key-show-early-on-C-h t
   which-key-idle-delay 10000.0
   which-key-idle-secondary-delay 0.05
   which-key-sort-order 'which-key-local-then-key-order)
  (set-face-attribute 'which-key-local-map-description-face nil
                      :weight 'bold)
  (which-key-mode t))


;; Improve the inbuilt help with extra contextual information

(bind-key [remap describe-function] #'helpful-callable)
(bind-key [remap describe-command] #'helpful-command)
(bind-key [remap describe-variable] #'helpful-variable)
(bind-key [remap describe-key] #'helpful-key)
(bind-key "C-c C-d" #'helpful-at-point 'global-map)


;; Buffer display rules to organise new buffers opening where I want them
;; to.

(defun cnit-fit-window-to-buffer-with-max (window)
  (let ((fit-window-to-buffer-horizontally t))
    (fit-window-to-buffer
     window (floor (frame-height) 3) 0 (floor (frame-width) 3) 0)))

(defconst cnit-regex-buffers-occur (rx "*occur*"))
(defconst cnit-regex-buffers-helpful (rx "*helpful" (1+ nonl) "*"))

(bind-key "C-`" #'popper-toggle)
(bind-key "M-`" #'popper-cycle)
(bind-key "C-M-`" #'popper-toggle-type)
(with-eval-after-load 'popper
  (setopt
   popper-display-control nil
   popper-group-function #'popper-group-by-project
   popper-mode-line nil
   popper-reference-buffers
   `(,cnit-regex-buffers-occur
     occur-mode ,cnit-regex-buffers-helpful helpful-mode))
  (popper-echo-mode 1))
(add-hook 'after-init-hook (lambda () (popper-mode t)))

(setq display-buffer-alist
      `(((or .
             (,cnit-regex-buffers-occur (derived-mode-p 'occur-mode)))
         (display-buffer-in-side-window)
         (window-height . cnit-fit-window-to-buffer-with-max)
         (side . bottom)
         (dedictated . t)
         (body-function . select-window)
         (window-parameters (no-delete-other-windows . t)))
        ((or .
             (,cnit-regex-buffers-helpful
              (derived-mode-p 'helpful-mode)))
         (display-buffer-in-side-window)
         (window-width . cnit-fit-window-to-buffer-with-max)
         (side . right)
         (dedicated . t)
         (body-function . select-window)
         (window-parameters (no-delete-other-windows . t)))))


;; Window splitting. Prefer to use the longest dimension for splitting,
;; this ensures the splitting uses the direction with the most available
;; display space.

(setopt split-window-preferred-direction 'longest)


;; Window layout history

(add-hook 'after-init-hook #'winner-mode)
;; Theme

(defun cnit-pre-load-theme (_theme)
  "Disable any loaded themes before enabling a new THEME.
This prevents overlapping themes; something I would rarely want."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(advice-add 'load-theme :before #'cnit-pre-load-theme)

(defun cnit-load-system-theme ()
  "Load a theme based on the system color scheme."
  (when (fboundp 'cnit-with-system-colour-scheme)
    (cnit-with-system-colour-scheme
     (load-theme 'modus-vivendi) (load-theme 'modus-operandi))))

(add-hook 'emacs-startup-hook #'cnit-load-system-theme)
;; Org Mode
;; Most packages won't get there own section. However org mode and it's
;; various extensions are a significant part of the Emacs experience.

;; Apply automatic line breaking when inserting a space at a column.

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'auto-fill-mode))


;; Automatically tangle files on save, this helps with literate
;; code/configs to generate the resulting files.

(defun cnit-org-after-save-babel-tangle ()
  "Automatically tangle org files when in 'org-mode'."
  (when (eq major-mode 'org-mode)
    (org-babel-tangle)))
(with-eval-after-load 'org
  (add-hook 'after-save-hook #'cnit-org-after-save-babel-tangle))


;; Link the org src commands to help with automated code related
;; tasks that I want to be able to run prior to the save and exit.

(with-eval-after-load 'org-src
  (defun cnit-org-edit-src-exit (orig &rest r)
    "Run before save hooks on save."
    ;;(run-hooks 'before-save-hook) ; BUG - Can't exit src code blocks without working formatter, impacts pressing return in src blocks.
    (apply orig r))
  (defun cnit-org-edit-src-save (orig &rest r)
    "Run before save hooks on save."
    (with-demoted-errors "Error running `before-save-hook': %S"
      (run-hooks 'before-save-hook))
    (apply orig r))
  (advice-add #'org-edit-src-save :around #'cnit-org-edit-src-save)
  (advice-add #'org-edit-src-exit :around #'cnit-org-edit-src-exit))
;; Editing
;; Tools and enhancements to make editing more efficient and
;; precise. Focuses on improving readability, providing structural
;; guidance, and automating routine tasks.

;; Automatic formatting of buffers on save. Need to make sure formatter
;; is available where possible. Should account for org src
;; buffers, for both save and exit.

(with-eval-after-load 'format-all
  (defun cnit-format-all-buffer-hook ()
    (when (and org-src-mode
               (derived-mode-p 'lisp-data-mode)
               format-all-mode)
      (format-all-buffer)))
     (add-hook 'before-save-hook 'cnit-format-all-buffer-hook)
  (advice-add
   'format-all-buffer
   :before (lambda (&rest _r) (format-all-ensure-formatter) t))
  (advice-add
   'format-all-region
   :before (lambda (&rest _r) (format-all-ensure-formatter) t))
  (advice-add
   'format-all-region-or-buffer
   :before (lambda (&rest _r) (format-all-ensure-formatter) t)))


;; Highlight unmatched closing delimiters only in the error face
;; foreground colour. This will help with editing to highlight any
;; incorrectly closed blocks.

(with-eval-after-load 'rainbow-delimiters
  (setopt rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))


;; Ediff is used for easy working with buffer differences. By default it
;; uses a second frame for the control panel, but that doesn't work well
;; with tiling window managers.

(with-eval-after-load 'ediff-wind
  (winner-mode t)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  (setopt
   ediff-window-setup-function #'ediff-setup-windows-plain
   ediff-split-window-function #'split-window-sensibly))
;; Project
;; This section covers managing projects, whether version-controlled or
;; not. It provides tools for navigating, planning, and tracking work
;; across different contexts, from codebases to general tasks. The goal
;; is to keep all project-related activities organized and consistent.

;; Bindings for managing projects, these are typically git based projects
;; as I don't use other vc systems.

(bind-key "s-p" project-prefix-map)
(keymap-set project-prefix-map "v" #'magit-project-status)
(with-eval-after-load 'project
  (setopt
   project-switch-use-entire-map t
   project-mode-line nil))


;; Project memoization performance fix for the project modeline when the
;; file is not in a project. Refer to Emacs bug for the reason I have
;; this here
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-05/msg01039.html.

(defun cnit-memoize-project-current (orig &optional prompt directory)
  (if (boundp 'memoize-project-current--cache)
      memoize-project-current--cache
    (set
     (make-local-variable 'memoize-project-current--cache)
     (funcall orig prompt directory))))
(defun cnit-project-mode-line-format ()
  "Memoize 'project-current' for mode line performance."
  (let ((original-project-current (symbol-function 'project-current)))
        (unwind-protect
         (progn
           (fset 'project-current
                 (lambda (&optional prompt directory)
                   (cnit-memoize-project-current original-project-current
                                                 prompt
                                                 directory)))
           (project-mode-line-format))
         (fset 'project-current original-project-current))))
(advice-add 'project-mode-line-format :override #'cnit-project-mode-line-format)


;; Forget zombie projects and import projects in standard project
;; directories. This should happen automaticaly on a project switch
;; command.

(with-eval-after-load 'project
  (defvar cnit-project-base-directories '("~/feature/" "~/dev/"))
  "List of directories containing vc controlled subdirectories.")
(defun cnit-project-prompter-advice ()
  "Update remembered projects."
  (project-forget-zombie-projects)
  (let ((inhibit-message t))
    (mapc
     (lambda (dir) (project-remember-projects-under dir))
     (flatten-list
      (mapcar
       (lambda (dir)
         (seq-filter
          (lambda (x) (file-directory-p x))
          (directory-files dir t "^[^.].*")))
       cnit-project-base-directories)))))
(advice-add
 'project-prompt-project-dir
 :before 'cnit-project-prompter-advice)


;; Cleanup any projects that are inside other projects. This is to help
;; cleanup modules that are downloaded as part of builds, tests and
;; pre-commits.

(defun cnit/project-prune-nested-projects ()
  "Remove projects from `project--list` that are nested inside other projects.
Keeps only the topmost project directories.

This filters `project--list` in place and writes the updated list to disk."
  (interactive)
  (require 'seq)
  (project--ensure-read-project-list)
  (let* ((projects (mapcar #'car project--list))
         (sorted (seq-sort #'string-lessp projects))
         (pruned '())
         (last-root nil))
    ;; Go linearly through sorted list; skip any path that’s under the last kept root
    (dolist (proj sorted)
      (unless (and last-root
                   (string-prefix-p
                    (file-name-as-directory last-root)
                    (file-name-as-directory proj)
                    (file-name-case-insensitive-p proj)))
        (push proj pruned)
        (setq last-root proj)))

    ;; Keep only entries whose root is in pruned list
    (setq project--list
          (seq-filter
           (lambda (entry) (member (car entry) pruned))
           project--list))

    (project--write-project-list)
    (message "Pruned nested projects; %d remain"
             (length project--list))
    (length project--list)))


;; Automatically configure development environment dependencies when
;; entering a project. Using direnv as the main utility.

(add-hook 'after-init-hook 'envrc-global-mode 91)
(with-eval-after-load 'envrc
  (setopt envrc-show-summary-in-minibuffer nil)
  (define-key envrc-mode-map (kbd "s-e") 'envrc-command-map))


;; - Magit workspaces.
;;   Add ticket number, but don't force
;;   Auto generate from branch with name scheme
;;   Branch with title, lower snake cased


;; This is being prepped in magit-worktrees.el and will be ported
;; here when completed.
;; Completions
;; This section will include all completion types and options. This
;; includes, but is not limited to, programming, textual, shell and LLM
;; completions.

;; Improve on the inbuilt completions. Display multiple candidates to
;; make selections easier.

(add-hook 'after-init-hook #'vertico-mode)
(with-eval-after-load 'vertico
  (setopt vertico-cycle t))


;; Improved filering of completion candidates. Helps to find things
;; easier using different methods such as out of order regexps.

(setopt completion-styles '(orderless basic)
	completion-category-overrides '((file (styles basic partial-completion))))


;; Change dabbrev to hippie expand for better completion capabilities.

(global-set-key [remap dabbrev-expand] 'hippie-expand)


;; Completion in region enhancements. Show in popup by point and allow
;; for selection using standard movement keys.

(autoload #'corfu--in-region "corfu")
(setopt completion-in-region-function 'corfu--in-region)
(with-eval-after-load 'corfu
  (setopt
   corfu-cycle t))


;; Template package configuration.

(defun tempel-setup-capf ()
  "Add the Tempel Capf to `completion-at-point-functions'"
  (setq-local completion-at-point-functions
              (cons #'tempel-expand completion-at-point-functions)))

(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

;; (with-eval-after-load 'tempel
;;   (setopt tempel-trigger-prefix "<"))


;; Minibuffer completions annotations. Applies useful extended details
;; such as documentation, values, file details etc.

(add-hook 'after-init-hook (lambda () (marginalia-mode)))
(with-eval-after-load 'marginalia
  (bind-key "M-a" #'marginalia-cycle 'minibuffer-mode-map))


;; Aider integration. Integration of Aider for project aware LLM work.

;; https://aider.chat/docs/llms/github.html
;; https://github.com/MatthewZMD/aidermacs
;; BUG: model is hardcoded to github copilot, however I would like the dynamic models.
;;      this may get fixed in future aider and aidermacs variants. For now I also need
;;      to create the file .aider.config.yaml and .aider.model.settings.yml according
;;      to the to this part of an aider issue:
;;      https://github.com/Aider-AI/aider/issues/2227#issuecomment-3141551921

(bind-key "s-a" #'aidermacs-transient-menu)
;; Copilot config
(with-eval-after-load 'aidermacs
  (setopt
   aidermacs-default-chat-mode 'ask
   aidermacs-extra-args '("--model" "github_copilot/gpt-4.1" "--no-show-model-warnings")
   aidermacs-default-model "github_copilot/gpt-4.1"))
;; Ollama config. Struggles with memory locally
;; (with-eval-after-load 'aidermacs
;;   (setopt
;;    aidermacs-extra-args '("--model" "ollama_chat/codellama:7b")
;;    aidermacs-default-model "ollama_chat/codellama:7b"
;;    aidermacs-weak-model "ollama_chat/mistral:7b"
;;    aidermacs-architect-model "ollama_chat/deepseek-r1:8b"))
;; Prog mode
;; Enable supportive modes for programming.

(add-hook
 'prog-mode-hook
 (lambda ()
   (format-all-mode t)
   (rainbow-delimiters-mode t)))
;; Emacs Lisp
;; Indentation configuration

(with-eval-after-load 'emacs-mode
  (add-hook
   'emacs-lisp-mode
   (Lambda
    ()
    (setopt
     indent-tabs-mode nil
     lisp-indent-function nil
     lisp-indent-offset 2))))


;; Automatic formatting of Elisp. Setup formatter for Emacs lisp to be
;; the default.

(with-eval-after-load 'format-all
  (define-format-all-formatter
   elisp-autofmt
   (:executable)
   (:install)
   (:languages "Emacs Lisp")
   (:features region)
   (:format
    (format-all--buffer-native
     'emacs-lisp-mode
     (if region
         (lambda () (elisp-autofmt-region (car region) (cdr region)))
       #'elisp-autofmt-buffer))))
  (cnit-update-format-all-formatter "Emacs Lisp" 'elisp-autofmt))


;; Check for balanced parameters prior to allowing save.

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'lisp-data-mode)
              (check-parens)))
          -90)
;; Nix

(add-to-list 'auto-mode-alist `(,(rx ".nix" string-end) . nix-ts-mode))
(cnit-update-format-all-formatter "Nix" 'nixfmt)
(provide 'init)

;;; init.el ends here
