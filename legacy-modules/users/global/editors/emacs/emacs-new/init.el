;;; Package --- init file  -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
;; Basic Emacs Settings
;; Load local packages autoloads

(load "local-packages-autoloads")


;; Start emacs server after when there is spare time.

(defun cnit-emacs-server ()
  "Start Emacs server if not already running."
  (unless (and (fboundp 'server-running-p) (server-running-p))
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
 "C-c x m"
 #'cnit-command-execute-extended-command-using-mode
 global-map)
(bind-key
 "C-c x k"
 #'cnit-command-execute-extended-command-using-modes-and-keymaps
 global-map)
(bind-key
 "C-c x d" #'cnit-command-execute-extended-command-default global-map)
(bind-key "C-c x x" #'cnit-command-execute-extended-command global-map)


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

(defconst cnit-regex-buffers-occur (rx "*Occur*"))
(defconst cnit-regex-buffers-helpful (rx "*helpful" (1+ nonl) "*"))
(defconst cnit-regex-buffers-info (rx "*info*"))

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

(setopt display-buffer-alist nil)

(add-to-list
 'display-buffer-alist
 `((or . (,cnit-regex-buffers-occur (derived-mode-p 'occur-mode)))
   (display-buffer-in-side-window)
   (window-height . cnit-fit-window-to-buffer-with-max)
   (side . bottom)
   (dedicated . t)
   (body-function . select-window)
   (window-parameters (no-delete-other-windows . t))))

(add-to-list
 'display-buffer-alist
 `((or .
       (,cnit-regex-buffers-helpful
        (derived-mode-p 'helpful-mode)
        ,cnit-regex-buffers-info
        (derived-mode-p 'Info-mode)))
   (display-buffer-in-side-window)
   (window-width . cnit-fit-window-to-buffer-with-max)
   (side . right)
   (dedicated . t)
   (body-function . select-window)
   (window-parameters (no-delete-other-windows . t))))

;; Magit buffers will prefer to reuse an existing window displaying
;; magit. Will use the current window if magit not showing anywhere.
(with-eval-after-load 'magit
  (setopt magit-display-buffer-function #'display-buffer))
(add-to-list
 'display-buffer-alist
 `((derived-mode . magit-mode)
   (display-buffer-reuse-mode-window display-buffer-same-window)
   (inhibit-same-window . nil)))


;; Window splitting. Prefer to use the longest dimension for splitting,
;; this ensures the splitting uses the direction with the most available
;; display space.

(setopt split-window-preferred-direction 'longest)


;; Window layout history

(add-hook 'after-init-hook (lambda () (winner-mode t)))


;; Enable repeat mode. Can help with actions in key sequences that may
;; need to be repeated.

(add-hook 'after-init-hook (lambda () (repeat-mode t)))


;; Prevent accidental closing of Emacs

(setopt confirm-kill-emacs #'y-or-n-p)


;; Delete selection mode for automatically deleting selected region when
;; performing editing operations.

(add-hook 'after-init-hook (lambda () (delete-selection-mode 1)))


;; Tabs. Sometimes a programming language might use tabs, better to turn
;; on for those languages specifically as otherwise it'll screw up most
;; others.

(setq-default indent-tabs-mode nil)
;; Theme

(advice-add
 'load-theme
 :before
 (defun cnit-pre-load-theme (&rest _args)
   "Disable any loaded themes before enabling a new THEME.
This prevents overlapping themes; something I would rarely want."
   (dolist (theme custom-enabled-themes)
     (disable-theme theme))))

(defun cnit-load-system-theme ()
  "Load a theme based on the system color scheme."
  (require 'standard-themes)
  (when (fboundp 'cnit-with-system-colour-scheme)
    (cnit-with-system-colour-scheme
     (load-theme 'standard-dark-tinted t) (load-theme 'standard-light-tinted t))))

(add-hook 'emacs-startup-hook #'cnit-load-system-theme)
;; Productivity
;; Configuration for tools that enhance productivity, navigation, and
;; overall workflow in Emacs.

;; Navigate quickly around the visiable frames.

(bind-key "M-j" #'avy-goto-char-timer)
(bind-key "M-j" #'avy-isearch isearch-mode-map)
(with-eval-after-load 'avy
  (setopt avy-style 'at-full
	  avy-single-candidate-jump nil))


;; Embark for acting upon objects.

(bind-key "C-." #'embark-act)

;; Embark based help
(setq prefix-help-command #'embark-prefix-help-command)
(bind-key "C-h b" #'embark-bindings)
(with-eval-after-load 'embark
  (keymap-set embark-general-map "g" #'gptel-add)
  (keymap-set embark-general-map "?" #'gptel-quick))
(with-eval-after-load 'vertico-multiform
  (add-to-list
   'vertico-multiform-categories '(embark-keybinding grid)))

(with-eval-after-load 'avy
  (setf (alist-get ?. avy-dispatch-alist)
        (defun avy-action-embark (pt)
          (unwind-protect
              (save-excursion
                (goto-char pt)
                (embark-act))
            (select-window (cdr (ring-ref avy-ring 0))))
          t)))
;; Org mode
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


;; Org agenda basic settings. Configuration of location and files.

(with-eval-after-load 'org
  (setopt
   org-directory "~/nook/agenda"
   org-agenda-files '("inbox.org" "main.org")
   org-refile-targets '(("main.org" :maxlevel . 3)))
  (advice-add
   'org-refile
   :after (lambda (&rest _) (org-save-all-org-buffers))))


;; Org capture

(keymap-set global-map "C-c c" #'org-capture)
(with-eval-after-load 'org-capture
  (defun cnit-org-current-parent-target ()
    "Return a refile target pointing to the current heading."
    (org-back-to-heading t)
    ;; Return a cons of (heading . buffer)
    (let
        ((heading (nth 4 (org-heading-components))) ; get heading title
         (buf (current-buffer)))
      (list (list heading buf))))

  (setopt
   org-capture-templates
   '(("i"
      "Inbox"
      entry
      (file+headline "~/nook/agenda/inbox.org" "Inbox")
      "* TODO %? [/] [%]\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
     ("s"
      "Subtask"
      entry
      (function cnit-org-current-parent-target)
      "* TODO %? [/][%]\n:PROPERTIES:\n:CREATED: %U\n:END:\n"))))


;; Org agenda views

(bind-key "C-c o" #'org-agenda)
(with-eval-after-load 'org-agenda
  (setopt org-agenda-custom-commands
          '(("r"
             "Review"
             tags-todo
             "+inbox"
             ((org-agenda-overriding-header "Inbox")))
            ("m" "Main tasks"
             ((todo
               ""
               ((org-agenda-files '("main.org"))
                (org-agenda-overriding-header "Main Tasks"))))))))


;; Org babel for evaluating code blocks.

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp .t) (eshell . t) (shell . t))))
;; Editing
;; Tools and enhancements to make editing more efficient and
;; precise. Focuses on improving readability, providing structural
;; guidance, and automating routine tasks.

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


;; Ansi color dwim. Will apply to region if select, otherwise whole
;; buffer. Run with prefix to preserve sequences rather than remove.

(defun cnit-ansi-color-dwim (&optional arg)
  "Apply ansi-color to region if active, else to whole buffer.
With prefix ARG, preserve color sequences (don't remove them)."
  (interactive "P")
  (let* ((beg
          (if (use-region-p)
              (region-beginning)
            (point-min)))
         (end
          (if (use-region-p)
              (region-end)
            (point-max))))
    (if arg
        (ansi-color-apply-on-region beg end)
      (ansi-color-apply-on-region beg end t))))

(defvar buffer-quick-edits-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'cnit-ansi-color-dwim)
    map)
  "Keymap for buffer quick edits.")

(bind-key "C-c b" buffer-quick-edits-prefix-map)
;; Project
;; This section covers managing projects, whether version-controlled or
;; not. It provides tools for navigating, planning, and tracking work
;; across different contexts, from codebases to general tasks. The goal
;; is to keep all project-related activities organized and consistent.

;; Bindings for managing projects, these are typically git based projects
;; as I don't use other vc systems.

(advice-add
 #'magit-project-status
 :before
 (defun cnit-magit-project-status ()
   (require 'project)))
(keymap-set project-prefix-map "v" #'magit-project-status)
(with-eval-after-load 'project
  (setopt
   project-switch-use-entire-map t
   project-mode-line nil))


;; Project memoization performance fix for the project modeline when the
;; file is not in a project. Refer to Emacs bug for the reason I have
;; this here
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-05/msg01039.html.

(with-eval-after-load 'project
  (defun cnit-memoize-project-current
      (orig &optional prompt directory)
    (if (boundp 'memoize-project-current--cache)
        memoize-project-current--cache
      (set
       (make-local-variable 'memoize-project-current--cache)
       (funcall orig prompt directory))))
  (defun cnit-project-mode-line-format ()
    "Memoize 'project-current' for mode line performance."
    (let ((original-project-current
           (symbol-function 'project-current)))
      (unwind-protect
          (progn
            (fset 'project-current
                  (lambda (&optional prompt directory)
                    (cnit-memoize-project-current
                     original-project-current prompt directory)))
            (project-mode-line-format))
        (fset 'project-current original-project-current))))
  (advice-add
   'project-mode-line-format
   :override #'cnit-project-mode-line-format))


;; Forget zombie projects and import projects in standard project
;; directories. This should happen automaticaly on a project switch
;; command.

(defun cnit-project-remember-default-projects ()
  (interactive)
  (require 'project)
  (cnit-project-prompt-dir-advice)
  (message "Projects updated."))
(bind-key "C-c p" #'cnit-project-remember-default-projects)
(with-eval-after-load 'project
  (require 'dash)
  (defvar cnit-project-base-directories
    '("~/feature/"
      "~/dev/"
      "~/worktrees"
      "~/git-clones"
      "~/dotfiles"
      "~/nook")
    "List of directories containing vc controlled subdirectories.")
  (defun cnit-remember-projects-under-recursive (dir)
    "Remember projects under DIR recursively.
End recursion at the first folder that is a project."
    (if (project--find-in-directory dir)
        (project-remember-projects-under dir)
      (-each
       (-filter
        #'file-directory-p
        (directory-files dir
                         t
                         (rx
                          string-start (not ".") (one-or-more any))))
       #'cnit-remember-projects-under-recursive)))
  (defun cnit-project-prompt-dir-advice ()
    "Discover projects in base directories."
    (let ((inhibit-message t))
      (-each
       (-filter
        #'file-directory-p cnit-project-base-directories)
       #'cnit-remember-projects-under-recursive))
    (advice-remove
     'project-prompt-project-dir #'cnit-project-prompt-dir-advice))
  (advice-add
   'project-prompt-project-dir
   :before #'cnit-project-prompt-dir-advice)
  (advice-add
   'project-prompt-project-dir
   :before #'project-forget-zombie-projects))


;; Basig magit settings.

(with-eval-after-load 'magit
  (setopt
   magit-commit-show-diff nil
   magit-process-popup-time 5))

;; Magit worktree enhancements. Creation of worktree in standard folder
;; using branch naming scheme.

(autoload 'cnit-magit-worktree-checkout-existing "magit-worktrees"
  nil
  t)
(autoload 'cnit-magit-worktree-chekout-new "magit-worktrees" nil t)
(with-eval-after-load 'magit
  (transient-append-suffix
   'magit-worktree "c"
   '("f" "from branch" cnit-magit-worktree-checkout-existing))
  (transient-append-suffix
   'magit-worktree "f"
   '("n" "new branch" cnit-magit-worktree-chekout-new)))


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

(add-hook 'after-init-hook (lambda () (envrc-global-mode t)) 91)
(with-eval-after-load 'envrc
  (setopt envrc-show-summary-in-minibuffer nil)
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))


;; Allow safe directories list for dir-locals to prevent being asked
;; repeatedly.

(defvar cnit-trusted-dir-locals
  '("~/nook/" "~/projects/work/" "~/projects/personal/")
  "List of directories whose .dir-locals.el files are fully trusted.")

(defun cnit-trusted-dir-p (dir)
  "Return non-nil if DIR is in or under one of `cnit-trusted-dir-locals`."
  (let ((dir (expand-file-name dir)))
    (seq-some
     (lambda (trusted)
       (string-prefix-p (expand-file-name trusted) dir))
     cnit-trusted-dir-locals)))

(defun cnit-hack-local
    (orig-fun all-vars unsafe-vars risky-vars dir-name)
  (if (cnit-trusted-dir-p dir-name)
      t ;; Automatically trust everything
    (apply orig-fun
           variables
           all-vars
           unsafe-vars
           risky-vars
           dir-name)))

(advice-add 'hack-local-variables-confirm :around #'cnit-hack-local)
;; Completions
;; This section will include all completion types and options. This
;; includes, but is not limited to, programming, textual, shell and LLM
;; completions.

;; Improve on the inbuilt completions. Display multiple candidates to
;; make selections easier.

(add-hook
 'after-init-hook
 (lambda ()
   (vertico-mode t)
   (vertico-multiform-mode t)))
(with-eval-after-load 'vertico
  (setopt vertico-cycle t)
  (bind-key "C-<return>" #'vertico-exit-input 'vertico-map))


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

(bind-key "C-c t" #'tempel-insert)


;; Minibuffer completions annotations. Applies useful extended details
;; such as documentation, values, file details etc.

(add-hook 'after-init-hook (lambda () (marginalia-mode)))
(with-eval-after-load 'marginalia
  (bind-key "M-a" #'marginalia-cycle 'minibuffer-mode-map))


;; Gptel

(defvar gptel-prefix
  (let ((map (make-sparse-keymap)))
    (keymap-set map "b" #'gptel)
    (keymap-set map "f" #'gptel-add-file)
    (keymap-set map "m" #'gptel-menu)
    (keymap-set map "p" #'gptel-system-prompt)
    (keymap-set map "q" #'gptel-quick)
    (keymap-set map "r" #'gptel-add)
    (keymap-set map "s" #'gptel-send)
    (keymap-set map "t" #'gptel-tools)
    (keymap-set map "w" #'gptel-rewrite)
    map)
  "Keymap for GPTel related commands.")

(keymap-set global-map "C-c g" gptel-prefix)
(with-eval-after-load 'gptel
  (let ((map gptel-prefix))
    (keymap-set map "d" #'gptel-context-remove)
    (keymap-set map "D" #'gptel-context-remove-all))
  (setopt
   gptel-backend (gptel-make-gh-copilot "Copilot")
   gptel-model 'gpt-4.1
   gptel-default-mode 'org-mode)
  (add-hook 'gptel-mode-hook 'visual-line-mode)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll))
;; Tools
;; This section covers general tools that don't fit into any other
;; area. This will cover things such as build, debug, shells and anything
;; else that is a general function or tool.

;; Capture ansi color codes in complilation buffer for better display.

(with-eval-after-load 'compile
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))


;; Use emacs to help entering text into any program.

(require 'emacs-everywhere)

(setq
 emacs-everywhere-system-configs
 (append
  emacs-everywhere-system-configs
  '(((wayland . niri)
     :focus-command ("niri" "msg" "action" "focus-window" "--id" "%w")
     :info-function emacs-everywhere--app-info-linux-niri))))

(defun emacs-everywhere--app-info-linux-niri ()
  "Return information on the current active window, on a Linux Niri session."
  (require 'json)
  (let* ((json
          (json-read-from-string
           (emacs-everywhere--call
            "niri" "msg" "-j" "focused-window"))) ;; -j for json
         (wid (cdr (assq 'id json)))
         (window-id
          (if (numberp wid)
              (number-to-string wid)
            wid))
         (window-title (cdr (assq 'title json)))
         (app-name (cdr (assq 'app_id json)))
         (window-geometry nil)) ;; no geometry in niri
    (make-emacs-everywhere-app
     :id window-id
     :class app-name
     :title window-title
     :geometry window-geometry)))


;; Kubenetes cluster management.

(autoload 'kele-dispatch "kele" nil t)
(bind-key "C-c k" #'kele-dispatch)
(with-eval-after-load 'kele
  (kele-mode)
  (defun kele--list-kinds (context namespace &rest kinds)
    (magit-insert-section
     (kele-list-root)
     (magit-insert-section
      (overview) (magit-insert-heading "Overview")
      (insert
       (propertize "Context: " 'font-lock-face 'header-line)
       context
       "\n")
      (when namespace
        (insert
         (propertize "Namespace: "
                     'font-lock-face
                     'header-line)
         namespace "\n"))
      (insert
       (propertize "Last Updated: " 'font-lock-face 'header-line)
       (format-time-string "%Y-%m-%d %H:%M:%S"
                           kele--list-snapshot-time)
       "\n")
      (insert "\n"))
     (dolist (kind kinds)
       (-let*
        ((gv
          (car
           (kele--get-groupversions-for-type
            kele--global-discovery-cache
            kind
            :context context)))
         ((group version) (kele--groupversion-split gv))
         (gvk
          (kele--gvk-create
           :group group
           :version version
           :kind kind)))
        (condition-case err
            (magit-insert-section
             (kele-list-table `((gvk . ,gvk)))
             (magit-insert-heading
              (format
               "%s: %s"
               (propertize "Resources"
                           'font-lock-face 'magit-section-heading)
               (propertize kind
                           'font-lock-face 'kele-resource-kind-face)))
             (magit-insert-section-body
              (vtable-insert
               (kele--vtable-tabulate gvk context namespace))
              (vtable-end-of-table) (insert "\n")))
          (error
           (message "[kele] Failed to list %s: %s"
                    kind
                    (error-message-string err))))))))
  (defun kele--edit-resource ()
    (interactive nil kele-get-mode)
    (-let*
     ((ctx kele--current-resource-buffer-context)
      ((&alist
        'kind
        kind
        'apiVersion
        api-version
        'metadata
        (&alist 'name name 'namespace namespace))
       (kele--resource-buffer-context-resource ctx))
      (context (kele--resource-buffer-context-context ctx)))
     (with-editor
      (kele-kubectl-do
       "edit"
       "--context"
       context
       "--namespace"
       namespace
       kind
       name))))
  (bind-key "e" #'kele--edit-resource kele-get-mode-map))
;; Window manager support
;; This section defines functions and settings to support using emacs
;; functionality directly from the window manager through the use of
;; emacsclient.

;; Allow capturing org inbox tasks.

;; emacsclient -s wm -e '(cnit-wm-org-capture)'

(advice-add
 'org-capture-finalize
 :after
 (defun cnit-wm-org-capture-delete-frame (&rest _unused)
   (if (equal "Org Capture" (frame-parameter nil 'name))
       (delete-frame))))

(defun cnit-wm-org-capture ()
  (interactive)
  (require 'cl-lib)
  (let ((frame
         (make-frame
          '((name . "Org Capture")
            (window-system . pgtk)
            ;; (minibuffer . nil)
            (undecorated . t)
            (vertical-scroll-bars . nil)
            (horizontal-scroll-bars . nil)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0)))))
    (with-selected-frame frame
      (cl-letf (((symbol-function 'switch-to-buffer-other-window)
                 'switch-to-buffer))
        (org-capture nil "i")
        (setq-local mode-line-format nil)
        (delete-other-windows)))))
;; Dired mode

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  (setopt
   dired-kill-when-opening-new-dired-buffer t
   dired-do-revert-buffer t
   dired-auto-revert-buffer t
   dired-dwim-target t))

(with-eval-after-load 'wdired
  (setopt wdired-allow-to-change-permissions t))
;; Prog mode
;; Enable supportive modes for programming.

(add-hook
 'prog-mode-hook
 (lambda ()
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


;; Check for balanced parameters prior to allowing save.

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'lisp-data-mode)
              (check-parens)))
          -90)
;; Nix

(add-to-list 'auto-mode-alist `(,(rx ".nix" string-end) . nix-ts-mode))
(provide 'init)

;;; init.el ends here
