# [[file:../../modules.org::*General Settings][General Settings:3]]
{
  flake.aspects.editor.homeManager = {
    programs.emacs.extraPackages = epkgs: with epkgs; [ ];
    editor.earlyInitFiles.core.text = ''
      ;; Ensure loading of latest package version rather than the first
      ;; found. Will ensure doesn't load an older byte compiled version when
      ;; there is a new package version installed.
      (setopt load-prefer-newer t)

      ;; Temporary set garbage collection to high value and set after-init-hook
      ;; to revert to normal. Improves startup performance whilst maintaining
      ;; resonable memory utilistation during normal operation.
      (let ((original-gc-cons-threshold gc-cons-threshold))
        (setopt gc-cons-threshold most-positive-fixnum)
        (add-hook 'after-init-hook
                  (lambda ()
                    (setopt gc-cons-threshold original-gc-cons-threshold))
                  91))

      ;; Disable package installs
      (with-eval-after-load 'package
        (fmakunbound 'package-install)
        (fmakunbound 'package-install-file)
        (fmakunbound 'package-install-from-buffer)
        (fmakunbound 'package-install-from-file)
        (fmakunbound 'package-install-selected-packages)
        (fmakunbound 'package-reinstall)
        (fmakunbound 'package-upgrade)
        (fmakunbound 'package-upgrade-all)
        (fmakunbound 'package-vc-install)
        (fmakunbound 'package-vc-install-from-checkout)
        (fmakunbound 'package-vc-install-selected-packages)
        (fmakunbound 'package-vc-upgrade)
        (fmakunbound 'package-vc-upgrade-all))

      ;; Enable melpa for package searching
      (with-eval-after-load 'package
        (add-to-list
         'package-archives '("melpa" . "https://melpa.org/packages/")
         t))

      ;; Disable UI elements as using Emacs only with keyboard.
      (setopt
       inhibit-startup-screen t
       inhibit-splash-screen t)
      (when (fboundp 'tool-bar-setup)
        (advice-add 'tool-bar-setup :override #'ignore))
      (push '(tool-bar-lines . 0) default-frame-alist)
      (push '(menu-bar-lines . 0) default-frame-alist)
      (push '(vertical-scroll-bars) default-frame-alist)
      (push '(horizontal-scroll-bars) default-frame-alist)
      (when (bound-and-true-p tooltip-mode)
        (tooltip-mode -1))
      (setopt
       use-file-dialog nil
       use-dialog-box nil)

      ;; Set early background colour to help reduce startup flash
      (when (fboundp 'cnit-with-system-colour)
        (cnit-with-system-colour-scheme
         (set-face-attribute
          'default nil
          :background "#000000"
          :foreground "#FFFFFF")
         (set-face-attribute
          'default nil
          :background "#FFFFFF"
          :foreground "#000000")))
    '';
    editor.initFiles.core.text = ''
      ;; Start emacs server after when there is spare time.
      (run-with-idle-timer 5 nil
                           (lambda ()
                                   (unless (and (fboundp 'server-running-p) (server-running-p))
                                     (server-start))))

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

      ;; Improve the inbuilt help with extra contextual information
      (bind-key [remap describe-function] #'helpful-callable)
      (bind-key [remap describe-command] #'helpful-command)
      (bind-key [remap describe-variable] #'helpful-variable)
      (bind-key [remap describe-key] #'helpful-key)
      (bind-key "C-c C-d" #'helpful-at-point 'global-map)

      ;; Prevent accidental closing of Emacs
      (setopt confirm-kill-emacs #'y-or-n-p)

      ;; Tabs. Sometimes a programming language might use tabs, better to turn
      ;; on for those languages specifically as otherwise it'll screw up most
      ;; others.
      (setq-default indent-tabs-mode nil)
    '';

  };
}
# General Settings:3 ends here
