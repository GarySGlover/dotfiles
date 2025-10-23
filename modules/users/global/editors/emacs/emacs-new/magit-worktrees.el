;; -*- lexical-binding: t; -*-

;; Notes
;; worktree created from feature/02460-name-name in repo `repository` doesn't capture
;; work item number correcty annd didn't use repo name worktree ended up at
;; 02460_feature_02460-name-name__test.repo

(defun cnit-magit--select-repo ()
  "Return the current repo path and name, prompting if not in a Git repo."
  (let ((repo-path (magit-toplevel)))
    (unless repo-path
      ;; Not in a repo: ask user
      (setq repo-path
            (read-directory-name "Select Git repository: "
                                 default-directory)))
    `((path ,repo-path)
      (name
       ,(file-name-nondirectory (directory-file-name repo-path))))))

(defun cnit-magit--select-branch (repo-path)
  "Prompt the user to select a branch (local or remote) in REPO-PATH."
  (let* ((default-directory repo-path)
         (local-branches (magit-list-local-branch-names))
         (remote-branches (magit-list-remote-branch-names))
         (all-branches (append local-branches remote-branches)))
    (completing-read "Select branch: " all-branches nil t)))

(defun cnit-magit--branch-name-from-remote (remote-branch)
  "Return the branch name from a REMOTE-BRANCH string using verbose rx.
Example: 'origin/feature/test' → 'feature/test'."
  (if (string-match
       (rx
        string-start
        (one-or-more alphanumeric)
        "/"
        (group (one-or-more any))
        string-end)
       remote-branch)
      (match-string 1 remote-branch)
    remote-branch))

(defun cnit-magit--ensure-local-branch (branch repo-path)
  "Ensure BRANCH exists locally in REPO-PATH.
If BRANCH is remote-only, create a local tracking branch."
  (let* ((default-directory repo-path)
         (name (cnit-magit--branch-name-from-remote branch)))
    (unless (or (member branch (magit-list-local-branch-names))
                (member name (magit-list-local-branch-names)))
      (magit-branch-create name branch))
    ;; Return the local branch name
    (if (member branch (magit-list-local-branch-names))
        branch
      name)))

(defun cnit-magit--parse-worktree-name (basename)
  "Parse BASENAME of a worktree folder.
Return an alist with keys (ticket type name repo) or nil if no match."
  (when (string-match
         (rx
          (or string-start "/")
          (group-n 1 (one-or-more (or wordchar "-")))
          (zero-or-one (and "__" (group-n 2 (one-or-more num))))
          (zero-or-one (and "__" (group-n 3 (one-or-more letter))))
          (and "__" (group-n 4 (one-or-more (or wordchar "-" "."))))
          (or string-end "/"))
         basename)
    `((name . ,(match-string 1 basename))
      (ticket . ,(match-string 2 basename))
      (type . ,(match-string 3 basename))
      (repo . ,(match-string 4 basename)))))

(defun cnit-magit--list-worktrees ()
  "Return a deduplicated list of parsed worktree info from ~/worktrees.
Each element is an alist with keys (ticket type name).
:repo is discarded for uniqueness here."
  (let* ((worktree-root (expand-file-name "~/worktrees"))
         (dirs
          (when (file-directory-p worktree-root)
            (directory-files worktree-root t "^[^.]")))
         results)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (let ((parsed
               (cnit-magit--parse-worktree-name
                (file-name-nondirectory dir))))
          (when parsed
            ;; Drop :repo for uniqueness
            (push `((ticket . ,(alist-get 'ticket parsed))
                    (type . ,(alist-get 'type parsed))
                    (name . ,(alist-get 'name parsed)))
                  results)))))
    ;; Deduplicate by ticket+type+name
    (delete-dups results)))

(defun cnit-magit--ticket-candidates-for-branch (local-branch)
  "Return a list of ticket numbers from existing worktrees matching LOCAL-BRANCH.
LOCAL-BRANCH is expected in the form `type/name` or just `name`."
  (let ((candidates '()))
    (dolist (wt (cnit-magit--list-worktrees))
      (let* ((wt-type (alist-get 'type wt))
             (wt-name (alist-get 'name wt))
             (branch
              (if wt-type
                  (concat wt-type "/" wt-name)
                wt-name)))
        (when (string= branch local-branch)
          (let ((ticket (alist-get 'ticket wt)))
            (when ticket
              (push ticket candidates))))))
    (delete-dups (nreverse candidates))))

(defvar cnit-magit--ticket-providers '()
  "Alist mapping provider names to ticket-fetching functions.

Each entry is of the form (NAME . FUNCTION), where:
- NAME is a string shown in `completing-read` when prompting for a provider.
- FUNCTION is a symbol naming a function to call when the provider
  is selected.

The FUNCTION must accept a single argument:
  LOCAL-BRANCH (a string of the form \"type/name\" or just \"name\").
This can be used by the provider to filter relevant tickets,
though it is not required.

The FUNCTION must return a list of ticket identifiers as strings.
These strings will be shown to the user in a `completing-read` prompt.

Examples:
  (defun cnit-magit--fetch-github-tickets (local-branch)
    ;; Fetch open issues from GitHub API, return a list of IDs as strings
    '(\"101\" \"102\" \"103\"))

  (defun cnit-magit--fetch-azure-tickets (local-branch)
    ;; Fetch work items from Azure DevOps, return IDs as strings
    '(\"A123\" \"A124\"))

To add a new provider, simply push a new (NAME . FUNCTION) pair
onto `cnit-magit--ticket-providers`.")

(defun cnit-magit--prompt-ticket (local-branch)
  "Prompt for a ticket number for LOCAL-BRANCH.
First show local tickets and provider names.  If a provider is selected,
fetch tickets from the provider and prompt again.

Only allows new tickets that are all digits. If the user enters an empty string,
returns nil."
  (let* ((local-tickets
          (cnit-magit--ticket-candidates-for-branch local-branch))
         (providers (mapcar #'car cnit-magit--ticket-providers))
         (initial-candidates
          (append local-tickets providers '("None")))
         (done nil)
         (result nil))
    (while (not done)
      (let ((selection
             (completing-read
              "Select ticket or provider (None = no ticket): "
              initial-candidates
              nil
              nil)))
        (cond
         ;; User selected a local ticket
         ((member selection local-tickets)
          (setq result selection)
          (setq done t))
         ;; User selected a provider
         ((member selection providers)
          (let* ((fetch-fn
                  (cdr
                   (assoc selection cnit-magit--ticket-providers)))
                 (provider-tickets
                  (when fetch-fn
                    (funcall fetch-fn local-branch)))
                 (ticket
                  (completing-read (format "Select ticket from %s: "
                                           selection)
                                   provider-tickets
                                   nil t)))
            (if (or (not ticket) (string-empty-p ticket))
                (setq result nil)
              (setq result ticket))
            (setq done t)))
         ;; User chose "None"
         ((string= selection "None")
          (setq result nil)
          (setq done t))
         ;; User typed a new ticket number
         (t
          (cond
           ((string-empty-p selection)
            (setq result nil)
            (setq done t))
           ((string-match-p "\\`[0-9]+\\'" selection)
            (setq result selection)
            (setq done t))
           (t
            (message "Ticket must be all digits. Please try again.")
            (sit-for 1)))))))
    result))

(defun cnit-magit--create-worktree (repo-path worktree-dir branch)
  "Create a git worktree for BRANCH at WORKTREE-DIR from REPO-PATH."
  (unless (file-directory-p worktree-dir)
    (let ((default-directory repo-path))
      (magit-worktree-add worktree-dir branch))))

(defun cnit-magit--parse-branch-for-info (local-branch)
  "Parse LOCAL-BRANCH and extract type, ticket, and name information.

LOCAL-BRANCH is expected to be in the form \"type/ticket-name\" or \"ticket-name\".
Returns an alist with keys:
- 'type: the branch type (e.g. \"feature\"), or nil if not present.
- 'ticket: the ticket number as a string, or nil if not present.
- 'name: the remainder of the branch name as a string.

Example:
  (cnit-magit--parse-branch-for-info \"feature/1234-fix-bug\")
  => ((type . \"feature\") (ticket . \"1234\") (name . \"fix-bug\"))
"
  (when (string-match
         (rx
          string-start
          (zero-or-one (and (group-n 1 (one-or-more letter)) "/"))
          (zero-or-one (and (group-n 2 (one-or-more num)) "-"))
          (group-n 3 (one-or-more graphic))
          string-end)
         local-branch)
    `((type . ,(match-string 1 local-branch))
      (ticket . ,(match-string 2 local-branch))
      (name . ,(match-string 3 local-branch)))))

(defun cnit-magit--get-ticket-for-branch (local-branch)
  "Get the ticket number for LOCAL-BRANCH.

First attempts to parse the ticket number from the branch name using
`cnit-magit--parse-branch-for-info`. If not found, prompts the user
to select or enter a ticket number using `cnit-magit--prompt-ticket`.

Returns the ticket number as a string, or nil if none is selected."
  (or (cdr
       (assoc
        'ticket (cnit-magit--parse-branch-for-info local-branch)))
      (cnit-magit--prompt-ticket local-branch)))

(defun cnit-magit--worktree-dir (name repo-name &optional type ticket)
  "Return the worktree directory path for a branch.

NAME is the branch name (string).
REPO-NAME is the name of the repository.
TYPE is the branch type (e.g. \"feature\"), or nil.
TICKET is the ticket number as a string, or nil.

The directory name is constructed as:
  <name>__[ticket][__type]__<repo-name>
All components are lowercased. If TICKET or TYPE are nil, they are omitted.

Returns the absolute path to the worktree directory."
  (let ((ticket-tag
         (if ticket
             (format "__%s" ticket)
           ""))
        (type-tag
         (if type
             (format "__%s" (downcase type))
           ""))
        (repo-tag (format "__%s" repo-name)))
    (expand-file-name (concat
                       (downcase name) ticket-tag type-tag repo-tag)
                      "~/worktrees")))

(defun cnit-magit--new-branch-name ()
  "Prompt the user for a new branch name, format it, and return it as a string.

The name is lowercased, spaces and non-alphanumeric characters are replaced with '-',
and leading/trailing dashes are removed."
  (let ((name-raw
         (downcase
          (completing-read
           "Branch name: "
           (-map
            (lambda (x) (cdr (assoc 'name x)))
            (cnit-magit--list-worktrees))))))
    (replace-regexp-in-string
     (rx
      (or (and bos (one-or-more "-")) (and (one-or-more "-") eos)))
     ""
     (replace-regexp-in-string
      (rx (one-or-more (not alnum))) "-" name-raw))))

(defun cnit-magit--new-branch-type ()
  "Prompt the user for a branch type and return it as a lowercase string.

Returns nil if the user enters an empty string."
  (let ((result
         (downcase
          (completing-read "Type: " '("Feature" "Bug") nil t))))
    (if (string-empty-p result)
        nil
      result)))

(defun cnit-magit--worktree-branch
    (worktree name &optional type ticket)
  "Create a new branch and corresponding worktree.

WORKTREE is the directory for the new worktree.
NAME is the branch name (string).
TYPE is the branch type (e.g. \"feature\"), or nil.
TICKET is the ticket number as a string, or nil.

Prompts for a starting point for the branch, constructs the branch name,
and creates the worktree and branch."
  (let* ((start (magit-read-starting-point "Branch"))
         (branch-name
          (concat
           (when type
             (concat type "/"))
           (when ticket
             (concat ticket "-"))
           name)))
    (magit-worktree-branch worktree branch-name start)))

;; Main worktree flows
(defun cnit-magit-worktree-checkout-existing ()
  "Checkout an existing branch into a worktree folder."
  (interactive)
  (when-let* ((repo (cnit-magit--select-repo))
              (repo-path (car (alist-get 'path repo)))
              (repo-name (car (alist-get 'name repo)))
              (branch (cnit-magit--select-branch repo-path))
              (local-branch
               (cnit-magit--ensure-local-branch branch repo-path))
              (worktree
               (let* ((ticket
                       (cnit-magit--get-ticket-for-branch
                        local-branch))
                      (info
                       (cnit-magit--parse-branch-for-info
                        local-branch))
                      (type (cdr (assoc 'type info)))
                      (name (cdr (assoc 'name info))))
                 (cnit-magit--worktree-dir name repo-name
                                           type
                                           ticket))))
    (magit-worktree-checkout worktree local-branch)))

(defun cnit-magit-worktree-chekout-new ()
  "Checkout a new branch into a worktree folder."
  (interactive)
  (when-let* ((repo (cnit-magit--select-repo))
              (repo-path (car (alist-get 'path repo)))
              (repo-name (car (alist-get 'name repo)))
              (name (cnit-magit--new-branch-name)))
    (let* ((type (cnit-magit--new-branch-type))
           (ticket
            (cnit-magit--prompt-ticket
             (if type
                 (format "%s/%s" type name)
               name)))
           (worktree
            (cnit-magit--worktree-dir name repo-name type ticket)))
      (cnit-magit--worktree-branch worktree name type ticket))))
