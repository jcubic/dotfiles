;; ai.el --- Agent Shell configuration  -*- lexical-binding: t -*-
;;
;; Agent-shell permission usage
;;
;; (setq agent-shell-permission-responder-function
;;       (agent-shell-make-permission
;;        '((allow
;;           (read . ("~/projects/" "~/.claude/" "~/.agent-shell/"
;;                    "~/.emacs" "~/.bashrc" "~/bin/" "//tmp/"
;;                    "~/.emacs.d/"))
;;           (write . ("//tmp/" "/"))
;;           (execute . ("*"
;;                       "git checkout *" "git status *" "git diff *"
;;                       "git clone *" "git ls-tree *" "git ls-files *"
;;                       "git log *" "git show *" "git branch *"
;;                       "git reflog *" "git rev-parse *" "git remote -v *"
;;                       "git config *" "git grep *"))
;;           (mcp . ("*")))
;;          (ask
;;           (execute . ("sudo *" "ssh *" "git *" "kill *" "emacsclient *"))
;;           (mcp . ("playwright-browser")))))) ;; MCP that use my own browser

(require 'acp)
(require 'agent-shell)

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

(setq agent-shell-preferred-agent-config
      (agent-shell-anthropic-make-claude-code-config))

(setq agent-shell-anthropic-default-model-id "claude-opus-4-6")
(setq agent-shell-busy-indicator-frames 'dots-block)
(setq agent-shell-context-sources nil)
(setq acp-logging-enabled t)
(setq agent-shell-session-strategy 'prompt)

(defun claude (dir)
  "Start a new Agent Shell session in DIR."
  (interactive (list (read-directory-name "Directory: " default-directory nil t)))
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (agent-shell '(4))))

(defun agent-shell-hook ()
  "Set up key bindings for `agent-shell-mode' buffers."
  (interactive)
  (local-set-key [C-M-tab] 'previous-buffer-same-mode)
  (local-set-key [C-tab] 'next-buffer-same-mode)
  (local-set-key [s-tab] 'agent-shell-cycle-session-mode)
  (local-set-key [C-up] 'backward-paragraph)
  (local-set-key [C-down] 'forward-paragraph))

(add-hook 'agent-shell-mode-hook 'agent-shell-hook)

;; --------------------------------------------------------------------------
;; :: AGENT-SHELL PERMISSION SYSTEM
;; --------------------------------------------------------------------------

(defun agent-shell--permission-path-match-p (path patterns cwd)
  "Return non-nil if PATH matches any of PATTERNS.
`/' means the session CWD, `//' prefix means literal root path,
directory patterns match as prefixes, file patterns match exactly.
Symlinks are resolved on both sides before comparing."
  (let ((true-path (file-truename path)))
    (seq-some
     (lambda (pattern)
       (let* ((expanded (cond
                         ((string= pattern "/")
                          cwd)
                         ((string-prefix-p "//" pattern)
                          (substring pattern 1))
                         (t (expand-file-name pattern))))
              (is-dir (string-suffix-p "/" expanded))
              (true-expanded (file-name-as-directory (file-truename expanded)))
              (true-expanded (if is-dir true-expanded
                              (directory-file-name true-expanded))))
         (if is-dir
             (or (string-prefix-p true-expanded true-path)
                 (string= (substring true-expanded 0 -1) true-path))
           (string= true-expanded true-path))))
     patterns)))

(defun agent-shell--normalize-git-command (command)
  "Strip -C <path> from a git COMMAND for pattern matching.
Returns (NORMALIZED-CMD . EXTRACTED-PATH-OR-NIL)."
  (let ((cmd (string-trim command)))
    (if (string-match "^\\(git\\)\\s-+-C\\s-+\\(\\S-+\\)\\s-+\\(.*\\)$" cmd)
        (cons (concat "git " (match-string 3 cmd))
              (expand-file-name (match-string 2 cmd)))
      (cons cmd nil))))

(defun agent-shell--permission-single-command-match-p (command patterns)
  "Return non-nil if a single COMMAND matches any of PATTERNS.
`*' matches everything, trailing `*' matches as a prefix.
Git commands with -C <path> are normalized before matching."
  (let* ((normalized (agent-shell--normalize-git-command command))
         (cmd (car normalized)))
    (seq-some
     (lambda (pattern)
       (cond
        ((string= pattern "*") t)
        ((string-suffix-p "*" pattern)
         (let ((prefix (substring pattern 0 -1)))
           (or (string-prefix-p prefix cmd)
               (string= (string-trim-right prefix) cmd))))
        (t (string= pattern cmd))))
     patterns)))

(defun agent-shell--split-compound-command (command)
  "Split a compound COMMAND into individual sub-commands."
  (split-string command "[;|&]+" t "[ \t]+"))

(defun agent-shell--permission-command-match-p (command patterns)
  "Return non-nil if any sub-command in COMMAND matches PATTERNS.
Splits compound commands on `;', `&&', `||', and `|'."
  (let ((sub-commands (agent-shell--split-compound-command command)))
    (seq-some
     (lambda (cmd)
       (agent-shell--permission-single-command-match-p cmd patterns))
     sub-commands)))

(defun agent-shell--extract-command-paths (command)
  "Extract file path arguments from COMMAND string.
Recognizes absolute paths, ~ paths, and relative paths."
  (let (paths)
    (dolist (token (split-string command))
      (when (or (string-prefix-p "/" token)
                (string-prefix-p "~" token)
                (string-prefix-p "./" token)
                (string-prefix-p "../" token))
        (push (expand-file-name token) paths)))
    (nreverse paths)))

(defun agent-shell--permission-paths-allowed-p (command permissions cwd)
  "Return non-nil if all file paths in COMMAND are within allowed directories.
Checks paths against both read and write allow patterns."
  (let* ((read-patterns (cdr (assq 'read (cdr (assq 'allow permissions)))))
         (write-patterns (cdr (assq 'write (cdr (assq 'allow permissions)))))
         (all-patterns (append read-patterns write-patterns))
         (paths (agent-shell--extract-command-paths command)))
    (or (null paths)
        (seq-every-p
         (lambda (path)
           (agent-shell--permission-path-match-p path all-patterns cwd))
         paths))))

(defun agent-shell--mcp-tool-allowed-p (title)
  "Return non-nil if TITLE names a tool from a configured MCP server.
Extracts the server name from TITLE (e.g. `mcp__playwright__browse')
and checks if it exists in `agent-shell-mcp-servers'."
  (when (string-match "^mcp__\\([^_]+\\)__" title)
    (let ((server-name (match-string 1 title)))
      (seq-some (lambda (server)
                  (equal (cdr (assq 'name server)) server-name))
                agent-shell-mcp-servers))))

(defun agent-shell--permission-extract-fetch-url (title)
  "Extract the URL from a fetch permission TITLE.
TITLE is typically \"Fetch https://...\"; returns just the URL."
  (if (string-match "^Fetch \\(.*\\)$" title)
      (match-string 1 title)
    title))

(defun agent-shell--permission-should-deny-p (kind title permissions cwd)
  "Return non-nil if a tool of KIND with TITLE should be auto-denied.
Checks `deny' patterns in PERMISSIONS for read, write, execute, and fetch."
  (let* ((kind-sym (intern kind))
         (canonical (pcase kind-sym
                      ('edit 'write)
                      ('search 'read)
                      (_ kind-sym)))
         (deny-patterns (cdr (assq canonical (cdr (assq 'deny permissions))))))
    (when deny-patterns
      (pcase canonical
        ('fetch
         (agent-shell--permission-single-command-match-p
          (agent-shell--permission-extract-fetch-url title) deny-patterns))
        ((or 'read 'write)
         (let* ((paths (agent-shell--extract-command-paths title))
                (path (or (car paths)
                          (when (string-match " \\(.+\\)$" title)
                            (expand-file-name (match-string 1 title) cwd))
                          (expand-file-name title cwd))))
           (agent-shell--permission-path-match-p path deny-patterns cwd)))
        ('execute
         (let ((sub-commands (agent-shell--split-compound-command title)))
           (seq-some
            (lambda (cmd)
              (agent-shell--permission-single-command-match-p
               (string-trim cmd) deny-patterns))
            sub-commands)))))))

(defun agent-shell--permission-should-allow-p (kind title permissions cwd)
  "Return non-nil if a tool of KIND with TITLE should be auto-allowed.
CWD is the session working directory used to expand `/' patterns.
KIND is \"read\", \"write\", \"edit\", \"search\", \"execute\", or \"fetch\".
\"edit\" is treated as \"write\" and \"search\" as \"read\".  For execute, if TITLE
matches an `ask' pattern, only specific (non-wildcard) `allow'
patterns can override it.  Additionally, any file paths in the
command must be within allowed read or write directories.
For MCP tools, TITLE is matched against command patterns under
the `mcp' kind.  For fetch, URL patterns are matched against
allow/ask lists; deny patterns are handled separately by
`agent-shell--permission-should-deny-p'."
  (let* ((kind-sym (intern kind))
         (canonical (pcase kind-sym
                      ('edit 'write)
                      ('search 'read)
                      (_ kind-sym)))
         (allow-patterns (cdr (assq canonical (cdr (assq 'allow permissions)))))
         (ask-patterns (cdr (assq canonical (cdr (assq 'ask permissions))))))
    (cond
     ((memq canonical '(read write))
      (let* ((paths (agent-shell--extract-command-paths title))
             (path (or (car paths)
                       (when (string-match " \\(.+\\)$" title)
                         (expand-file-name (match-string 1 title) cwd))
                       (expand-file-name title cwd))))
        (agent-shell--permission-path-match-p path allow-patterns cwd)))
     ((eq kind-sym 'execute)
      (let* ((sub-commands (agent-shell--split-compound-command title))
             (read-patterns (cdr (assq 'read (cdr (assq 'allow permissions)))))
             (write-patterns (cdr (assq 'write (cdr (assq 'allow permissions)))))
             (path-patterns (append read-patterns write-patterns))
             (command-allowed
              (seq-every-p
               (lambda (cmd)
                 (let ((trimmed (string-trim cmd)))
                   (cond
                    ;; cd <path>: allowed if path is within read/write dirs
                    ((string-match "^cd\\s-+\\(\\S-+\\)" trimmed)
                     (let ((path (expand-file-name (match-string 1 trimmed))))
                       (agent-shell--permission-path-match-p path path-patterns cwd)))
                    ;; matches ask pattern: need specific (non-wildcard) allow
                    ((agent-shell--permission-single-command-match-p trimmed ask-patterns)
                     (seq-some
                      (lambda (pattern)
                        (and (not (string= pattern "*"))
                             (agent-shell--permission-single-command-match-p
                              trimmed (list pattern))))
                      allow-patterns))
                    ;; otherwise: normal allow check
                    (t (agent-shell--permission-single-command-match-p trimmed allow-patterns)))))
               sub-commands)))
        (and command-allowed
             (agent-shell--permission-paths-allowed-p title permissions cwd))))
     ((and (eq kind-sym 'other)
           (string-prefix-p "mcp__" title))
      (let* ((mcp-allow (cdr (assq 'mcp (cdr (assq 'allow permissions)))))
             (mcp-ask (cdr (assq 'mcp (cdr (assq 'ask permissions)))))
             (server-name (when (string-match "^mcp__\\([^_]+\\)__" title)
                            (match-string 1 title))))
        (when server-name
          (if (member server-name mcp-ask)
              nil
            (or (member server-name mcp-allow)
                (member "*" mcp-allow)
                (agent-shell--mcp-tool-allowed-p title))))))
     ((eq kind-sym 'fetch)
      (let ((url (agent-shell--permission-extract-fetch-url title)))
        (cond
         ((and ask-patterns
               (agent-shell--permission-single-command-match-p url ask-patterns))
          (seq-some
           (lambda (pattern)
             (and (not (string= pattern "*"))
                  (agent-shell--permission-single-command-match-p
                   url (list pattern))))
           allow-patterns))
         (t (and allow-patterns
                 (agent-shell--permission-single-command-match-p url allow-patterns))))))
     (t nil))))

(defun agent-shell-make-permission (permissions)
  "Return a permission responder function using declarative PERMISSIONS.
PERMISSIONS is an alist with `allow', `ask', and `deny' keys, each
containing kind-specific pattern lists.  `deny' auto-rejects matching
requests.  `ask' falls through to the interactive UI.  `allow'
auto-approves."
  (lambda (permission)
    (let* ((tool-call (map-elt permission :tool-call))
           (kind (map-elt tool-call :kind))
           (title (or (map-elt tool-call :title) ""))
           (cwd (agent-shell-cwd))
           (_ (message "Permission check: kind=%s title=%s cwd=%s" kind title cwd))
           (allow-choice (seq-find
                          (lambda (opt)
                            (equal (map-elt opt :kind) "allow_once"))
                          (map-elt permission :options)))
           (reject-choice (seq-find
                           (lambda (opt)
                             (equal (map-elt opt :kind) "reject_once"))
                           (map-elt permission :options))))
      (cond
       ((and (agent-shell--permission-should-deny-p kind title permissions cwd)
             reject-choice)
        (funcall (map-elt permission :respond)
                 (map-elt reject-choice :option-id))
        t)
       ((and (agent-shell--permission-should-allow-p kind title permissions cwd)
             allow-choice)
        (funcall (map-elt permission :respond)
                 (map-elt allow-choice :option-id))
        t)))))

(defun agent-shell-session-file ()
  "Copy the current session transcript file path to the kill ring."
  (interactive)
  (if agent-shell--transcript-file
      (progn
        (kill-new agent-shell--transcript-file)
        (message "Copied: %s" agent-shell--transcript-file))
    (user-error "No transcript file for this session")))

;; -----------------------------------------------------------------------------
;; Agent-Shell debug code
;; -----------------------------------------------------------------------------
(defun agent-ping ()
  "Function wake up the agent-shell when it's halted. The request are in limbo never fullfilled so the agent is stalled."
  (interactive)
  (progn
    (map-put! agent-shell--state :active-requests nil)
    (shell-maker-finish-output :config shell-maker--config :success t)))

;; -----------------------------------------------------------------------------
(defun agent-show-pending ()
  "show Agent-Shell pending requests"
  (interactive)
  (map-elt (map-elt agent-shell--state :client) :pending-requests))

;; -----------------------------------------------------------------------------
;; remove advice
;; (advice-remove 'acp--route-incoming-message #'my/acp-debug-advice)
;; (advice-mapc (lambda (fn _props) (message "%S" fn)) 'acp--route-incoming-message)
;; -----------------------------------------------------------------------------

(defun my/acp-debug-advice (orig-fn &rest args)
  (condition-case err
      (apply orig-fn args)
    (error
     (with-current-buffer (get-buffer-create "*agent-shell-debug*")
       (goto-char (point-max))
       (insert (format "\n\n=== %s ===\nError: %S\nBacktrace:\n%s\n"
                       (format-time-string "%T")
                       err
                       (with-output-to-string (backtrace)))))
     (message "agent-shell ACP error logged to *agent-shell-debug*"))))

(advice-add 'acp--route-incoming-message :around #'my/acp-debug-advice)

;; --------------------------------------------------------------------------
;; :: AGENT-SHELL CONFIG
;; --------------------------------------------------------------------------
(setq agent-shell-mcp-servers
      `(((name . "context7")
         (type . "http")
         (url . "https://mcp.context7.com/mcp")
         (headers . (((name . "CONTEXT7_API_KEY")
                      (value . ,(if (boundp 'CONTEXT_7_API_KEY) CONTEXT_7_API_KEY ""))))))
        ((name . "brave-search")
         (command . "npx")
         (args . ("-y" "@brave/brave-search-mcp-server"))
         (env . (((name . "BRAVE_API_KEY")
                  (value . ,(if (boundp 'BRAVE_SEARCH_API_KEY) BRAVE_SEARCH_API_KEY ""))))))
        ((name . "DeepWiki")
         (type . "http")
         (url . "https://mcp.deepwiki.com/mcp")
         (headers . nil))
        ((name . "chrome-devtools")
         (command . "npx")
         (args . ("-y" "chrome-devtools-mcp@latest" "--browser-url=http://127.0.0.1:9222"))
         (env . nil))
        ((name . "chakra-ui")
         (command . "npx")
         (args . ("-y" "@chakra-ui/react-mcp"))
         (env . nil))
        ((name . "reader")
         (command . "npx")
         (args . ("-y" "@nicepkg/jina-reader-mcp"))
         (env . nil))
        ((name . "playwright")
         (command . "npx")
         (args . ("-y" "@playwright/mcp@latest" "--config" "~/projects/jcubic/dotfiles/emacs/playwright-mcp.json"))
         (env . nil))
        ((name . "playwright-browser")
         (command . "npx")
         (args . ("-y" "@playwright/mcp@latest" "--extension"))
         (env . nil))))

(setq agent-shell-permission-responder-function
      (agent-shell-make-permission
       '((allow
          (read . ("~/projects/" "~/.claude/" "~/.agent-shell/" "~/.mutimon/"
                   "~/.horavox/" "~/.emacs" "~/.bashrc" "~/bin/" "//tmp/"
                   "~/.emacs.d/"))
          (write . ("~/.mutimon/" "~/.horavox/" "//tmp/" "/"))
          (execute . ("*"
                      "git checkout *" "git status *" "git diff *"
                      "git clone *" "git ls-tree *" "git ls-files *"
                      "git log *" "git show *" "git branch *"
                      "git reflog *" "git rev-parse *" "git remote -v *"
                      "git config *" "git grep *"))
          (mcp . ("*"))
          (fetch . ("*")))
         (deny
          (execute . ("node -e *" "python -c *" "bash -c *")))
         (ask
          (execute . ("sudo *" "ssh *" "git *" "kill *" "emacsclient *" "emacs-version"))
          (mcp . ("playwright-browser"))))))
