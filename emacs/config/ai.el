;; ai.el --- Agent Shell configuration  -*- lexical-binding: t -*-

(require 'acp)
(require 'agent-shell)

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

(setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))

(setq agent-shell-anthropic-default-model-id "claude-opus-4-6")

(defun claude (dir)
  "Start a new Agent Shell session in DIR."
  (interactive (list (read-directory-name "Directory: " default-directory)))
  (let ((default-directory dir))
    (agent-shell--new-shell :location dir)))

(setq agent-shell-busy-indicator-frames 'dots-block)

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
directory patterns match as prefixes, file patterns match exactly."
  (seq-some
   (lambda (pattern)
     (let ((expanded (cond
                      ((string= pattern "/")
                       cwd)
                      ((string-prefix-p "//" pattern)
                       (substring pattern 1))
                      (t (expand-file-name pattern)))))
       (if (string-suffix-p "/" expanded)
           (string-prefix-p expanded path)
         (string= expanded path))))
   patterns))

(defun agent-shell--permission-command-match-p (command patterns)
  "Return non-nil if COMMAND matches any of PATTERNS.
`*' matches everything, trailing `*' matches as a prefix."
  (seq-some
   (lambda (pattern)
     (cond
      ((string= pattern "*") t)
      ((string-suffix-p "*" pattern)
       (string-prefix-p (substring pattern 0 -1) command))
      (t (string= pattern command))))
   patterns))

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

(defun agent-shell--permission-should-allow-p (kind title permissions cwd)
  "Return non-nil if a tool of KIND with TITLE should be auto-allowed.
CWD is the session working directory used to expand `/' patterns.
KIND is \"read\", \"write\", \"edit\", \"search\", or \"execute\".
\"edit\" is treated as \"write\" and \"search\" as \"read\".  For execute, if TITLE
matches an `ask' pattern, only specific (non-wildcard) `allow'
patterns can override it.  Additionally, any file paths in the
command must be within allowed read or write directories.
For MCP tools, TITLE is matched against command patterns under
the `mcp' kind."
  (let* ((kind-sym (intern kind))
         (canonical (pcase kind-sym
                      ('edit 'write)
                      ('search 'read)
                      (_ kind-sym)))
         (allow-patterns (cdr (assq canonical (cdr (assq 'allow permissions)))))
         (ask-patterns (cdr (assq canonical (cdr (assq 'ask permissions))))))
    (cond
     ((memq canonical '(read write))
      (let ((paths (agent-shell--extract-command-paths title)))
        (agent-shell--permission-path-match-p
         (or (car paths) title) allow-patterns cwd)))
     ((eq kind-sym 'execute)
      (let ((command-allowed
             (if (agent-shell--permission-command-match-p title ask-patterns)
                 (seq-some
                  (lambda (pattern)
                    (and (not (string= pattern "*"))
                         (if (string-suffix-p "*" pattern)
                             (string-prefix-p (substring pattern 0 -1) title)
                           (string= pattern title))))
                  allow-patterns)
               (agent-shell--permission-command-match-p title allow-patterns))))
        (and command-allowed
             (agent-shell--permission-paths-allowed-p title permissions cwd))))
     ((eq kind-sym 'mcp)
      (agent-shell--permission-command-match-p title allow-patterns))
     (t nil))))

(defun agent-shell-make-permission (permissions)
  "Return a permission responder function using declarative PERMISSIONS.
PERMISSIONS is an alist with `allow' and `ask' keys, each containing
kind-specific pattern lists matching Claude Code settings.json format."
  (lambda (permission)
    (let* ((tool-call (map-elt permission :tool-call))
           (kind (map-elt tool-call :kind))
           (title (or (map-elt tool-call :title) ""))
           (cwd (agent-shell-cwd))
           (_ (message "Permission check: kind=%s title=%s cwd=%s" kind title cwd))
           (allow-choice (seq-find
                          (lambda (opt)
                            (equal (map-elt opt :kind) "allow_once"))
                          (map-elt permission :options))))
      (when (and (agent-shell--permission-should-allow-p kind title permissions cwd)
                 allow-choice)
        (funcall (map-elt permission :respond)
                 (map-elt allow-choice :option-id))
        t))))
