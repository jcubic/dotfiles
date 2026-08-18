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

;; load latest Agent-Shell from source
(dolist (path '("/home/kuba/projects/jcubic/agent-shell"
                "/home/kuba/projects/jcubic/shell-maker"
                "/home/kuba/projects/jcubic/acp.el"))
   (add-to-list 'load-path path))

(require 'acp)
(require 'agent-shell)

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

(setq agent-shell-opencode-authentication
      (agent-shell-opencode-make-authentication :none t))

(setq agent-shell-markdown-render-function #'agent-shell-markdown-replace-markup)
(setq agent-shell-highlight-blocks t)

(setq agent-shell-anthropic-default-model-id "opus[1m]")

(setq agent-shell-busy-indicator-frames 'dots-block)
(setq agent-shell-context-sources nil)
(setq acp-logging-enabled t)
(setq agent-shell-session-strategy 'prompt)

;; --------------------------------------------------------------------------
;; :: AGENT-SHELL INIT FUNCTIONS
;; --------------------------------------------------------------------------
(defun run-agent (dir)
  "Function that runs agent-shell with a given directory"
  (let ((default-directory
         (file-name-as-directory (expand-file-name dir))))
    (agent-shell '(4))))

(defmacro agent (symbol)
  "Macro that create Agent-Shell runner function"
  (let* ((name (symbol-name symbol))
         (config (cond ((string-equal name "claude")
                        '(agent-shell-anthropic-make-claude-code-config))
                       ((string-equal name "opencode")
                        '(agent-shell-opencode-make-agent-config))
                       (t (error (concat "wrong name: " name))))))
    `(defun ,symbol (dir)
       ,(concat "Function that run Agent-Shell with " name " ACP")
       (interactive "DDirectory: ")
       (let ((agent-shell-preferred-agent-config ,config))
         (run-agent dir)))))

(agent claude)
(agent opencode)

;; --------------------------------------------------------------------------
;; :: KEYBINDING
;; --------------------------------------------------------------------------
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
;; :: COMPLETION
;; --------------------------------------------------------------------------
;; company setup
(defun agent-shell-company-trigger ()
  "Open company on @ or / at a word boundary, like corfu does."
  (when (and (memq (char-before) '(?@ ?/))
             (or (= (point) (1+ (line-beginning-position)))
                 (memq (char-before (1- (point))) '(?\s ?\t ?\n))))
    ;; manual begin ignores min-prefix, so @ alone pops
    (company-manual-begin)))

(defun agent-shell-company-setup ()
  (when agent-shell-completion-mode
    (setq-local company-backends '(company-capf))
    (setq-local company-idle-delay nil)
    (setq-local company-minimum-prefix-length 3)
    (company-mode 1)
    ;; replace agent-shell's completion-at-point trigger with a company one
    (remove-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point t)
    (add-hook 'post-self-insert-hook #'agent-shell-company-trigger nil t)))

(add-hook 'agent-shell-completion-mode-hook #'agent-shell-company-setup)

;; --------------------------------------------------------------------------
;; FIX THINKING
;; --------------------------------------------------------------------------
;; temporary hack to enable Claude Code thinking
;; (defun add-acp-config (args)
;;   (if (plist-member args :meta)
;;       args
;;     (append args
;;             '(:meta ((claudeCode
;;                       . ((options
;;                           . ((thinking
;;                               . ((type . "adaptive")
;;                                  (display . "summarized"))))))))))))
;;
;; (with-eval-after-load 'acp
;;   (advice-add
;;    'acp-make-session-new-request :filter-args
;;    'add-acp-config
;;    '((name . agent-shell/summarized-thinking))))
;;
;; (advice-remove 'acp-make-session-new-request 'add-acp-config)

;; --------------------------------------------------------------------------
;; :: MOUSE FIX
;; --------------------------------------------------------------------------
;; `agent-shell--filter-buffer-substring' walks the range with
;; `(while (< pos end) ...)'.  When START > END -- a right-to-left mouse
;; selection, or a kill where mark > point -- the loop never runs and the
;; function returns "", silently breaking mouse copy depending on selection
;; direction.  Normalize the range in :filter-args so START <= END, matching
;; stock `buffer-substring'.  (`delete-region' already handles either order.)
;; Remove once the upstream fix (jcubic/agent-shell) is merged.

(defun agent-shell--normalize-filter-range (args)
  "Sort the START/END of ARGS so a reversed range is not dropped."
  (let ((start (nth 0 args))
        (end   (nth 1 args))
        (rest  (cddr args)))          ; preserve optional DELETE as-is
    (append (list (min start end) (max start end)) rest)))

(with-eval-after-load 'agent-shell
  (advice-add
   'agent-shell--filter-buffer-substring :filter-args
   'agent-shell--normalize-filter-range
   '((name . agent-shell/normalize-filter-range))))

;; --------------------------------------------------------------------------
;; :: AGENT-SHELL TURN-COMPLETE NOTIFICATION
;; --------------------------------------------------------------------------
(setq agent-shell--debug nil)

(defun agent-shell--buffer-focused-p (buffer)
  "Return non-nil if BUFFER is visible in the frame that has input focus.
Any window of the focused frame that displays BUFFER counts -- it need
not be the selected window.  So if you are editing in one window while
the Agent Shell is visible in another window of the same focused frame,
this returns non-nil.  It returns nil when BUFFER is hidden, or shown
only in a frame that lacks input focus (e.g. while you are in the
browser)."
  (seq-some
   (lambda (frame)
     (and (frame-focus-state frame)
          (get-buffer-window buffer frame)))
   (frame-list)))

(defun agent-shell-announce-turn-complete (_event)
  "Announce via `vox' when an Agent Shell turn completes.
Runs inside the session buffer (see `agent-shell--emit-event'), so
`current-buffer' is the shell.  The announcement is skipped only while
you are actively viewing the session; it still fires when the frame is
visible but unfocused (e.g. while you are in the browser).  The
announced directory is the session CWD (the DIR passed to `claude')."
  (let ((buffer (current-buffer)))
    (unless (agent-shell--buffer-focused-p buffer)
      (let* ((dir (file-name-nondirectory
                   (directory-file-name (agent-shell-cwd))))
             (msg (format "Agent %s czeka" dir)))
        (start-process "agent-shell-vox" nil
                       "vox" "now" "--message" msg)))))

(defun agent-shell-subscribe-turn-complete ()
  (interactive)
  "Subscribe the current Agent Shell buffer to the `turn-complete' event."
  (agent-shell-subscribe-to
   :shell-buffer (current-buffer)
   :event 'turn-complete
   :on-event #'agent-shell-announce-turn-complete))

(add-hook 'agent-shell-mode-hook 'agent-shell-subscribe-turn-complete)

;; --------------------------------------------------------------------------
;; :: AGENT-SHELL PERMISSION SYSTEM
;; --------------------------------------------------------------------------

;; ---- Quote-aware shell tokenizer -----------------------------------------
;;
;; The path-extraction and compound-split helpers below used to lean on naive
;; `split-string', which is blind to shell quoting.  A command such as
;;   sed -E 's/\x1b\[[0-9;]*m//g; s|https?://[^ ]*||g; s/^[0-9T:.Z-]+ //'
;; made them emit stray "paths" (e.g. the //' fragment of the sed script) and
;; forced a permission prompt for an otherwise-allowed command.  These helpers
;; instead parse the command the way a shell does: quotes group, backslash
;; escapes, and heredoc bodies are opaque -- so regex/URL/path-looking text
;; inside quotes or heredocs is never mistaken for a real argument.
;;
;; `agent-shell--shell-token-re' and `agent-shell--shell-quoted-re' are
;; generated from the jQuery Terminal command tokenizer by regex2elisp.js in
;; the dotfiles repo root; regenerate with `node regex2elisp.js' rather than
;; hand-editing them.  Emacs regexps have no lookahead, so the original's
;; `/regex/' literal branch is intentionally omitted: in POSIX shell an
;; unquoted space always separates arguments, so a bare /foo bar/ is not a
;; single token anyway.  Heredocs are handled by a separate strip pass instead
;; of a token-regex branch, because a heredoc terminator is line-anchored and
;; back-referenced -- folding that into the token regex risks catastrophic
;; backtracking in Emacs's engine.

(defconst agent-shell--shell-token-re
  "\\(?:\"[^\"\\\\]*\\(?:\\\\\\(?:.\\|\n\\)[^\"\\\\]*\\)*\"\\|'[^'\\\\]*\\(?:\\\\\\(?:.\\|\n\\)[^'\\\\]*\\)*'\\|`[^`\\\\]*\\(?:\\\\\\(?:.\\|\n\\)[^`\\\\]*\\)*`\\|\\(?:\\\\[[:space:]]\\|[^[:space:]]\\)\\)+"
  "Match a single shell token.
Either a quoted string (\"...\", '...', `...`) or a run of
backslash-escaped-space / non-space characters.")

(defconst agent-shell--shell-quoted-re
  "\\(?:\"[^\"\\\\]*\\(?:\\\\\\(?:.\\|\n\\)[^\"\\\\]*\\)*\"\\|'[^'\\\\]*\\(?:\\\\\\(?:.\\|\n\\)[^'\\\\]*\\)*'\\|`[^`\\\\]*\\(?:\\\\\\(?:.\\|\n\\)[^`\\\\]*\\)*`\\)"
  "Match a single double-, single-, or back-quoted string.")

(defconst agent-shell--heredoc-re
  (concat "<<-?[ \t]*['\"]?"               ; << , optional - , spaces, open quote
          "\\([A-Za-z_][A-Za-z0-9_]*\\)"   ; group 1: delimiter word
          "['\"]?[^\n]*"                   ; close quote + rest of the operator line
          "\n\\(?:.\\|\n\\)*?"             ; body (any char incl. newline, lazy)
          "\n[ \t]*\\1[ \t]*$")            ; terminator line = delimiter word
  "Match a heredoc from its `<<' operator through the terminator line.")

(defun agent-shell--strip-heredocs (command)
  "Remove heredoc bodies and terminator lines from COMMAND.
The `<<DELIM' operator and the rest of its line are kept; the body and
the terminator line are dropped so their opaque contents are not parsed
as shell paths or sub-commands.  An unterminated heredoc is left as-is."
  (replace-regexp-in-string
   agent-shell--heredoc-re
   (lambda (m) (save-match-data (substring m 0 (string-match "\n" m))))
   command t t))

(defun agent-shell--tokenize-command (command)
  "Split COMMAND into shell tokens, quote-aware.
Heredoc bodies are stripped first; quoted strings stay intact as single
tokens."
  (let ((str (agent-shell--strip-heredocs command))
        (re (concat "[[:space:]]*\\(" agent-shell--shell-token-re "\\)"))
        (start 0) tokens)
    (while (and (< start (length str))
                (string-match re str start))
      (push (match-string 1 str) tokens)
      (setq start (match-end 0)))
    (nreverse tokens)))

(defun agent-shell--mask-quoted (command)
  "Replace quoted spans in COMMAND with equal-length filler.
Length and offsets are preserved, so separators inside quotes vanish
while positions into COMMAND stay valid for slicing."
  (replace-regexp-in-string
   agent-shell--shell-quoted-re
   (lambda (m) (save-match-data (make-string (length m) ?x)))
   command t t))

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
  "Split a compound COMMAND into individual sub-commands.
Splits on `;', `&&', `||', `|' and `&' that fall outside quotes and
heredoc bodies, so separators inside quoted arguments (regex alternation,
sed scripts) do not create spurious sub-commands."
  (let* ((stripped (agent-shell--strip-heredocs command))
         (masked (agent-shell--mask-quoted stripped))
         (start 0) (pos 0) subs)
    (while (string-match "[;|&]+" masked pos)
      ;; Capture the match bounds before `string-trim' -- it calls
      ;; `string-match' internally and would clobber the match data,
      ;; leaving `pos' stuck and looping forever.
      (let ((mb (match-beginning 0)) (me (match-end 0)))
        (let ((sub (string-trim (substring stripped start mb))))
          (unless (zerop (length sub)) (push sub subs)))
        (setq start me pos me)))
    (let ((sub (string-trim (substring stripped start))))
      (unless (zerop (length sub)) (push sub subs)))
    (nreverse subs)))

(defun agent-shell--permission-command-match-p (command patterns)
  "Return non-nil if any sub-command in COMMAND matches PATTERNS.
Splits compound commands on `;', `&&', `||', and `|'."
  (let ((sub-commands (agent-shell--split-compound-command command)))
    (seq-some
     (lambda (cmd)
       (agent-shell--permission-single-command-match-p cmd patterns))
     sub-commands)))

(defun agent-shell--extract-command-paths (command)
  "Extract file-path arguments from COMMAND string.
Uses quote-aware tokenization so path-looking fragments inside quoted
strings or heredoc bodies (regexes, URLs, sed scripts) are ignored.
Recognizes absolute, ~, and explicit relative (./ ../) paths.  Tokens
that are themselves quoted literals are skipped -- as before, only bare
path arguments are checked."
  (let (paths)
    (dolist (token (agent-shell--tokenize-command command))
      (unless (memq (aref token 0) '(?\" ?\' ?\`))
        (when (or (string-prefix-p "/" token)
                  (string-prefix-p "~" token)
                  (string-prefix-p "./" token)
                  (string-prefix-p "../" token))
          (push (expand-file-name token) paths))))
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

(defun agent-shell-announce-permission-request ()
  "Announce via `vox' when an Agent Shell session asks for permission.
The permission responder is invoked inside the session buffer, so
`current-buffer' is the shell.  As with `agent-shell-announce-turn-complete',
the announcement is skipped only while you are actively viewing the session;
it still fires when the frame is visible but unfocused.  The announced
directory is the session CWD."
  (let ((buffer (current-buffer)))
    (unless (agent-shell--buffer-focused-p buffer)
      (let* ((dir (file-name-nondirectory
                   (directory-file-name (agent-shell-cwd))))
             (msg (format "Agent %s pyta o pozwolenie" dir)))
        (start-process "agent-shell-vox" nil
                       "vox" "now" "--message" msg)))))

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
           (_ (if agent-shell--debug
                  (message "Permission check: kind=%s title=%s cwd=%s" kind title cwd)))
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
        t)
       ;; Neither auto-denied nor auto-allowed: the request falls through to
       ;; the interactive UI, i.e. the agent is asking you.  Notify (unless
       ;; you are looking at the session) and return nil so the UI still runs.
       (t
        (agent-shell-announce-permission-request)
        nil)))))

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
(defmacro maybe-var (var)
  `(if (boundp ',var) ,var ""))

(setq agent-shell-mcp-servers
      `(((name . "context7")
         (type . "http")
         (url . "https://mcp.context7.com/mcp")
         (headers . (((name . "CONTEXT7_API_KEY")
                      (value . ,(maybe-var CONTEXT_7_API_KEY))))))
        ((name . "brave-search")
         (command . "npx")
         (args . ("-y" "@brave/brave-search-mcp-server"))
         (env . (((name . "BRAVE_API_KEY")
                  (value . ,(maybe-var BRAVE_SEARCH_API_KEY))))))
        ((name . "browserstack")
         (command . "npx")
         (args . ("-y"  "@browserstack/mcp-server@latest"))
         (env . (((name . "BROWSERSTACK_USERNAME")
                  (value . ,(maybe-var BROWSER_STACK_USERNAME)))
                 ((name . "BROWSERSTACK_ACCESS_KEY")
                  (value . ,(maybe-var BROWSER_STACK_ACCESS_KEY))))))
        ((name . "DeepWiki")
         (type . "http")
         (url . "https://mcp.deepwiki.com/mcp")
         (headers . nil))
        ((name . "chrome-devtools")
         (command . "npx")
         (args . ("-y" "chrome-devtools-mcp@latest" "--browser-url=http://127.0.0.1:9222"))
         (env . nil))
        ((name . "code-index")
         (command . "uvx")
         (args . ("code-index-mcp"))
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
         (command . "playwright-mcp")
         (args . ("--name"
                  "Playwright-Headless"
                  "--config"
                  ,(expand-file-name "~/projects/jcubic/dotfiles/emacs/playwright-mcp.json")))
         (env . nil))
        ((name . "playwright-browser")
         (command . "playwright-mcp")
         (args . ("--name" "Playwright-Browser" "--extension"))
         (env . nil))
        ((name . "wikidata")
         (type . "http")
         (url . "https://wd-mcp.wmcloud.org/mcp/")
         (headers . nil))
        ((name . "specification-website")
         (type . "http")
         (url . "https://mcp.specification.website/mcp")
         (headers . nil))))

(setq agent-shell-permission-responder-function
      (agent-shell-make-permission
       '((allow
          (read . ("~/projects/" "~/.claude/" "~/.agent-shell/" "~/.mutimon/"
                   "~/.horavox/" "~/.emacs" "~/.bashrc" "~/bin/" "//tmp/"
                   "/usr/local/share/emacs/" "~/.local/share/icons/" "~/.emacs.d/"
                   "//dev/" "//dev/null" "~/.config/nvm/"))
          (write . ("~/.mutimon/" "~/.horavox/" "//tmp/" "/"
                    "~/.clarity-icons/" "~/.local/share/icons/Clarity/"))
          (execute . ("*"
                      "git checkout *" "git status *" "git diff *"
                      "git clone *" "git ls-tree *" "git ls-files *"
                      "git log *" "git show *" "git branch *" "git ls-remote"
                      "git reflog *" "git rev-parse *" "git remote -v *"
                      "git config *" "git grep *"))
          (mcp . ("*"))
          (fetch . ("*")))
         (deny
          (execute . ()))
         (ask
          (execute . ("sudo *" "ssh *" "git *" "kill *" "emacsclient *"
                      "node -e *" "python -c *" "bash -c *" "sh -c *"))
          (mcp . ("playwright-browser"))))))
