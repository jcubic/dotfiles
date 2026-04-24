(require 'acp)
(require 'agent-shell)

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

(setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))

(setq agent-shell-anthropic-default-model-id "claude-opus-4-6")

(defun claude (dir)
  (interactive (list (read-directory-name "Directory: " default-directory)))
  (let ((default-directory dir))
    (agent-shell--new-shell :location dir)))

(setq agent-shell-mcp-servers
      '(((name . "context7")
         (type . "http")
         (url . "https://mcp.context7.com/mcp")
         (headers . (((name . "CONTEXT7_API_KEY")
                      (value . "ctx7sk-37e7f1db-ad21-4e1f-9b76-87f52248fed2")))))
        ((name . "DeepWiki")
         (type . "http")
         (url . "https://mcp.deepwiki.com/mcp")
         (headers . nil))
        ((name . "chrome-devtools")
         (command . "npx")
         (args . ("-y" "chrome-devtools-mcp@latest"))
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
         (args . ("-y" "@playwright/mcp@latest"))
         (env . nil))))

(defun agent-shell-hook ()
  (interactive)
  (local-set-key [C-M-tab] 'previous-buffer-same-mode)
  (local-set-key [C-tab] 'next-buffer-same-mode)
  (local-set-key [s-tab] 'agent-shell-cycle-session-mode)
  (local-set-key [C-up] 'backward-paragraph)
  (local-set-key [C-down] 'forward-paragraph))

(add-hook 'agent-shell-mode 'agent-shell-hook)

(setq agent-shell-permission-responder-function
      (lambda (permission)
        (let* ((tool-call (map-elt permission :tool-call))
               (kind (map-elt tool-call :kind))
               (title (or (map-elt tool-call :title) ""))
               (allow-choice (seq-find
                              (lambda (opt)
                                (equal (map-elt opt :kind) "allow_once"))
                              (map-elt permission :options)))
               (should-allow
                (cond
                 ;; Read/write files — allow
                 ((member kind '("read" "write")) t)
                 ;; Bash commands
                 ((equal kind "execute")
                  (and
                   ;; Block sudo and ssh
                   (not (string-match-p "\\`sudo " title))
                   (not (string-match-p "\\`ssh " title))
                   ;; Allow safe git commands, block other git
                   (not (and (string-match-p "\\`git " title)
                             (not (string-match-p
                                   (concat "\\`git "
                                           "\\(checkout\\|status\\|diff"
                                           "\\|log\\|show\\|branch"
                                           "\\|remote -v\\|ls-files\\)")
                                   title))))))
                 ;; Everything else — allow
                 (t t))))
          (when (and should-allow allow-choice)
            (funcall (map-elt permission :respond)
                     (map-elt allow-choice :option-id))
            t))))
