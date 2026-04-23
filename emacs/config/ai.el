(require 'acp)
(require 'agent-shell)

(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

(setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))

(setq agent-shell-anthropic-default-model-id "claude-opus-4-6")

(defun claude ()
  (interactive)
  (agent-shell-new-shell))

(setq agent-shell-mcp-servers
      '(((name . "context7")
         (type . "http")
         (url . "https://mcp.context7.com/mcp")
         (headers . (((name . "CONTEXT7_API_KEY")
                      (value . "ctx7sk-37e7f1db-ad21-4e1f-9b76-87f52248fed2")))))
        ((name . "DeepWiki")
         (type . "http")
         (url . "https://mcp.deepwiki.com/mcp")
         (headers . ()))
        ((name . "chrome-devtools")
         (command . "npx")
         (args . ("-y" "chrome-devtools-mcp@latest"))
         (env . ()))
        ((name . "chakra-ui")
         (command . "npx")
         (args . ("-y" "@chakra-ui/react-mcp"))
         (env . ()))
        (name . "reader")
        (command . "npx")
        (args . ("-y" "@nicepkg/jina-reader-mcp"))
        (env . ()))
        ((name . "playwright")
         (command . "npx")
         (args . ("-y" "@playwright/mcp@latest"))
         (env . ()))))

(defun agent-shell-hook ()
  (interactive)
  (local-set-key [C-M-tab] 'previous-buffer-same-mode)
  (local-set-key [C-tab] 'next-buffer-same-mode)
  (local-set-key [s-tab] 'agent-shell-cycle-session-mode))

(add-hook 'agent-shell-mode 'agent-shell-hook)
