(toggle-debug-on-error)

(setq vterm-max-scrollback 10000)
(setq warning-minimum-level :emergency)
(setq ring-bell-function 'ignore)
(setq debug-on-error 1)
(setq confirm-kill-processes nil)

(global-auto-revert-mode t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setenv "NODE_NO_READLINE" "1")
(setenv "BROWSER" "brave-browser")
(setq inferior-js-program-command "node --interactive")

;; fix bug with history and killed buffers
(add-hook 'kill-buffer-hook
          (lambda ()
            (interactive)
            (setq buffer-name-history (delete (buffer-name)
                                              buffer-name-history))))

(global-set-key (kbd "C-c C-p") 'fill-paragraph)

(global-set-key (kbd "<C-mouse-1>") (lambda (e)
                                      (interactive "e")
                                      (mouse-set-point e)))

(global-set-key (kbd "<C-down-mouse-1>")
                (lambda (e)
                  (interactive "e")
                  (message "C+<mouse>")))
