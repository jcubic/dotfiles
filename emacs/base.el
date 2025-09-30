(toggle-debug-on-error)

(setq vterm-max-scrollback 10000)
(setq warning-minimum-level :emergency)
(setq ring-bell-function 'ignore)
(setq debug-on-error 1)
(setq confirm-kill-processes nil)

(global-auto-revert-mode t)

(setenv "NODE_NO_READLINE" "1")
(setenv "BROWSER" "brave-browser")

