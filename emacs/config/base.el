(toggle-debug-on-error)

(setq vterm-max-scrollback 10000)
(setq warning-minimum-level :emergency)
(setq ring-bell-function 'ignore)
(setq debug-on-error 1)
(setq confirm-kill-processes nil)
(setq scroll-error-top-bottom t)
(setq require-final-newline t)

;; --------------------------------------------------------------------------
;; :: DEFAULT BROWSER
;; --------------------------------------------------------------------------
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "brave-browser")

;; --------------------------------------------------------------------------
;; :: NOTIFICATIONS
;; --------------------------------------------------------------------------
(setq notify-method 'notify-via-libnotify)
(fset 'yes-or-no-p 'y-or-n-p)
(setq next-line-add-newlines nil)

;; --------------------------------------------------------------------------
;; :: DISABLED COMMANDS
;; --------------------------------------------------------------------------
(setq disabled-command-function nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; --------------------------------------------------------------------------
;; :: UTF-8 CHARSET
;; --------------------------------------------------------------------------
(set-language-environment 'UTF-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; --------------------------------------------------------------------------
;; :: MAIN UI
;; --------------------------------------------------------------------------
(mouse-wheel-mode t)
(menu-bar-mode -1)
(show-paren-mode +1)
(tool-bar-mode -1)
(column-number-mode t)
(global-auto-revert-mode t)
(tooltip-mode -1)

;; frame config
(setq frame-title-format
      '("GNU Emacs: "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; time
(setq display-time-interval 1)
(setq display-time-format "%T")
(display-time-mode)

;; transparenct frame
(set-frame-parameter nil 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; bigger font
(set-face-attribute 'default nil :height 110)

;; no blinking
(and (fboundp 'blink-cursor-mode) (blink-cursor-mode (- (*) (*) (*))))
;; no scrollbar
(scroll-bar-mode -1)

;; --------------------------------------------------------------------------
;; :: TABS VS SPACES
;; --------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun tab-mode ()
  "toggle tabs/spaces"
  (interactive)
  (message (concat "indent-tabs-mode "
                   (if indent-tabs-mode "disabled" "enabled")))
  (setq indent-tabs-mode (not indent-tabs-mode)))

(global-set-key (kbd "C-c t") 'tab-mode)

(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setenv "NODE_NO_READLINE" "1")
(setenv "BROWSER" "brave-browser")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "brave-browser")

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

;; http://emacs.stackexchange.com/questions/22647/reload-single-file-in-every-window
(defun reload ()
  "Revert buffer, then restore previous position."
  (interactive)
  (let ((pt  (point)))
    (revert-buffer t t)
    (goto-char pt)))

;; --------------------------------------------------------------------------
;; :: CURSOR SETUP
;; --------------------------------------------------------------------------
(require 'multiple-cursors)
(require 'bar-cursor)

(bar-cursor-mode)
(setq cursor-type 'bar)
(setq-default cursor-type 'bar)
(set-face-attribute 'mc/cursor-bar-face nil :height 3)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-r") 'mc/edit-lines)

;; --------------------------------------------------------------------------
;; :: CLIPBOARD/YANKING
;; --------------------------------------------------------------------------
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region t)
(setq x-select-enable-clipboard-manager nil)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq interprogram-paste-function 'x-selection-value)
(delete-selection-mode t)

(add-to-list 'yank-excluded-properties 'font)
(add-to-list 'yank-excluded-properties 'font-lock-face)

(defun swap-region-ring (&optional arg)
  "replace selected text with the one from kill ring"
  (interactive "*P")
  (remove-region)
  (yank arg))

(defun remove-region ()
  (interactive)
  (backward-delete-char (- (point) (mark))))

(global-set-key (kbd "C-c y") 'swap-region-ring)
(global-set-key (kbd "C-c C-c") 'remove-region)
(define-key isearch-mode-map "\C-y" 'isearch-yank-kill)
