;;; -*- lexical-binding: nil; -*-

(toggle-debug-on-error)

(setq vterm-max-scrollback 10000)
(setq warning-minimum-level :emergency)
(setq ring-bell-function 'ignore)
(setq debug-on-error 1)
(setq confirm-kill-processes nil)
(setq scroll-error-top-bottom t)
(setq require-final-newline t)

;; --------------------------------------------------------------------------
(require 'server)

(unless (server-running-p)
  (server-start))
;; --------------------------------------------------------------------------
;; Theme
;; --------------------------------------------------------------------------
(setq dracula-alternate-mode-line-and-minibuffer t)

(load-theme 'dracula t)

(with-eval-after-load 'dracula-theme
  (set-face-attribute 'mode-line nil :background "#3FB6ED" :foreground "#ffffff" :box nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :background "#44475a" :foreground "#8B8B8B" :box nil :height 110)

  (set-face-attribute 'isearch nil :background nil :foreground "#f1fa8c" :weight 'bold :underline t)
  (set-face-attribute 'lazy-highlight nil :background nil :foreground "#bd93f9" :underline t)
  (set-face-attribute 'match nil :background nil :foreground "#f1fa8c" :weight 'bold)

  (set-face-attribute 'show-paren-match nil :background "#ff8c00" :foreground "black" :weight 'bold)

  (set-face-attribute 'minibuffer-prompt nil :background nil :foreground "#f1fa8c" :weight 'bold)
  (set-face-attribute 'minibuffer-prompt nil :box nil))

(with-eval-after-load 'company
  (set-face-attribute 'company-tooltip-selection nil
                      :background "#f1fa8c" :foreground "#282a36"))
;; --------------------------------------------------------------------------
(require 'moody)
(moody-replace-mode-line-front-space)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)

;; --------------------------------------------------------------------------
;; :: DEFAULT BROWSER
;; --------------------------------------------------------------------------
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "helium")

(setenv "BROWSER" "helium")

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
(setenv "LC_ALL" nil)

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
;; :: SERVER mode
;; --------------------------------------------------------------------------
(server-mode 1)

;; --------------------------------------------------------------------------
;; :: GIT GUTTER
;; --------------------------------------------------------------------------
(require 'git-gutter)

(global-git-gutter-mode +1)

(setq git-gutter:disabled-modes '(agent-shell-mode term-mode))

(defun gutter-refresh ()
  (interactive))

(add-to-list 'git-gutter:update-commands 'gutter-refresh)

(global-set-key (kbd "C-c C-g") 'gutter-refresh)

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

(setq inferior-js-program-command "node --interactive")

(setenv "NODE_NO_READLINE" "1")

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
(setq kill-read-only-ok t)
(setq mouse-drag-copy-region 'non-empty)
(setq x-select-enable-clipboard-manager nil)

(if (fboundp 'x-cut-buffer-or-selection-value)
    (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (setq interprogram-paste-function 'x-selection-value))
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


(global-set-key (kbd "C-c f") 'fill-paragraph)

;; --------------------------------------------------------------------------
;; Custom keyboard prefix
;; --------------------------------------------------------------------------
(define-prefix-command 'my-prefix-map)
(global-set-key (kbd "C-z") 'my-prefix-map)

;; --------------------------------------------------------------------------
;; FIX: mouse-save-then-kill empty-region kill clobbers the kill ring
;; --------------------------------------------------------------------------
;; In stock Emacs (checked 30.2 and 32.0.50) the `mouse-drag-copy-region'
;; = `non-empty' guard in the THIRD cond-branch of `mouse-save-then-kill' is
;; inverted vs the drag path and the docstring:
;;   drag path  (mouse.el:1660):  (or (not (eq ... 'non-empty)) (/= beg end))
;;   this branch (mouse.el:2391): (or (not (eq ... 'non-empty)) (not (/= (mark t) (point))))
;; The stray `not' makes it copy ONLY when the region is empty, so a right-click
;; on a degenerate/empty region runs (kill-new "") and clobbers the kill ring
;; (and the X clipboard via interprogram-cut) -- which breaks mouse copy in
;; read-only buffers (agent-shell, *Backtrace*, ...).  Needs `mouse-drag-copy-region'
;; = `non-empty' (set above) to take effect.  Verbatim copy of the 32.0.50
;; definition with the single `not' removed (see FIX marker).  Delete once fixed
;; upstream.
(defun mouse-save-then-kill (click)
  "Set the region according to CLICK; the second time, kill it.
CLICK should be a mouse click event.

If the region is inactive, activate it temporarily.  Set mark at
the original point, and move point to the position of CLICK.

If the region is already active, adjust it.  Normally, do this by
moving point or mark, whichever is closer, to CLICK.  But if you
have selected whole words or lines, move point or mark to the
word or line boundary closest to CLICK instead.

If `mouse-drag-copy-region' is non-nil, this command also saves the
new region to the kill ring (replacing the previous kill if the
previous region was just saved to the kill ring).

If this command is called a second consecutive time with the same
CLICK position, kill the region (or delete it
if `mouse-drag-copy-region' is non-nil)."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((posn     (event-start click))
         (click-pt (posn-point posn))
         (window   (posn-window posn))
         (buf      (window-buffer window))
         ;; Don't let a subsequent kill command append to this one.
         (this-command this-command)
         ;; Check if the user has multi-clicked to select words/lines.
         (click-count
          (if (and (eq mouse-selection-click-count-buffer buf)
                   (with-current-buffer buf (mark t)))
              mouse-selection-click-count
            0)))
    (cond
     ((not (numberp click-pt)) nil)
     ;; If the user clicked without moving point, kill the region.
     ;; This also resets `mouse-selection-click-count'.
     ((and (eq last-command 'mouse-save-then-kill)
           (eq click-pt mouse-save-then-kill-posn)
           (eq window (selected-window)))
      (if mouse-drag-copy-region
          ;; Region already saved in the previous click;
          ;; don't make a duplicate entry, just delete.
          (funcall region-extract-function 'delete-only)
        (kill-region (mark t) (point) 'region))
      (setq mouse-selection-click-count 0)
      (setq mouse-save-then-kill-posn nil))

     ;; Otherwise, if there is a suitable region, adjust it by moving
     ;; one end (whichever is closer) to CLICK-PT.
     ((or (with-current-buffer buf (region-active-p))
          (and (eq window (selected-window))
               (mark t)
               (or (and (eq last-command 'mouse-save-then-kill)
                        mouse-save-then-kill-posn)
                   (and (memq last-command '(mouse-drag-region
                                             mouse-set-region))
                        (or mark-even-if-inactive
                            (not transient-mark-mode))))))
      (select-window window)
      (let* ((range (mouse-start-end click-pt click-pt click-count)))
        (if (< (abs (- click-pt (mark t)))
               (abs (- click-pt (point))))
            (set-mark (car range))
          (goto-char (nth 1 range)))
        (setq deactivate-mark nil)
        (mouse-set-region-1)
        (when mouse-drag-copy-region
          ;; Region already copied to kill-ring once, so replace.
          ;; FIX: only replace when the EXTRACTED text is non-empty (see the
          ;; third-branch note); otherwise a stripped/empty extraction does
          ;; (kill-new "" t) and clobbers the ring top.
          (let ((s (funcall region-extract-function nil)))
            (when (> (length s) 0)
              (kill-new s t))))
        ;; Arrange for a repeated mouse-3 to kill the region.
        (setq mouse-save-then-kill-posn click-pt)))

     ;; Otherwise, set the mark where point is and move to CLICK-PT.
     (t
      (select-window window)
      (mouse-set-mark-fast click)
      (let ((before-scroll (with-current-buffer buf point-before-scroll)))
        (if before-scroll (goto-char before-scroll)))
      (exchange-point-and-mark)
      (mouse-set-region-1)
      (when mouse-drag-copy-region
        ;; FIX: upstream's guard here is `(not (/= (mark t) (point)))', which is
        ;; inverted (copies only when EMPTY).  But even the position test isn't
        ;; enough: buffers whose `filter-buffer-substring-function' strips
        ;; invisible/overlay content (agent-shell; *Backtrace* via
        ;; `backtrace--filter-visible') return "" for a non-empty range, so
        ;; guard on the EXTRACTED string being non-empty instead.
        (let ((s (filter-buffer-substring (mark t) (point))))
          (when (> (length s) 0)
            (kill-new s))))
      (setq mouse-save-then-kill-posn click-pt)))))

;; --------------------------------------------------------------------------
;; FIX: backtrace--filter-visible returns "" for reversed ranges
;; --------------------------------------------------------------------------
;; Same bug as agent-shell's filter: as a `filter-buffer-substring-function' it
;; must accept BEG/END in either order, but `(while (< beg end) ...)' returns ""
;; when BEG > END -- so mouse copy from *Backtrace* silently yields nothing
;; depending on selection direction.  Normalize with min/max.  `backtrace' is
;; loaded on demand, so wrap in `with-eval-after-load' to win over the stock
;; definition.  Remove once fixed upstream.
(with-eval-after-load 'backtrace
  (defun backtrace--filter-visible (beg end &optional _delete)
    "Return the visible text between BEG and END."
    (let ((beg (min beg end))
          (end (max beg end))
          (result ""))
      (while (< beg end)
        (let ((next (next-single-char-property-change beg 'invisible)))
          (unless (get-char-property beg 'invisible)
            (setq result (concat result (buffer-substring beg (min end next)))))
          (setq beg next)))
      result)))
