(require 'highlight-current-line)
(require 'highlight-chars)
(require 'highlight-symbol)

(setq highlight-current-line-mode-list '(php-mode
                                         js-mode
                                         javascript-mode
                                         c-mode-common
                                         sql-mode
                                         lisp-mode
                                         emacs-lisp-mode
                                         python-mode
                                         yaml-mode
                                         lisp-interaction-mode
                                         markdown-mode
                                         nxml-mode
                                         web-mode
                                         scheme-mode))

(setq highlight-symbol-highlight-single-occurrence nil)
(highlight-symbol-mode)
(setq highlight-symbol-idle-delay 0.5)
(setq highlight-symbol-disable '(term-mode
                                 erc-mode
                                 minibuffer-mode
                                 minibuffer-inactive-mode
                                 nodejs-repl-mode))

(setq prevent-highlight-symbol-mode nil) ;;'(term-mode erc-mode))

(defun highlight-selected-modes ()
    ;; if on the list
    (if (memql major-mode highlight-current-line-mode-list)
        (highlight-current-line-minor-mode))
    ;; if not on the list
    (if (and (null (memql major-mode highlight-symbol-disable))
             (not prevent-highlight-symbol-mode))
        (highlight-symbol-mode)))


(add-hook 'after-change-major-mode-hook 'highlight-selected-modes)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

(defun hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(add-hook 'erc-mode-hook 'hide-trailing-whitespace)
(add-hook 'shell-mode-hook 'hide-trailing-whitespace)
(add-hook 'vterm-mode-hook 'hide-trailing-whitespace)
(add-hook 'agent-shell-mode 'hide-trailing-whitespace)


;; Show Red tabs


(setq highlight-chars-disable '(term-mode
                                agent-shell-mode
                                erc-mode
                                fundamental-mode
                                grep-mode
                                nodejs-repl-mode))

(add-hook 'change-major-mode-hook
          (lambda ()
            (if (null (memql major-mode highlight-chars-disable))
                (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
              (remove-hook 'font-lock-mode-hook 'hc-highlight-tabs))))

(setq whitespace-style '(face tabs))
(setq tab-face (make-face 'tab-face))
(set-face-background 'tab-face "red")
(setq whitespace-tab 'tab-face)

(setq whitespace-mode-disable '(term-mode
                                erc-mode
                                shell-mode
                                nodejs-repl-mode
                                vterm-mode))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (if (null (memql major-mode whitespace-mode-disable))
                (whitespace-mode))))
