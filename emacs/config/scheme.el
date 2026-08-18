(require 'cmuscheme)
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq scheme-program-name "/home/kuba/projects/jcubic/scheme/lips/master/bin/lips.js")
(add-hook 'scheme-mode-hook 'turn-on-font-lock)

(defun lisp ()
  "call run-lisp with clisp intepreter."
  (interactive)
  (run-lisp "/usr/bin/clisp -q"))

(defun scheme ()
  "call run-scheme with mit scheme interpreter."
  (interactive)
  (run-scheme "/usr/bin/guile3.0"))

(defun kawa ()
  "call run-scheme with kawa scheme interpreter."
  (interactive)
  (run-scheme "/usr/bin/kawa"))

(defun chicken ()
  "call run-scheme with Chicken interpreter."
  (interactive)
  (run-scheme "/usr/bin/csi"))

(defun guile ()
  "call run-scheme with guile interpreter."
  (interactive)
  (run-scheme "/usr/bin/guile3.0"))

(defun lips ()
  "call run-scheme with LIPS interpreter."
  (interactive)
  (run-scheme "/usr/bin/env lips -q"))

(defun gambit ()
  "call run-scheme with guile interpreter."
  (interactive)
  (run-scheme "/usr/bin/env gsi"))

(defun my-scheme-send-newline (&rest _)
  "Terminate the current line in the inferior Scheme buffer before a
buffer-send, so the next prompt lands on its own line instead of being
appended right after the current prompt (e.g. `lips> lips>')."
  (let ((proc (ignore-errors (scheme-proc))))
    (when (and proc (process-live-p proc))
      (with-current-buffer (process-buffer proc)
        (save-excursion
          (goto-char (process-mark proc))
          ;; don't add a blank line if already at bol
          (unless (bolp)
            (let ((inhibit-read-only t))
              ; ; moves process-mark past the newline
              (insert-before-markers "\n"))))))))

(advice-add 'scheme-send-region :before #'my-scheme-send-newline)

(defun comint-send-input-indent ()
  (interactive)
  (let ((parens (or (car (syntax-ppss)) 0)))
    (if (zerop parens)
        (comint-send-input)
      (newline-and-indent))))

(define-key scheme-mode-map (kbd "C-x C-e") #'scheme-send-last-sexp)
(define-key inferior-scheme-mode-map (kbd "RET") #'comint-send-input-indent)

(defun inferior-scheme ()
  (setq show-trailing-whitespace nil))

(add-hook 'inferior-scheme-mode-hook #'inferior-scheme)

;; Scheme Regex #/.../ handling


(defun scheme-regex-patch ()
   (setq-local
    syntax-propertize-function
    (lambda (beg end)
      (goto-char beg)
      (scheme-syntax-propertize-sexp-comment2 end)
      (scheme-syntax-propertize-regexp end)
      (funcall
       (syntax-propertize-rules
        ("\\(#\\);" (1 (prog1 "< cn"
                         (scheme-syntax-propertize-sexp-comment2 end))))
        ("\\(#\\)/" (1 (when (null (nth 8 (save-excursion
                                            (syntax-ppss
                                             (match-beginning 0)))))
                         (put-text-property
                          (match-beginning 1)
                          (match-end 1)
                          'syntax-table (string-to-syntax "|"))
                         (scheme-syntax-propertize-regexp end)
                         nil)
                       )))
       (point) end))))

(defun scheme-syntax-propertize-sexp-comment2 (end)
  (let ((state (syntax-ppss)))
    (when (eq 2 (nth 7 state))
      ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
      (condition-case nil
          (progn
            (goto-char (+ 2 (nth 8 state)))
            ;; FIXME: this doesn't handle the case where the sexp
            ;; itself contains a #; comment.
            (forward-sexp 1)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))
        (scan-error (goto-char end))))))

(defun scheme-syntax-propertize-regexp (end)
  (let* ((state (syntax-ppss))
         (within-str (nth 3 state))
         (start-delim-pos (nth 8 state)))
    (when (and within-str
               (char-equal ?# (char-after start-delim-pos)))
      (while
          (and
           (re-search-forward "/" end 'move)
           (eq -1
               (% (save-excursion
                    (backward-char)
                    (skip-chars-backward "\\\\")) 2))))
      (when (< (point) end)
        (progn
          (put-text-property
           (match-beginning 0)
           (match-end 0)
           'syntax-table (string-to-syntax "|")))))))

(add-hook 'scheme-mode-hook 'scheme-regex-patch)

(setq comint-input-ignoredups t)

(setq comint-prompt-read-only t)

(add-hook 'comint-mode-hook
          (lambda()
            (setq truncate-lines 1)))
