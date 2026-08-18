(require 'cmuscheme)
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq scheme-program-name "/home/kuba/projects/jcubic/scheme/lips/master/bin/lips.js")
(add-hook 'scheme-mode-hook 'turn-on-font-lock)

;; -- multiple named Scheme REPLs --------------------------------------------
;; Run each interpreter in its own *scheme<NAME>* buffer so several REPLs can
;; live at the same time.  When you evaluate from a scheme-mode buffer the
;; target is the REPL that is *visible in the selected frame* (see the
;; `scheme-proc' advice below); if none is visible it falls back to
;; `scheme-buffer', the most recently started REPL.

(defun run-scheme-named (name cmd)
  "Run inferior Scheme CMD in its own buffer named *scheme<NAME>*.
Like `run-scheme', but several REPLs can coexist.  With a prefix
argument, edit the command line before running it."
  (interactive
   (list (read-string "REPL name: ")
         (if current-prefix-arg
             (read-string "Run Scheme: " scheme-program-name)
           scheme-program-name)))
  (let ((bufname (format "*scheme<%s>*" name)))
    (unless (comint-check-proc bufname)
      (let ((cmdlist (split-string-and-unquote cmd)))
        (set-buffer (apply #'make-comint (format "scheme<%s>" name)
                           (car cmdlist)
                           (scheme-start-file (car cmdlist))
                           (cdr cmdlist)))
        (inferior-scheme-mode)))
    (setq scheme-program-name cmd
          scheme-buffer bufname)
    (pop-to-buffer-same-window bufname)))

(defun scheme-visible-repl-buffer ()
  "Return a live inferior-scheme REPL buffer shown in the selected frame, or nil.
If several are visible the first one in window order wins."
  (catch 'found
    (dolist (win (window-list (selected-frame) 'nomini))
      (let ((buf (window-buffer win)))
        (when (and (buffer-live-p buf)
                   (eq (buffer-local-value 'major-mode buf) 'inferior-scheme-mode)
                   (get-buffer-process buf))
          (throw 'found buf))))))

(defun scheme-proc--prefer-visible (orig &rest args)
  "Target the REPL visible in the selected frame when evaluating from source.
Advice around `scheme-proc': in a non-REPL buffer, if a REPL window is
showing in the current frame, evaluate into it; otherwise behave as usual."
  (let* ((repl (and (not (eq major-mode 'inferior-scheme-mode))
                    (scheme-visible-repl-buffer)))
         (scheme-buffer (if repl (buffer-name repl) scheme-buffer)))
    (apply orig args)))

(advice-add 'scheme-proc :around #'scheme-proc--prefer-visible)

(defun lisp ()
  "call run-lisp with clisp intepreter."
  (interactive)
  (run-lisp "/usr/bin/clisp -q"))

(defun kawa ()
  "Run kawa in *scheme<kawa>*."
  (interactive)
  (run-scheme-named "kawa" "/usr/bin/kawa"))

(defun chicken ()
  "Run Chicken (csi) in *scheme<chicken>*."
  (interactive)
  (run-scheme-named "chicken" "/usr/bin/csi"))

(defun guile ()
  "Run guile in *scheme<guile>*."
  (interactive)
  (run-scheme-named "guile" "/usr/bin/guile3.0"))

(defun lips ()
  "Run LIPS in *scheme<lips>*."
  (interactive)
  (run-scheme-named "lips" "/usr/bin/env lips -q"))

(defun gambit ()
  "Run Gambit (gsi) in *scheme<gambit>*."
  (interactive)
  (run-scheme-named "gambit" "/usr/bin/env gsi"))

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
