;;; -*- lexical-binding: nil; -*-

;; async-shell-command
(defun async-exec-command (command &rest success)
  (interactive "MExecute command: ")
  (let* ((buffer-name (generate-new-buffer-name "**shell**"))
         (buffer (get-buffer-create buffer-name))
         (process (apply #'start-process
                         (append (list buffer-name buffer)
                                 (split-string-and-unquote command)))))
    (lexical-let ((buffer buffer)
                  (success (car success))
                  (command command))
      (set-process-sentinel process
                            (if success
                                (lambda (process str)
                                  (let ((user-buffer (current-buffer)))
                                    (save-excursion
                                      (set-buffer buffer)
                                      (let ((content (buffer-string)))
                                        (kill-buffer buffer)
                                        (if (or (string= str "finished\n")
                                                (string-match "exited abnormally" str))
                                            (progn
                                              (set-buffer user-buffer)
                                              (funcall success content))
                                          (message content))))))
                              (lambda (proces str)
                                (kill-buffer buffer)))))
    (concat "execute: " command)))

;; shell config
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(setq term-scroll-to-bottom-on-output t)

 (defun remote-term (new-buffer-name cmd &rest switches)
   (interactive)
   (setq term-ansi-buffer-name (concat "*ssh:" new-buffer-name "*"))
   (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
   (setq term-ansi-buffer-name
         (apply 'make-term term-ansi-buffer-name cmd nil switches))
   (set-buffer term-ansi-buffer-name)
   (term-mode)
   (term-char-mode)
   ;;(term-set-escape-char ?\C-x)
   (switch-to-buffer term-ansi-buffer-name))

(defmacro term-color (name color)
  `(set-face-attribute ',(intern (concat "term-color-" (symbol-name name)))
                       nil :foreground ,color))

(defun term-exec-handler ()
  (interactive)
  ;; XTerm colors
  ;;(macroexpand '(term-color blue "#5555FF"))
  (term-color blue "#5555FF")
  (term-color green "#55FF55")
  (term-color red "#FF5555")
  (term-color magenta "#FF55FF")
  (term-color cyan "#FF55FF")
  (term-color yellow "#FFFF55")
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (lexical-let ((buff buff))
      (set-process-sentinel proc
                            (lambda (process event)
                              (unless (process-live-p process)
                                (kill-buffer buff)))))))

(add-hook 'term-exec-hook 'term-exec-handler)

(defmacro ssh (name &optional server)
  "macro creates function that connect to ssh by name"
  (let ((str (symbol-name name)))
    `(defun ,name ()
       (interactive)
       (remote-term ,str "ssh" ,(or server str)))))

(defun send-raw-key-fun (str)
  (lexical-let ((str str))
    (lambda ()
      (interactive)
      (term-send-raw-string (read-key str)))))

(defun term-send-raw-key (str)
  (interactive)
  (term-send-raw-string (read-key str)))

(defun raw (str)
  (interactive "sSend Raw Key: ")
  (term-send-raw-string (read-key str)))


(defun read-key (str)
  (mapconcat (lambda (event)
             (and (characterp event)
                  (char-to-string event)))
           (read-kbd-macro str)))

(defun toggle-ansi-term-mode ()
  "Switch ansi-term btween character and line mode."
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (term-char-mode)))

(add-hook 'term-mode-hook
          (lambda()
            ;;(highlight-current-line-on nil)
            (setq show-trailing-whitespace nil)
            (define-key term-raw-map (kbd "M-w") 'kill-ring-save)
            (define-key term-raw-map (kbd "C-w") 'kill-region)
            (define-key term-raw-map (kbd "<C-left>") (send-raw-key-fun "\e[1;5D"))
            (define-key term-raw-map (kbd "<C-right>") (send-raw-key-fun "\e[1;5C"))
            (define-key term-raw-map (kbd "C-y")
              (lambda ()
                (interactive)
                (term-send-raw-string (current-kill 0))))
            ;; exit nano
            (define-key term-raw-map (kbd "C-c x") (send-raw-key-fun "C-x"))
            ;; cancel
            (define-key term-raw-map (kbd "C-c c") (send-raw-key-fun "C-c"))

            (define-key term-raw-map (kbd "C-S-SPC") 'toggle-ansi-term-mode)
            (define-key term-mode-map (kbd "C-S-SPC") 'toggle-ansi-term-mode)

            ;; vi
            ;; (define-key term-raw-map (kbd "C-c <ESC>")
            ;;   (send-raw-key-fun "<ESC>"))
            (define-key term-raw-map (kbd "M-x") 'execute-extended-command)))
