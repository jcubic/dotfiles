;;; -*- lexical-binding: nil; -*-

;; --------------------------------------------------------------------------
;; :: SAME MAJOR MODE SWTICH
;; --------------------------------------------------------------------------

(defun buffer-same-mode (change-buffer-fun)
  (let ((current-mode major-mode)
        (next-mode nil))
    (while (not (eq next-mode current-mode))
      (funcall change-buffer-fun)
      (setq next-mode major-mode))))

(defun previous-buffer-same-mode ()
  (interactive)
  (buffer-same-mode #'previous-buffer))

(defun next-buffer-same-mode ()
  (interactive)
  (buffer-same-mode #'next-buffer))

(defun switch-to-buffer-same-mode ()
  "Switch to a buffer with the same major mode, or any buffer on empty input."
  (interactive)
  (let* ((mode major-mode)
         (buffers (cl-remove-if-not
                   (lambda (buf)
                     (and (eq (buffer-local-value 'major-mode buf) mode)
                          (not (eq buf (current-buffer)))))
                   (buffer-list)))
         (names (mapcar #'buffer-name buffers))
         (target (completing-read
                  (format "Switch to %s buffer (RET for all): " mode)
                  names nil nil)))
    (if (string-empty-p target)
        (call-interactively #'switch-to-buffer)
      (switch-to-buffer target))))

(global-set-key [C-M-tab] 'previous-buffer-same-mode)
(global-set-key [C-tab] 'next-buffer-same-mode)
(global-set-key (kbd "C-x b") 'switch-to-buffer-same-mode)

;; --------------------------------------------------------------------------
;; :: SWITCH BETWEEN WINDOWS
;; --------------------------------------------------------------------------

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(global-set-key [M-left] (ignore-error-wrapper 'windmove-left))
(global-set-key [M-right] (ignore-error-wrapper 'windmove-right))
(global-set-key [M-up] (ignore-error-wrapper 'windmove-up))
(global-set-key [M-down] (ignore-error-wrapper 'windmove-down))

;; --------------------------------------------------------------------------
;; :: BOOKMARKS IN SAME BUFFER
;; --------------------------------------------------------------------------
(defvar bookmark-markers '())

(defun bookmark (bookmark)
   (interactive "nBookmark: ")
   (let* ((buffer (current-buffer))
          (bookmarks
           (let ((pair (assoc buffer bookmark-markers)))
             (if (eq pair nil)
                 (let ((new-pair (cons buffer '())))
                   (progn
                     (setq bookmark-markers
                           (append bookmark-markers
                                   (list new-pair)))
                     new-pair))
              pair))))
     (let ((pair (assoc bookmark bookmarks)))
       (if (eq pair nil)
           (setf (cdr bookmarks)
                 (append (cdr bookmarks) (list (cons bookmark (point)))))
         (setf (cdr pair) (point))))))

(defun jump-to-bookmark (bookmark)
  (interactive "nJump To: ")
  (let ((pair-bookmars (assoc (current-buffer) bookmark-markers)))
    (if (not (eq pair-bookmars nil))
        (let ((pair-point (assoc bookmark (cdr pair-bookmars))))
          (if (not (eq pair-point nil))
              (goto-char (cdr pair-point)))))))

(dolist (i (mapc #'1+ (range 9)))
    (global-set-key (read-kbd-macro (concat "C-c "
                                            (number-to-string i)))
                    (lexical-let ((i i))
                      (lambda ()
                        (interactive)
                        (jump-to-bookmark i)))))

(global-set-key (kbd "C-c 0") 'bookmark)

(defun jump-to-mark ()
  (interactive)
  (goto-char (mark-marker)))

;; overwrite marker pop and jump to just jump
(global-set-key (kbd "C-x C-SPC") 'jump-to-mark)

;; --------------------------------------------------------------------------
;; :: MOVE LINES
;; --------------------------------------------------------------------------
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift down)]  'move-line-down)
(global-set-key [(control shift up)]  'move-line-up)

;; --------------------------------------------------------------------------
;; :: MOUSEWHEEL
;; --------------------------------------------------------------------------
(defun sd-mousewheel-scroll-up (event)
  "Scroll window under mouse up by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-up 5))
      (select-window current-window))))

(defun sd-mousewheel-scroll-down (event)
  "Scroll window under mouse down by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-down 5))
      (select-window current-window))))

(global-set-key (kbd "<mouse-5>") 'sd-mousewheel-scroll-up)
(global-set-key (kbd "<mouse-4>") 'sd-mousewheel-scroll-down)

;; --------------------------------------------------------------------------
;; :: SHOW IN SAME BUFFER
;; --------------------------------------------------------------------------
(setq same-window-list '("*Help*" "*Completions*" "*Backtrace*" "*js*"))

(dolist (name same-window-list)
  (add-to-list 'same-window-buffer-names name))

(defun toggle-same ()
  "function toggle buffers in same-window-list"
  (interactive)
  (dolist (name same-window-list)
    (let ((help name))
      (if (member help same-window-buffer-names)
          (progn
            (message "same - disabled")
            (setq same-window-buffer-names
                  (remove help same-window-buffer-names)))
        (progn
          (message "same - enabled")
          (add-to-list 'same-window-buffer-names help))))))

(global-set-key (kbd "C-c h") 'toggle-same)

;; --------------------------------------------------------------------------
;; :: OPEN RELATIVE FILE FROM CLIPBOARD (grep friendly)
;; --------------------------------------------------------------------------
(defun get-kill-text ()
  (interactive)
  (let* ((text (current-kill 0))
         (start 0)
         (end (length text)))
    (set-text-properties start end nil text)
    text))

(defun clear-filename (fname)
  (replace-regexp-in-string
   "\\(^[ \t\n\r]*\\)\\|\\([ \t\n\r]*$\\)" ""
  (replace-regexp-in-string
   "\\(:.*\\|?.*\\)$" ""
   (replace-regexp-in-string
    "^webpack:///" ""
    (replace-regexp-in-string "http://localhost\\(:[0-9]+\\)?/home/" "/home/" fname)))))

(defun open-clipboard-filename ()
  (interactive)
  (let* ((re "[\\\/]")
         (open-filename (clear-filename (current-kill 0))) ;; strip grep output
         (pattern (car (split-string open-filename re))) ;; get first directory
         (name (replace-regexp-in-string (concat pattern re ".+")
                                         open-filename
                                         buffer-file-name)))
    (if (file-exists-p name)
        (progn (message name)
               (find-file name)))))

(defun force-keys ()
  (local-set-key (kbd "C-c C-f") 'open-clipboard-filename))

(add-hook 'after-change-major-mode-hook 'force-keys)


;; --------------------------------------------------------------------------
;; :: KILL SAME MAJOR MODE BUFFER SWITCH
;; --------------------------------------------------------------------------
(defun my-switch-to-prev-buffer-same-mode (orig-fun window &optional bury-or-kill)
  "After killing, prefer another buffer with the same major mode."
  (let* ((killed-buf (when (eq bury-or-kill 'kill)
                       (window-buffer window)))
         (target-mode (when killed-buf
                        (buffer-local-value 'major-mode killed-buf)))
         (result (funcall orig-fun window bury-or-kill)))
    (when (and target-mode
               (window-live-p window)
               (not (eq (buffer-local-value 'major-mode (window-buffer window))
                        target-mode)))
      (let ((candidates
             (seq-filter (lambda (b)
                           (and (buffer-live-p b)
                                (not (eq b killed-buf))
                                (not (eq b (window-buffer window)))
                                (eq (buffer-local-value 'major-mode b) target-mode)
                                (not (minibufferp b))
                                (not (string-prefix-p " " (buffer-name b)))))
                         (buffer-list))))
        (when candidates
          (set-window-buffer window (car candidates)))))
    result))

(advice-add 'switch-to-prev-buffer :around #'my-switch-to-prev-buffer-same-mode)
;;(advice-remove 'switch-to-prev-buffer #'my-switch-to-prev-buffer-same-mode)
