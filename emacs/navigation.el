;; --------------------------------------------------------------------------
;; This code allows you to swith between buffers in same major mode
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

(global-set-key [C-M-tab] 'previous-buffer-same-mode)
(global-set-key [C-tab] 'next-buffer-same-mode)

;; --------------------------------------------------------------------------
;; switch between windows
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
;; Bookmarks in same buffer
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
