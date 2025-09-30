(defun test (&rest args)
  (with-temp-buffer
    (eq (apply 'call-process "test" nil (current-buffer) nil args) 0)))

(defun have-permission (filename)
  (let ((expanded (replace-regexp-in-string "~"
                                            (concat "/home/" (user-real-login-name))
                                            filename)))
    (if (not (file-exists-p expanded))
        (let ((directory (file-name-directory expanded)))
          (and (test "-r" directory) (test "-x" directory) (test "-w" directory)))
      (and (test "-r" expanded) (test "-w" expanded)))))


(defun find-every-file (filename &optional wildcards)
  "Open file use su:: if user have no permissions to open the file"
  (interactive
   (find-file-read-args "Find All Files: "
                        (confirm-nonexistent-file-or-buffer)))

  (find-file (if (have-permission filename)
                 filename
               (let ((f (concat "/sudo::" (file-truename filename))))
                 (message f)
                 f))))

(global-set-key (kbd "C-x C-f") 'find-every-file)

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (let* ((warning "WARNING: EDITING FILE AS ROOT!")
           (space (+ 6 (- (window-width) (length warning))))
           (bracket (make-string (/ space 2) ?-))
           (warning (concat bracket warning bracket)))
      (setq header-line-format
            (propertize  warning 'face 'find-file-root-header-face)))))

(add-hook 'find-file-hook 'find-file-root-header-warning)
