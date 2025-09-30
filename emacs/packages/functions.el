;; utilities

(defun partition (lst n)
  (if (null lst)
      nil
    (if (= (% (length lst) n) 0)
        (cons (cons (car lst) (cadr lst))
              (partition (cddr lst) n)))))

(defun filter (fun seq)
  (if (null seq)
      nil
    (if (funcall fun (car seq))
        (cons (car seq) (filter fun (cdr seq)))
      (filter fun (cdr seq)))))

(defun complement (fun)
  (lexical-let ((fun fun))
    (lambda (&rest args)
      (not (apply fun args)))))

(defun partial (fun &rest args)
  (lexical-let ((fun fun) (args args))
    (lambda (&rest more)
      (apply fun (append args more)))))

(defun remove (item seq)
  (filter (partial (complement #'equal) item) seq))

(setq cars (partial #'mapcar #'car))
(setq cdrs (partial #'mapcar #'cdr))

(defun map (fun &rest seqs)
  (if (or (null seqs) (null (car seqs)))
      nil
     (cons (apply fun (mapcar #'car seqs))
           (apply #'map (cons fun (mapcar #'cdr seqs))))))


(defun zip (&rest lists)
  (apply #'map #'list lists))


;(zip (list :a :b :c) (list 1 2 3))

(defun major-mode ()
  (interactive)
  (message (symbol-name major-mode)))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
     (set-buffer buffer-or-string)
     major-mode))

(defun len ()
  (interactive)
  (message (number-to-string (length (region-to-string)))))

(defun region-to-string ()
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a, %d %b %Y %T %z")))


(defun date-rfc ()
  (interactive)
  (insert (shell-command-to-string "date -R | tr -d '\n'")))

(defun current-buffer-name ()
  (interactive)
  (insert-string (buffer-name)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun member-fun (fun element list)
  (let ((found nil) (list list))
    (while (and (not found) (not (null list)))
      (if (funcall fun (car list) element)
          (setq found t)
        (setq list (cdr list))))
    (if found
        list
      nil)))

(defun range (n &optional list)
  (if (eq n 0)
      list
    (let ((n (- n 1)))
      (range n (cons n list)))))


(defmacro for (var start end step &rest body)
  `(do ((,var ,start ,step)) (,end)
     (progn
       ,@body)))

(provide 'functions)
