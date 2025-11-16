(defun po-wrap ()
  "Filter current po-mode buffer through `msgcat' tool to wrap all lines."
  (interactive)
  (if (eq major-mode 'po-mode)
      (let ((tmp-file (make-temp-file "po-wrap."))
            (tmp-buf (generate-new-buffer "*temp*")))
        (unwind-protect
             (progn
               (write-region (point-min) (point-max) tmp-file nil 1)
               (if (zerop
                    (call-process
                     "msgcat" nil tmp-buf t (shell-quote-argument tmp-file)))
                   (let ((saved (point))
                         (inhibit-read-only t))
                     (delete-region (point-min) (point-max))
                     (insert-buffer tmp-buf)
                     (goto-char (min saved (point-max))))
                 (with-current-buffer tmp-buf
                   (error (buffer-string)))))
          (kill-buffer tmp-buf)
          (delete-file tmp-file)))))

;; --------------------------------------------------------------------------
(defun po-guess-language ()
  "Return the language related to this PO file."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward po-any-msgstr-block-regexp)
    (goto-char (match-beginning 0))
    (if (re-search-forward
         "\n\"Language: +\\(.+\\)\\\\n\"$"
         (match-end 0) t)
        (po-match-string 1))))
