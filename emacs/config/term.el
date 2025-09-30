(defun fix-commit-output (output)
  (replace-regexp-in-string "\033\\[[0-9]+[GK]" "." output))

(defun insert-control-character (str)
  (interactive "sInsert Raw: ")
  (insert (read-kbd-macro str)))

