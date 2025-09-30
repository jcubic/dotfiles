;; --------------------------------------------------------------------------
(defun shell-line (command)
  (replace-regexp-in-string "\n" "" (shell-command-to-string command)))

;; --------------------------------------------------------------------------
(defun git-root-repo ()
  (interactive)
  (shell-line "git rev-parse --show-toplevel"))

;; --------------------------------------------------------------------------
(defun git-grep (search)
  "git-grep the entire current repo"
  (interactive (list (completing-read "Search for: " nil nil nil
                                      (when (region-active-p)
                                        (regexp-quote (region-to-string))))))
  (grep-find (concat "find "
                   (vc-git-root (or buffer-file-name default-directory))
                   " -type f \\( ! -ipath '*min*.js' ! -ipath '*pack*.js' \\) "
                   "-exec grep -nHE "
                   "\"" search "\" {} \\;")))
