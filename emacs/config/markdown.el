(require 'poly-markdown)
(require 'flymake-languagetool)

(setq flymake-languagetool-server-jar nil)
(setq flymake-languagetool-url "https://api.languagetool.org")
(define-key flymake-mode-map (kbd "C-c C-a") 'flymake-languagetool-correct-dwim)

(defun markdown ()
  (interactive)
  (setq fill-column 100)
  (if (let ((name (buffer-file-name)))
        (or (string-match-p "jcubic/www/blog/repo" name)
            (string-match-p "jcubic/www/jankiewicz" name)
            (string-match-p "/pl/" name)))
      (setq-local flymake-languagetool-language "pl-PL")
    (setq-local flymake-languagetool-language "en-US"))
  (flymake-languagetool-load)
  (flymake-mode))

(defun lang (lang)
  (interactive "sLanguage: ")
  (ispell-change-dictionary lang)
  (flyspell-buffer))

(defun english ()
  (interactive)
  (lang "english"))

(defun flymake-err-at (pos)
  (let ((overlays (overlays-at pos)))
    (remove nil
            (mapcar (lambda (overlay)
                      (and (overlay-get overlay 'flymake-overlay)
                           (overlay-get overlay 'help-echo)))
                    overlays))))

(add-hook 'markdown-mode-hook 'markdown)
(add-hook 'poly-markdown-mode-hook 'markdown)

(dolist (pair '(("\\.\\(md\\|mdx\\)\\'" . poly-markdown-mode)
                ("\\.markdown$" . poly-markdown-mode)))
  (add-to-list 'auto-mode-alist pair))
