;;; -*- lexical-binding: nil; -*-

(require 'poly-markdown)
(require 'flymake-languagetool)

(setq flymake-languagetool-server-jar nil)
(if (not (string-equal flymake-languagetool-api-username "jcubic@jcubic.pl"))
    (setq flymake-languagetool-url "https://api.languagetool.org"))

(defun markdown ()
  (interactive)
  (setq fill-column 100)
  (if (let ((name (buffer-file-name)))
        (or (string-match-p "jcubic/www/blog/repo" name)
            (string-match-p "jankiewicz" name)
            (string-match-p "blog/me" name)
            (string-match-p "wikizeit" name)
            (string-match-p "/pl/" name)))
      (setq-local flymake-languagetool-language "pl-PL")
    (setq-local flymake-languagetool-language "en-US"))
  (flymake-languagetool-load)
  (flymake-mode)
  )

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

;; -----------------------------------------------------------------------------
;; :: hack to fix error in markdown mode
;; -----------------------------------------------------------------------------

(defun markdown-fontify-headings-fix (orig-fun last)
  "Guard against stale match-data from polymode re-fontification."
    (condition-case nil
        (funcall orig-fun last)
      (wrong-type-argument nil)))

(advice-add 'markdown-fontify-headings :around 'markdown-fontify-headings-fix)

;; -----------------------------------------------------------------------------
;; Key binding
;; -----------------------------------------------------------------------------
(define-key my-prefix-map (kbd "<left>") #'flymake-goto-prev-error)
(define-key my-prefix-map (kbd "<right>") #'flymake-goto-next-error)
(define-key my-prefix-map (kbd "SPC") #'flymake-languagetool-correct-dwim)
