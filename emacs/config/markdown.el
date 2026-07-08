;;; -*- lexical-binding: nil; -*-

(require 'poly-markdown)

(add-to-list 'load-path "/home/kuba/projects/jcubic/alt/")

(require 'alt)

(setq alt-languagetool-server-jar nil)
(if (not (string-equal alt-languagetool-api-username "jcubic@jcubic.pl"))
    (setq alt-languagetool-url "https://api.languagetool.org"))

(setq alt-correct-style 'company)
(setq alt-company-remove-label "[remove duplicate]")

(setq poly-markdown-enable-latex-math nil)

(defun markdown ()
  (interactive)
  (setq fill-column 100)
  (if (let ((name (buffer-file-name)))
        (or (string-match-p "jcubic/www/blog/repo" name)
            (string-match-p "jankiewicz" name)
            (string-match-p "blog/me" name)
            (string-match-p "wikizeit" name)
            (string-match-p "/pl/" name)))
      (setq-local alt-language "pl-PL")
    (setq-local alt-language "en-US"))
  (local-set-key (kbd "C-S-SPC") 'alt-correct-at-point)
  (local-set-key (kbd "C-:") 'alt-correct-auto)
  (local-set-key (kbd "C-S-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "C-S-n") 'flymake-goto-next-error)
  (company-mode +1)
  (alt-mode))

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
      ((wrong-type-argument args-out-of-range) nil)))

(advice-add 'markdown-fontify-headings :around 'markdown-fontify-headings-fix)

(defun markdown-fontify-blockquotes-fix (orig-fun last)
  "Guard against stale match-data from polymode re-fontification."
    (condition-case nil
        (funcall orig-fun last)
      ((wrong-type-argument args-out-of-range) nil)))

(advice-add 'markdown-fontify-blockquotes :around 'markdown-fontify-blockquotes-fix)

;; -----------------------------------------------------------------------------
;; Key binding
;; -----------------------------------------------------------------------------
(define-key my-prefix-map (kbd "<left>") #'flymake-goto-prev-error)
(define-key my-prefix-map (kbd "<right>") #'flymake-goto-next-error)
(define-key my-prefix-map (kbd "SPC") #'flymake-languagetool-correct-dwim)
