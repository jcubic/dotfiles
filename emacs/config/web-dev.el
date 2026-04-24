(require 'web-mode)
(require 'company)
(require 'prisma-mode)
(require 'tide)
(require 'typescript-mode)
(require 'coverage)

;; EMMET
(add-hook 'web-mode-hook 'emmet-mode)
(global-set-key (kbd "C-c k") 'emmet-expand-line)

(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(flymake-languagetool-load)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flymake-mode)
  ;;(flycheck-mode +1)
  ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (local-set-key (kbd "C-c i") 'tide-documentation-at-point)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . web-mode))

(defun setup-tide ()
  (let ((ext (file-name-extension buffer-file-name)))
    (when (string-equal "tsx" ext) ;; (or (string-equal "jsx" ext)
      (setup-tide-mode))))

(add-hook 'web-mode-hook 'setup-tide)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
