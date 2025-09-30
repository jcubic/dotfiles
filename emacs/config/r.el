(autoload 'R-mode "ess-site.el" "ESS" t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(setq inferior-ess-r-program "/usr/bin/R")

(setq ess-eval-visibly-p nil)
(setq ess-ask-for-ess-directory nil)

(defun r-custom ()
  (setq ess-default-style 'OWN)
  (ess-set-style 'RStudio- 'quiet)
  (setq ess-indent-level 2))

(add-hook 'R-mode-hook 'r-custom)

;; --------------------------------------------------------------------------
(defun call-r-command (command-list)
  "run R command in shell buffer window

  if there is displayed buffer that have shell it will use that window"
  (let ((run-command
         (lambda (name command-list old-window)
           (let* ((script-proc-buffer
                   (apply 'make-comint-in-buffer
                          "script"
                          (get-buffer-create name)
                          "/usr/bin/R"
                          nil
                          command-list))
                  (window (or old-window
                              (get-window-with-mode '(comint-mode eshell-mode))))
                  (script-proc (get-buffer-process script-proc-buffer)))
             (with-current-buffer script-proc-buffer
               (local-set-key (kbd "C-c q") (lambda () (interactive) (kill-process))))
             (with-current-buffer script-proc-buffer
               (make-local-variable 'process-environment)
               (setenv "LANG" "EN")
               (read-only-mode))
             (if window
                 (progn
                   (set-window-buffer window script-proc-buffer)
                   (select-window window)
                   (goto-char (point-max)))
               (switch-to-buffer-other-window script-proc-buffer))))))
    (let* ((name "*R<2>*")
           (old-buff (get-buffer name))
           (old-window (if old-buff (get-buffer-window old-buff))))
      (if old-buff
          (with-current-buffer old-buff
            (let ((proc (get-buffer-process old-buff)))
              (if proc
                  (progn
                    (lexical-let ((run-command run-command)
                                  (name name)
                                  (command-list command-list)
                                  (old-window old-window))
                      (set-process-sentinel proc
                                            (lambda (process event)
                                              (funcall run-command
                                                       name
                                                       command-list
                                                       old-window))))
                    (kill-process))
                (funcall run-command name command-list old-window))))
        (funcall run-command name command-list old-window)))))

;; --------------------------------------------------------------------------
(defun build-r-package ()
  (interactive)
  (call-r-command `("CMD" "INSTALL" "--with-keep.source" ,(git-root-repo))))

;; --------------------------------------------------------------------------
(defun exec-r-code (code)
    "run shiny R application in shell buffer"
  (interactive "sCode: ")
  (call-r-command `("-e" ,code "--no-save")))

;; --------------------------------------------------------------------------
(defmacro make-r-code-fn (name code)
  (let ((Rname (gensym)))
    `(defun ,name ()
       (interactive)
       (let ((,Rname (concat "setwd('" default-directory "');"
                        ,code)))
         (exec-r-code ,Rname)))))

;; --------------------------------------------------------------------------
(make-r-code-fn test-r-package "devtools::test()")
(make-r-code-fn test-r-file (concat "testthat::test_file('" (buffer-file-name) "')"))
(make-r-code-fn document-r-package "devtools::document(roclets=c('rd', 'collate', 'namespace')); devtools::build_vignettes()")
(make-r-code-fn shiny (concat "shiny::runApp('" default-directory "')"))
(make-r-code-fn shiny-file (concat "shiny::runApp('" (buffer-file-name) "')"))
(make-r-code-fn cmd-check (concat "rcmdcheck::rcmdcheck(devtools::as.package('" (git-root-repo) "')$path)"))

;; --------------------------------------------------------------------------
;; open shiny in browser
(defun open-url (url)
  (interactive "sURL: ")
  (let ((path "/usr/bin/google-chrome"))
    (apply 'start-process "Chrome" nil path `(,url))))

(add-hook 'comint-output-filter-functions #'r-buffer-filter-fn)

(setq r-shiny-query-params nil)

;; --------------------------------------------------------------------------
(defun r-shiny-url (query)
  (interactive "sURL: ")
  (setq r-shiny-query-params query))

;; --------------------------------------------------------------------------
(defun r-buffer-filter-fn (text)
  (if (and (string= "*R<2>*" (buffer-name))
       (string-match "Listening on \\([^[:space:]]+\\)" text))
      (let* ((adress (match-string 1 text))
             (url (if (null r-shiny-query-params)
                      adress
                    (concat adress r-shiny-query-params))))
        (open-url url))))

;; --------------------------------------------------------------------------
(defun r/get-names (names)
  (let ((list (split-string names ",")))
    (mapcar (lambda (name)
              (let ((name (replace-regexp-in-string "‘" "" name)))
                (replace-regexp-in-string "^[’\s']+\\|[’\s']+$" "" name)))
            list)))

;; --------------------------------------------------------------------------
(defun r/package-split (name)
  (if (string-match "@" name)
      (split-string name "@")
    (when (string-match "\\(\\w+\\).*==\s*\\([0-9.]+\\)" name)
      (list
       (match-string 1 name)
       (match-string 2 name)))))

;; --------------------------------------------------------------------------
(defun r/package-version-p (name)
  (not (null (string-match "@\\|==" name))))

;; --------------------------------------------------------------------------
(defun install-r-package (arg)
  (interactive "sPacakges: ")
  (let ((packages (if (string-match "," arg) (r/get-names arg) (list arg))))
    (if (r/package-version-p arg)
        (dolist (name packages)
          (let* ((lst (r/package-split name))
                 (R (concat "remotes::install_version("
                            "'" (car lst) "'"
                            (if (= (length lst) 2)
                                (concat ", version = '" (cadr lst) "'")
                              "")
                            ", upgrade = 'never'"
                            ", repos='http://cran.uk.r-project.org')")))
            (exec-r-code R)))
      (let* ((lst (mapconcat (lambda (name) (concat "'" name "'")) packages ","))
             (R (concat "install.packages("
                        "c(" lst ")"
                        ", repos='"
                        "http://cran.uk.r-project.org"
                        "')")))
        (exec-r-code R)))))
