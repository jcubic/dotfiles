;;; -*- lexical-binding: nil; -*-

;; send email: C-x m => C-c C-c
(if (and (boundp 'SMTP_EMAIL) (boundp 'SMTP_SERVER) (boundp 'SMTP_NAME))
    (progn
      (setq user-full-name SMTP_NAME
          user-mail-address SMTP_EMAIL)

      (setq send-mail-function 'smtpmail-send-it
            message-send-mail-function 'smtpmail-send-it)

      (setq smtpmail-smtp-server SMTP_SERVER
            smtpmail-smtp-service 587)

      (setq smtpmail-stream-type 'starttls)

      (setq auth-sources '("~/.authinfo.gpg"))

      (let ((bcc (concat "Bcc: " SMTP_EMAIL "\n")))
        (if (boundp 'message-default-mail-headers)
            (setq message-default-mail-headers
                  (concat bcc message-default-mail-headers))
          (setq message-default-mail-headers bcc)))))
