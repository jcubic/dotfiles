;;; -*- lexical-binding: nil; -*-

;; send email: C-x m => C-c C-c
(if (and (boundp 'SMTP_EMAIL) (boundp 'SMTP_SERVER) (boundp 'SMTP_NAME))

    (setq user-full-name SMTP_NAME
          user-mail-address SMTP_EMAIL)

  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)

  (setq smtpmail-smtp-server SMTP_SERVER
        smtpmail-smtp-service 587)

  (setq smtpmail-stream-type 'starttls)

  (setq auth-sources '("~/.authinfo.gpg")))
