(require 'erc)
(require 'tls)
(require 'erc-match)

;; SETUP ERC
;;
;; (setq ERC_USERNAME "<nick>")
;; (setq ERC_PASSWORD "<password>")
;; (setq ERC_CHANNEL_ACTIVITY_COMMAND "<channel activity after inactivy notification command>")
;; (setq ERC_MENTION_COMMAND "<irc channel mention command>")

(defun irc-freenode ()
  "Connect to the freenode"
  (interactive)
  (erc :server "holmes.freenode.net"
       :port 6667
       :nick ERC_USERNAME
       :password ERC_PASSWORD))

(defun irc ()
  (interactive)
  (erc-tls :server "irc.libera.chat"
           :port 6697
           :nick ERC_USERNAME
           :password ERC_PASSWORD))

(defun erc-cmd-JCUBIC ()
  (erc-send-command (concat "nick " ERC_USERNAME))
  (erc-send-command (concat "NickServ identify " ERC_PASSWORD)))

;; Monitor IRC activity
(setq inactivity-buffer-alist '(("#openclipart" (inactivity . 900))))

(defun inactivity-time (buffer-name)
  (interactive "bBuffer: ")
  (let ((buffer-alist-pair (assoc buffer-name inactivity-buffer-alist)))
    (if (not (null buffer-alist-pair))
        (let ((last-time-pair (assoc 'last-time (cdr buffer-alist-pair))))
          (if (not (null last-time-pair))
              (float-time (time-subtract (current-time)
                                         (cdr last-time-pair))))))))


(defvar irc-channel-activity-sound nil)
(defvar irc-mention-sound nil)

(setq irc-mention-sound t)
(setq irc-channel-activity-sound t)

(defun channel-activity (string &rest ignore)
  "notification when there is activity on a erc channel after inactivity"
  (let* ((buffer (buffer-name))
         (buffer-alist-pair (assoc buffer inactivity-buffer-alist))
         (buffer-alist (cdr buffer-alist-pair))
         (current-time (current-time)))
    (if (not (null buffer-alist))
        (let ((last-time-pair (assoc 'last-time buffer-alist))
              (inactivity (cdr (assoc 'inactivity buffer-alist))))
          (if (not (or (string-match "^\\*\\*\\*" string)
                       (string-match "\\[freenode-info\\]" string)
                       (string-match "^<xfbot>" string))) ;; hardcoded ocal bot
              (progn
                (if (and (or (null last-time-pair)
                             (> (float-time (time-subtract current-time
                                                           (cdr last-time-pair)))
                                inactivity))
                         irc-channel-activity-sound)
                    (async-exec-command ERC_CHANNEL_ACTIVITY_COMMAND))
                (if (null last-time-pair)
                    (setf (cdr buffer-alist-pair)
                          (append buffer-alist
                                  (list (cons 'last-time current-time))))
                  (setf (cdr last-time-pair) current-time))))))))

(add-hook 'erc-insert-pre-hook 'channel-activity)
; (remove-hook 'erc-insert-pre-hook 'channel-activity)

;; ERC sound notifications
(defun erc-notify-on-mention (match-type nickuserhost msg)
  (interactive)
  (let* ((whole-text (buffer-substring (point-min) (point-max)))
         (notif (or (string-match "^-NickServ-" whole-text)
                    (string-match "^\\*\\*\\*" whole-text)
                    ;; matrix Fabricators message by jcubic
                    (string-match (regexp-quote "[matrix] <jcubic>") whole-text))))
    (if (and (eq match-type 'current-nick) (not notif) irc-mention-sound)
        (progn
          (async-exec-command ERC_MENTION_COMMAND)
          (notify "ERC" msg)))))

(add-hook 'erc-text-matched-hook 'erc-notify-on-mention)

(setq erc-messages '())

(defun map-hash (fun hash)
  (let ((result '()))
    (maphash (lambda (k v)
               (setq result (cons (funcall fun k v) result))) hash)
    result))

(defun hash-keys (hash)
  (interactive)
  (map-hash (lambda (k v) k) hash))


(defun erc-private-message (match-type nickuserhost msg &optional notif)
  (interactive)
  (if (and (hash-table-p erc-channel-users) (not notif))
      (let ((users (hash-keys erc-channel-users)))
        (if (and (eq (length users) 1)
                 (equal (car users) (buffer-name))
                 (not (member (car users) erc-messages)))
            (progn
              (add-to-list 'erc-messages (car users))
              (async-exec-command
               "mpg123 -q /home/kuba/Pobrane/button-9.mp3"))))))

(add-hook 'erc-text-matched-hook 'erc-private-message)

(setq erc-nickserv-identify-mode 'autodetect)
(erc-autojoin-mode t)
(setq erc-join-buffer 'buffer)

;; https://www.freenode.net/irc_servers.shtml
;;   holmes.freenode.net
;;   orwell.freenode.net
;;   calvino.freenode.net
;;   gibson.freenode.net
;;   leguin.freenode.net
;;   lem.freenode.net
;;   wolfe.freenode.net
;;   sendak.freenode.net
;;   jordan.freenode.net
;;   lindbohm.freenode.net
;;   holmes.freenode.net
;;   barjavel.freenode.net
;;   bartol.freenode.net
;;   pratchett.freenode.net

