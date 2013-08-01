;; mode:-*-emacs-lisp-*- -*- coding: utf-8; -*-

;; Time-stamp: <2013-08-02 16:40:35 (zzhelyaz)>

;;________________________________________________________________________________
;;                                                                   Email client

(require 'mu4e)
(global-set-key (kbd "C-x m") 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; don't prompt for applying of marks, just apply
(setq mu4e-headers-leave-behavior 'apply)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

(setq mu4e-get-mail-command "offlineimap"
      mu4e-attachment-dir "~/Downloads"
      mu4e-confirm-quit nil
      mu4e-headers-skip-duplicates t
      mu4e-compose-dont-reply-to-self t 
      mu4e-headers-date-format "%d %b, %Y at %H:%M"
      mu4e-html2text-command "w3m -dump -cols 80 -T text/html"

      mu4e-headers-replied-mark '("R" . "↵")
      mu4e-headers-passed-mark '("P" . "⇉")
      mu4e-use-fancy-chars t)

;; headers in the overview
(setq mu4e-headers-fields
      '((:maildir       .  14)
        (:date          .  24)
        (:flags         .   6)
        (:from          .  24)
        (:subject       .  nil)))

;; try to display images in mu4e
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;; automatic update every 5 min
(setq mu4e-update-interval 300)

(setq mu4e-reply-to-address "zlatozar@gmail.com"
      user-mail-address "zlatozar@gmail.com"
      user-full-name  "Zlatozar Zhelyazkov"
      message-signature
      (concat
       "Zlatozar\n"
       "http://zlatozar.blogspot.com\n"))

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; from dired send file as attachment (C-c RET C-a)
(require 'gnus-dired)

(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;________________________________________________________________________________
;;                                                                       Org-mode

(setq org-directory "~/.emacs.d/org/")
(setq org-agenda-files (directory-files (concat org-directory "agenda/")
                                        t  "^[^#].*\\.org$")

      org-agenda-include-diary t
      org-agenda-show-all-dates t              ; shows days without items
      org-agenda-skip-deadline-if-done  t      ; don't show in agenda...
      org-agenda-skip-scheduled-if-done t      ; .. when done
      org-agenda-start-on-weekday nil          ; start agenda view with today

      org-agenda-skip-unavailable-files t

      org-agenda-todo-ignore-deadlines t       ; don't include ...
      org-agenda-todo-ignore-scheduled t       ; ...timed/agenda items...
      org-agenda-todo-ignore-with-date t       ; ...in the todo list

      org-completion-use-ido t

      org-enforce-to-checkbox-dependencies t   ; parents can't be closed...
      org-enforce-todo-dependencies t          ; ...before their children
      org-hide-leading-stars t                 ; hide leading stars

      org-export-html-inline-images t

      org-log-done 'time                       ; log time when marking as DONE
      org-return-follows-link t                ; return follows the link
      org-tags-column -78                      ; tags end at pos 78

      org-export-with-section-numbers nil      ; no numbers in export headings
      org-export-with-toc nil                  ; no ToC in export
      org-export-with-author-info nil          ; no author info in export
      org-export-with-creator-info nil         ; no creator info
      org-export-htmlize-output-type 'css

      org-use-fast-todo-selection t
      org-archive-location (concat org-directory "/archive.org::%s")

      org-tag-alist '(("FAMILY"   .  ?f)
                      ("FINANCE"  .  ?m)
                      ("FRIENDS"  .  ?v)
                      ("HACKING"  .  ?h)
                      ("HOME"     .  ?t)
                      ("URGENT"   .  ?u)
                      ("WORK"     .  ?w))

      org-todo-keywords '((type "TODO(t)" "STARTED(s)" "MAYBE(m)" "INFO(i)"
                           "WAITING(w)" "VIEW(v)" "|" "DONE(d)" "CANCELLED(c)")))

(global-set-key (kbd "s-a") 'org-agenda-list)

;; Calendar/diary
(setq diary-file "~/diary.txt"
      display-time-24hr-format   t
      display-time-format        nil
      display-time-use-mail-icon nil

      mark-holidays-in-calendar  nil
      calendar-week-start-day    1
      european-calendar-style    t

      calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t
      calendar-location-name "Sofia, Bulgaria")

(global-set-key [(control f4)] 'diary)
