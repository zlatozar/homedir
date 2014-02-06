;;; This file contains configuration for 'command line' Emacs

;;________________________________________________________________________________
;;                                                                        Minimal

;; Highlight everywhere
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq default-buffer-file-coding-system 'utf-8)
(setq inhibit-startup-message t)
(setq make-backup-files nil)

;; spaces instead of tabs
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; no white spaces
(add-hook 'before-save-hook 'whitespace-cleanup)

(show-paren-mode t)

(ido-mode 1)
(ido-everywhere 1)

;; modeline
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "cyan")
(set-face-foreground 'mode-line-inactive "black")
(set-face-background 'mode-line-inactive "brightblack")

;; mini buffer
(set-face-foreground 'minibuffer-prompt "yellow")

;; set the name of the host and current path/file in title bar
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
