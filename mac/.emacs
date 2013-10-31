;; Additional for MacOS

;; Import system PATH variables on Mac
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; Requires 'cocoAspell' installed
(if (eq system-type 'darwin)
    (if (file-executable-p "/usr/local/bin/aspell")
        (progn
          (setq ispell-program-name "/usr/local/bin/aspell")
          (setq
           ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi")))))

