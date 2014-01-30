;; Time-stamp: <2014-01-30 14:20:05 (zzhelyaz)>

;;_______________________________________________________________________________
;;                                                                   Emacs build

;; $ git clone git://git.savannah.gnu.org/emacs.git
;; $ cd emacs

;; # 'EMACS' tag is for stable, 'EMACS_PRETEST' - unstable
;; $ git tag | grep "EMACS_24"

;; Tested with Emacs 24.3.1

;;_______________________________________________________________________________
;;                                                                    Load paths

(require 'cl)

(defconst my/elisp-path "~/.emacs.d/el-packages/")

;; Reload packages that are not handled by `el-get'
(defun load-path ()
  (interactive)
  (let* ((directory my/elisp-path)
         (newdirs (lp-subdir-list directory)))
    (setq load-path (remove-duplicates (append load-path newdirs) :test #'string=)))
  (message "Third part packages are (re)loaded!"))

(defconst +lp-ignore-list+
  (list "CVS" ".git" ".hg" ".svn" ".." "."))

(defun lp-subdir-list (base &optional ignore)
  (unless ignore
    (setq ignore +lp-ignore-list+))
  (let ((pending (list base))
        (retval nil))
    (while pending
      (let ((dir (pop pending)))
        (push dir retval)
        (dolist (f (directory-files dir))
          (let ((name (concat dir "/" f)))
            (when (and (not (member f ignore))
                       (file-directory-p name))
              (push name pending)
              (push name retval))))))
    (reverse retval)))

(load-path)

;;_______________________________________________________________________________
;;                                                               Manage packages

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq my:el-get-packages
      '(cedet
        ecb
        ;; should be compatible with installed 'mu'
        mu4e

        ;; Programming
        find-file-in-project
        auto-complete-clang
        auto-complete
        browse-kill-ring
        multiple-cursors
        undo-tree
        flycheck
        goto-chg
        yasnippet
        idomenu
        iedit
        ag

        ;; Lisp family
        highlight-parentheses
        elisp-slime-nav
        scheme-complete
        redshank
        paredit
        dash

        ;; Clojure
        clojure-mode
        ac-nrepl
        elein

        ;; C/C++
        google-c-style
        cmake-mode
        doxymacs
        eassist

        ;; Python
        jedi

        ;; Version control
        magit

        ;; Misc
        readline-complete
        powerline
        emacs-w3m
        htmlize
        smex))

(el-get 'sync my:el-get-packages)

;;_______________________________________________________________________________
;;                                                                       General

;; If "Time-stamp: <>" in the first 10 lines of the file
(setq time-stamp-active t
      time-stamp-line-limit 10
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")

(add-hook 'write-file-hooks 'time-stamp)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq default-buffer-file-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-input-method nil)

(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n")

;; Turn off the visible cursor in non-selected windows
(setq cursor-in-non-selected-windows nil)

;; Turn off sound
(setq ring-bell-function 'ignore)

;; 'nil' means case-sensitive
(setq case-fold-search nil)

;; Decompresses and compresses files on the fly
(auto-compression-mode t)

;; Display in status bar
(display-time)
(column-number-mode t)

;; Highlight parents
(show-paren-mode t)

(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; Smart about file names in mini-buffer
(file-name-shadow-mode t)

;; Delete the selection with a key press
(delete-selection-mode 1)

;; Highlight everywhere
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Increase font size (this may vary)
(set-face-attribute 'default nil :height 110) ; <-- insert yours

;; Add proper word wrapping
(global-visual-line-mode t)

;; View most recent version
(global-auto-revert-mode 1)

;; Auto refresh `dired'
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
;; Easy way to rename directory/files in `dired'
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; Set the name of the host and current path/file in title bar
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; unNOVICEd commands...
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer     'disabled nil)

;; Hippie expand configuration
(defconst dabbrev-always-check-other-buffers t)
(defconst dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(setq hippie-expand-dabbrev-as-symbol nil)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev try-expand-dabbrev-all-buffers
                           try-expand-dabbrev-from-kill
                           try-complete-file-name-partially
                           try-complete-file-name
                           try-expand-all-abbrevs
                           try-expand-list try-expand-line
                           try-complete-lisp-symbol-partially
                           try-complete-lisp-symbol))

;; Ignore C-z
(global-set-key (kbd "C-z") nil)

;; copy/paste from X
(setq x-select-enable-clipboard t)

;; End files with a newline
(setq require-final-newline t)

;; Easily navigate silly cased words
(global-subword-mode 1)

;; shift-{arrows} to move between buffers
(windmove-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; First day of the week is Monday instead of Sunday
(setq european-calendar-style 't)
(setq calendar--week--start--day 1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Set *scratch* buffer mode
(setq initial-major-mode 'text-mode)

;; nuke whitespaces when writing to a file
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Better scrolling
(setq scroll-step 1)

;; Hide toolbar
(tool-bar-mode -1)

;; Import system PATH variable (for MacOS see  mac/.emacs snippets)
(setenv "PATH" (shell-command-to-string "source ~/.bashrc; echo -n $PATH"))

;;_______________________________________________________________________________
;;                                                     Main third party packages

;; Use M-x describe-mode for more information

(message "-= Basic Packages =-")

;; 'C-x u' to visualise
(require 'undo-tree)
(global-undo-tree-mode)

;; Edit multiple lines
(require 'multiple-cursors)

;; Go to last change (third party)
(require 'goto-chg)

;; Interactive edit on multiple strings (third party)
(require 'iedit)

(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

(global-set-key (kbd "C-=") 'iedit-dwim)

;; Display kill ring (third party)
(require 'browse-kill-ring)

;; Parentheses view (third party)
(require 'highlight-parentheses)

;; Easy way to publish code fragments (third party)
(require 'htmlize)

(defun my/htmlize-region (beg end)
  "Htmlize region and put into <pre> tag style that is left in <body> tag
plus add font-size: 10pt"
  (interactive "r")
  (let* ((buffer-faces (htmlize-faces-in-buffer))
         (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
         (pre-tag (format
                   "<pre style=\"%s font-size: 10pt\">"
                   (mapconcat #'identity (htmlize-css-specs
                                          (gethash 'default face-map)) " ")))
         (htmlized-reg (htmlize-region-for-paste beg end)))
    (switch-to-buffer-other-window "*htmlized output*")
    ;; clear buffer
    (kill-region (point-min) (point-max))
    ;; set mode to have syntax highlighting
    (nxml-mode)
    (save-excursion
      (insert htmlized-reg))
    (while (re-search-forward "<pre>" nil t)
      (replace-match pre-tag nil nil))
    (goto-char (point-min))))

;; Emacs browser (third party)
(when (locate-library "w3m")
  (require 'w3m)
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)

  (setq browse-url-browser-function 'w3m-browse-url)
  (setq browse-url-new-window-flag t)
  (setq w3m-symbol 'w3m-default-symbol)
  (setq w3m-default-display-inline-images t)
  (setq w3m-mailto-url-function 'compose-mail)
  (setq w3m-use-cookies t))

;; Predefined web jumps
(autoload 'webjump "webjump"
  "Jumps to a Web site from a programmable hotlist." t)

(eval-after-load "webjump"
  '(progn
     (setq webjump-sites
           (append
            '(("STL" .
               [simple-query
                "http://www.sgi.com/tech/stl/index.html"
                "http://www.google.com/search?q="
                "+site%3Awww.sgi.com%2Ftech%2Fstl"])
              ;; Watch out for versions! 1_52_0 is current
              ("Boost" .
               [simple-query
                "http://www.boost.org/doc/libs/1_52_0/libs/libraries.htm"
                "http://www.google.com/search?q="
                "+site%3Awww.boost.org%2Fdoc%2Flibs%2F1_52_0%2Fdoc%2F"])
              ("CPP" .
               [simple-query
                "http://www.cplusplus.com/reference/"
                "http://www.cplusplus.com/query/search.cgi?q=" ""])

              ("key-bindings" . "http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley")
              ("elisp" . "http://www.gnu.org/software/emacs/manual/html_mono/elisp.html")
              ("scheme" . "http://api.call-cc.org/doc/"))
            webjump-sample-sites))))

;;_______________________________________________________________________________
;;                                                  CEDET/ECB and projects setup

(load "~/.emacs.d/init-cedet.el")

;;_______________________________________________________________________________
;;                                                                 Auto complete

;; `auto-complete' package (third party)
(require 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)

;; Get pop-ups with docs even if a word is uniquely completed
(setq ac-dwim nil)

;; to work with `flymake'
(ac-flyspell-workaround)

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger auto-complete
;;----------------------------------------------------------------------------

;; Use 't when `auto-complete' is disabled
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; Hook AC into `completion-at-point'
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(setq ac-auto-show-menu t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)

;; AC everywhere - hack
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))))
(real-global-auto-complete-mode t)

;; Exclude very large buffers from `dabbrev'
(defun my/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'my/dabbrev-friend-buffer)

;;_______________________________________________________________________________
;;                                                                Manage Buffers

;; shell buffer auto completion (third party)
(require 'readline-complete)

(add-to-list 'ac-modes 'shell-mode)
(add-hook 'shell-mode-hook 'ac-rlc-setup-sources)

;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; Find file in project, with specific patterns
(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-x C-o ja")
                (ffip-create-pattern-file-finder "*.java"))
(global-set-key (kbd "C-x C-o cl")
                (ffip-create-pattern-file-finder "*.cl"))
(global-set-key (kbd "C-x C-o py")
                (ffip-create-pattern-file-finder "*.py"))

;; Set the name of the host and current path/file in title bar
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

(global-set-key [(super shift return)] 'toggle-maximize-buffer)

;; Define bookmarks location
(setq bookmark-default-file "~/.emacs.d/data/bookmarks")

;; Where my temp dir is
(setq temporary-file-directory "~/.emacs.d/data/tmp/")

;; Backups configuration
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/data/backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(ido-mode 1)
(ido-everywhere 1)

;; Functions in a file, and lets you jump to theme
(require 'imenu)

;; Usage: (put 'dired 'ido 'ignore)
(defadvice ido-read-buffer (around ido-read-buffer-possibly-ignore activate)
  "Check to see if user wanted to avoid using ido"
  (if (loop for command = this-command then (symbol-function command)
            while (symbolp command)
            thereis (eq 'ignore (get command 'ido)))
      (let ((read-buffer-function nil))
        (run-hook-with-args 'ido-before-fallback-functions 'read-buffer)
        (setq ad-return-value (apply 'read-buffer (ad-get-args 0))))
    ad-do-it))

(put 'shell 'ido 'ignore)
(put 'dired-create-directory 'ido 'ignore)

(setq ido-ignore-buffers
      '("\\` " ".*Completion" "^ ?\\*")
      ido-case-fold  t              ; be case-insensitive
      ido-use-filename-at-point nil ; don't use file name at point
      ido-use-url-at-point nil      ; don't use url at point
      ido-enable-flex-matching t
      ido-max-prospects 6
      ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; Make buffers classification
(require 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("version control" (or (mode . svn-status-mode) (mode . svn-log-edit-mode)
                                (name . "^\\*vc-")
                                (name . "^\\*Annotate")
                                (name . "^\\*git-")
                                (name . "^\\*magit")))
         ("EMACS" (or (name . "^\\*Messages\\*$")
                      (name . "^\\*Help\\*$")
                      (name . "^\\*info\\*$")
                      (name . "^\\*CEDET")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*clang")
                      (name . "^\\*gud\\*$")
                      (name . "^\\*Backtrace\\*$")
                      (name . "^\\*Process List\\*$")
                      (name . "^\\*Completions\\*$")
                      (name . "^\\*slime-events\\*$")
                      (name . "^\\*slime-compilation\\*$")
                      (name . "^\\*tramp")
                      (name . "^\\*compilation\\*$")))
         ("C++" (or (mode . c++-mode)))
         ("includes" (or (name . "\\.h$")
                         (name . "\\.hpp$")))
         ("agenda" (or (name . "^\\*Calendar\\*$")
                       (name . "^diary$")      (name . "^\\*Agenda")
                       (name . "^\\*org-")     (name . "^\\*Org")
                       (mode . org-mode)))
         ("dired" (or (mode . dired-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Order the groups so the order is : [Default], [agenda], [emacs]
(defadvice ibuffer-generate-filter-groups
  (after reverse-ibuffer-groups () activate)
  (setq ad-return-value (nreverse ad-return-value)))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; If open two files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'recentf)
(recentf-mode t)
(setq recentf-save-file "~/.emacs.d/data/recentf"
      recentf-max-saved-items 500
      recentf-max-menu-items 60)

(defun my/ido-choose-from-recentf ()
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda(path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list) nil t))))

;; Smart M-x (third party)
(require 'smex)
(setq smex-save-file "~/.emacs.d/data/smex-items")
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; Set up remote buffers
;; C-x C-f /machine:file - for remote
;; C-x C-f /sudo:user@remote.machine:/var/log - for privileges; for local use M-x sudo-edit
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/.emacs.d/data/tramp")

;; Swap two buffers
(defun my/swap-buffer ()
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))

(global-set-key (kbd "C-x /") 'my/swap-buffer)

;;_______________________________________________________________________________
;;                                                                      Spelling

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=normal"))

;; `flyspell' for particular programming modes
(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg. For use in hooks."
  (interactive)
  (flyspell-mode 1))

;; speed up spell check
(setq flyspell-issue-message-flag nil)

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                clojure-mode-hook
                python-mode-hook
                shell-mode-hook
                java-mode
                js-mode-hook
                c-mode-common-hook))
  (add-hook hook 'flyspell-prog-mode))

;; Define where to spell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; 'WordNet' installed from source and available in PATH
(defvar wordnet-bin-path "wn"
  "This should point to the full path of the wordnet command")

(defun my/wordnet-current-word ()
  "Shows the Wordnet overview for the current word."
  (interactive)
  (save-window-excursion
    (let ((buf (get-buffer-create "*wordnet*"))
          (word (get-current-word)))
      (save-window-excursion
        (set-buffer buf)
        (clear-buffer buf)
        (insert (concat "Wordnet overview for " word ": "))
        (call-process wordnet-bin-path nil "*wordnet*" t word "-over")
        (switch-to-buffer "*wordnet*")
        (beginning-of-buffer)
        (read-string "Press Enter to continue… ")))))

(defun get-current-word ()
  "Returns the current, or the last entered word."
  (save-excursion
    (backward-word)
    (setq start (point))
    (forward-word)
    (setq end (point))
    (buffer-substring-no-properties start end)))

(defun clear-buffer (buf)
  "Clear a buffer"
  (save-excursion
    (set-buffer buf)
    (kill-region (point-min) (point-max))))

;;_______________________________________________________________________________
;;                                                                   Programming

;; Go into proper mode according to file extension
(setq auto-mode-alist
      (append '(
                ("\\.idl$"  . c++-mode)
                ("\\.ipp$"  . c++-mode)
                ("\\.i$"    . c++-mode) ; SWIG files

                ("\\.asd$"  . lisp-mode)
                ("\\.cljs$" . clojure-mode)
                ("\\.ss$"   . scheme-mode)
                ("\\.sch$"  . scheme-mode)

                ("\\.conf$" . conf-mode)
                ("\\.mak$"  . makefile-mode))

              auto-mode-alist))

(when (eval-when-compile (>= emacs-major-version 24))
  (require 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

;; Use spaces instead of tabs
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; `yasinppet' code templates (third party)
(require 'yasnippet)
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "\C-x\C-y") 'yas-expand)
(yas-global-mode 1)

(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

;; `ediff' customizations
(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Skip SCM files when grep
(setq grep-find-command
      "find . -regex '.*~$\|.*/\.\(git\|hg\|svn\)\(/\|$\)' -prune -o -type f -print | xargs -E eof-str grep -I -nH -e ")

;; 'ag'(silversearcher-ag) as better 'grep' for programmers (third party)
(require 'ag)
(setq-default ag-highlight-search t)
(global-set-key (kbd "M-?") 'ag-project)

(require 'gud)
(define-key gud-mode-map '[f5] 'gud-step)
(define-key gud-mode-map '[f6] 'gud-next)

;; GDB
(setq gdb-many-windows 1)
(add-hook 'gdb-mode-hook
          (lambda()
            ;; set a temporary breakpoint at the current line and continue executing
            (local-set-key [f10]
                           (lambda()
                             (interactive nil)
                             (gud-tbreak "%f:%l c")
                             (gud-cont "%p")))
            ;; make gdb behave more like a normal terminal
            (local-set-key [up] 'comint-previous-input)
            (local-set-key [down] 'comint-next-input)))

;; Highlight XXX style code tags in source files
(dolist (mode '(c-mode
                c++-mode
                java-mode
                lisp-mode
                clojure-mode
                scheme-mode
                emacs-lisp-mode
                python-mode))
  (font-lock-add-keywords mode '(("\\(HACK\\|FIXME\\|TODO\\)"
                                  1 font-lock-warning-face prepend))))

;;_______________________________________________________________________________
;;                                                                Source Control

;; Git client (third party)
(require 'magit)
(setq magit-commit-signoff t)

;; Full screen when do `magit-status'
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;_______________________________________________________________________________
;;                                                                  Generic Lisp

(defun lisp-family (x)
  (mapcar (lambda (hook) (add-hook hook x))
          '(lisp-mode-hook
            slime-repl-mode-hook
            clojure-mode-hook
            nrepl-mode-hook
            scheme-mode-hook
            emacs-lisp-mode-hook
            ielm-mode-hook)))

(eval-after-load "redshank-loader"
  `(redshank-setup '(lisp-mode-hook
                     slime-repl-mode-hook) t))

;; Easy parenthesis manipulation (third party)
(require 'paredit)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun setup-paredit-for-mode-map (mode-map)
  (define-key mode-map (kbd "s-<up>") 'paredit-raise-sexp)
  (define-key mode-map (kbd "s-<right>") 'paredit-forward-slurp-sexp)
  (define-key mode-map (kbd "s-<left>") 'paredit-forward-barf-sexp)
  (define-key mode-map (kbd "s-S-<left>") 'paredit-backward-slurp-sexp)
  (define-key mode-map (kbd "s-S-<right>") 'paredit-backward-barf-sexp)
  (define-key mode-map (kbd "s-8") 'paredit-wrap-round)
  (define-key mode-map (kbd "s-9") 'paredit-wrap-round-from-behind)
  (define-key mode-map (kbd "s-<backspace>") 'paredit-splice-sexp-killing-backward)
  (define-key mode-map (kbd "s-t") 'transpose-sexps))

(eval-after-load "slime-repl" '(setup-paredit-for-mode-map slime-repl-mode-map))
(eval-after-load "scheme-mode" '(setup-paredit-for-mode-map scheme-mode-map))
(eval-after-load "clojure-mode" '(setup-paredit-for-mode-map clojure-mode-map))
(eval-after-load "nrepl-mode" '(setup-paredit-for-mode-map nrepl-mode-map))

(lisp-family 'enable-paredit-mode)
(lisp-family 'highlight-parentheses-mode)

;; Making `paredit' work with `delete-selection-mode'
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

;; indent when yank in lisp modes
(defadvice yank (after indent-region activate)
  (if (member major-mode '(clojure-mode
                           emacs-lisp-mode
                           lisp-mode scheme-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

;;_______________________________________________________________________________
;;                                                                    Emacs Lisp

;; On-the-fly evaluation/substitution of emacs lisp code (third party)
(require 'litable)

;; elisp `go-to-definition' with M-. and back again with M-, (third party)
(require 'elisp-slime-nav)

(defun my/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun my/set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

;; Hit C-h f (for function) or C-h v (for variable)
(defun my/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (elisp-slime-nav-mode t)
  (my/set-up-hippie-expand-for-elisp)
  (my/remove-elc-on-save)
  (ac-emacs-lisp-mode-setup)
  (setup-paredit-for-mode-map emacs-lisp-mode-map)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-setup)

;; Emacs Lisp REPL
(add-hook 'ielm-mode-hook 'my/emacs-lisp-setup)

(defun visit-ielm ()
  (interactive)
  (if (not (get-buffer "*ielm*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ielm))
    (switch-to-buffer-other-window "*ielm*")))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'visit-ielm)

;; Need for deep recursion
(set-variable 'max-lisp-eval-depth (* 30 max-lisp-eval-depth))
(set-variable 'max-specpdl-size (* 15 max-specpdl-size))

;;_______________________________________________________________________________
;;                                                Slime (Common Lisp, Scheme)

(require 'slime)
(slime-setup '(slime-fancy slime-scratch
                           slime-editing-commands
                           slime-fuzzy
                           slime-presentations
                           slime-scheme))

(setq slime-net-coding-system 'utf-8-unix)

(defun my/set-up-slime-repl-auto-complete ()
  "Bind TAB to `indent-for-tab-command', as in regular Slime buffers."
  (local-set-key (kbd "TAB") 'indent-for-tab-command))

;; Stop SLIME's REPL from grabbing DEL
(defun my/override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

(eval-after-load 'slime
  '(progn
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (add-hook 'slime-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))
     (add-hook 'slime-repl-mode-hook 'my/set-up-slime-repl-auto-complete)
     (add-hook 'slime-repl-mode-hook 'my/override-slime-repl-bindings-with-paredit)))

(setq slime-lisp-implementations
      '((mit-scheme ("mit-scheme") :init mit-scheme-init)))

(add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))

(global-set-key "\C-z" 'slime-selector)

;;_______________________________________________________________________________
;;                                                                    MIT Scheme

(defun mit-scheme-init (file encoding)
  (setq slime-protocol-version 'ignore)
  (format "%S\n\n"
          `(begin
            (load-option 'format)
            (load-option 'sos)
            (eval
             '(construct-normal-package-from-description
               (make-package-description '(swank) '(())
                                         (vector) (vector) (vector) false))
             (->environment '(package)))
            (load ,(expand-file-name
                    "~/.emacs.d/el-packages/mit-scheme-swank/swank.scm" ; <-- insert your path
                    slime-path)
                  (->environment '(runtime swank)))
            (eval '(start-swank ,file) (->environment '(swank))))))

(defun find-mit-scheme-package ()
  (save-excursion
    (let ((case-fold-search t))
      (and (re-search-backward "^[;]+ package: \\((.+)\\).*$" nil t)
           (match-string-no-properties 1)))))

(setq slime-find-buffer-package-function 'find-mit-scheme-package)

(require 'scheme-complete)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)

(eval-after-load 'scheme
  '(define-key scheme-mode-map (kbd "\C-cc") 'scheme-smart-complete))

(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode)))

;; Run MIT Scheme
(defun mit-scheme ()
  (interactive)
  (slime 'mit-scheme))

;; Highlight macros
(defun register-scheme-keywords (keywords)
  (mapc #'(lambda (kword)
            (font-lock-add-keywords 'scheme-mode
                                    `((,(concat "\\(" kword "\\)") 1 font-lock-keyword-face))))
        keywords))

;; Example: (register-scheme-keywords '("defgen" "fluid-let"))

;;_______________________________________________________________________________
;;                                                                   Common Lisp

;; ~/projects/lisp will be scanned for .asd (see ~/.config/common-lisp)

(defun slime-max-debug ()
  "Inserts declaim max debug properties"
  (interactive)
  (insert "(declaim (optimize (debug 3) (safety 3) (speed 0) (compilation-speed 0)))"))

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

(define-key slime-mode-map (kbd "C-<tab>") 'slime-fuzzy-complete-symbol)
(define-key slime-repl-mode-map (kbd "C-<tab>") 'slime-fuzzy-complete-symbol)

;; Common Lisp HyperSpec location (this may vary)
(require 'hyperspec)
(setq common-lisp-hyperspec-root
      (concat "file://" (expand-file-name "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/")) ; <-- insert your path
      common-lisp-hyperspec-symbol-table
      (expand-file-name "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/Data/Map_Sym.txt"))   ; <-- insert your path

(defadvice common-lisp-hyperspec
  (around hyperspec-lookup-w3m () activate)
  (let* ((window-configuration (current-window-configuration))
         (browse-url-browser-function
          `(lambda (url new-window)
             (w3m-browse-url url nil)
             (let ((hs-map (copy-keymap w3m-mode-map)))
               (define-key hs-map (kbd "q")
                 (lambda ()
                   (interactive)
                   (kill-buffer nil)
                   (set-window-configuration
                    ,window-configuration)))
               (use-local-map hs-map)))))
    ad-do-it))

;;_______________________________________________________________________________
;;                                                                       Clojure

(require 'elein)
(require 'clojure-mode)

;; Use forked version
(require 'nrepl)
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/data/nrepl-history")

;; Some default eldoc facilities
(add-hook 'nrepl-connected-hook
          (lambda ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))

;; Repl mode hook
(add-hook 'nrepl-mode-hook 'subword-mode)

(require 'ac-nrepl)
(add-hook 'nrepl-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-repl-mode))

(require 'nrepl-inspect)
(define-key nrepl-mode-map (kbd "C-c C-i") 'nrepl-inspect)

(require 'nrepl-ritz)

;; Ritz middleware
(define-key nrepl-interaction-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
(define-key nrepl-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
(define-key nrepl-interaction-mode-map (kbd "C-c C-a") 'nrepl-apropos)
(define-key nrepl-mode-map (kbd "C-c C-a") 'nrepl-apropos)

;; Should be run when you are in project
(defun clojure ()
  (interactive)
  (nrepl-ritz-jack-in))

;;----------------------------------------------------------------------------
;; Switching between source and test (must be named <source>_test.clj)
;;----------------------------------------------------------------------------

(defun midje-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (test-segments (append (list "test") segments)))
    (mapconcat 'identity test-segments "/")))

(defun midje-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
  (find-file (format "%s/%s_test.clj"
                     (file-name-as-directory
                      (locate-dominating-file buffer-file-name "src/"))
                     (midje-test-for (clojure-find-ns)))))

(defun midje-implementation-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string (replace-regexp-in-string "_test" "" namespace) "\\.")))
    (mapconcat 'identity segments "/")))

(defun midje-jump-to-implementation ()
  "Jump from midje test file to implementation."
  (interactive)
  (find-file (format "%s/src/%s.clj"
                     (locate-dominating-file buffer-file-name "src/")
                     (midje-implementation-for (clojure-find-package)))))

(defun midje-jump-between-tests-and-code ()
  (interactive)
  (if (clojure-in-tests-p)
      (midje-jump-to-implementation)
    (midje-jump-to-test)))

(define-key clojure-mode-map (kbd "C-c t") 'midje-jump-between-tests-and-code)

;;_______________________________________________________________________________
;;                                                                        Prolog

;; el-get-install prolog-el do not works for now
(require 'prolog)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'swi)

(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
                              auto-mode-alist))

;;_______________________________________________________________________________
;;                                             Generic C/C++ (see CEDET settings)

;; http://google-styleguide.googlecode.com (third party)
(require 'google-c-style)

;; Support for CMake (third party)
(autoload 'cmake-mode "cmake-mode" t)

;; 'doxygen' documentation (third party)
(require 'doxymacs)

;; Highlight digits in code
(defvar font-lock-number-face 'font-lock-number-face)
(defface font-lock-number-face
  '((((class color)(background light)) (:foreground "blue")))
  "Font Lock face used to highlight literals."
  :group 'font-lock-faces)

(set-face-foreground 'font-lock-number-face "green3")

(defvar hi-numbers
  '(("\\b\\(0[xX][0-9a-fA-F]+[lL]?\\|[0-9]+\\.?[0-9]*\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b" 1
     font-lock-number-face)))

(font-lock-add-keywords 'c-mode hi-numbers)
(font-lock-add-keywords 'c++-mode hi-numbers)

(add-hook 'c-mode-common-hook
          (lambda()
            (highlight-parentheses-mode)

            (google-set-c-style)

            (doxymacs-mode t)
            (doxymacs-font-lock)
            ;; Comments are in Java doc style, but you can switch
            ;; (setq doxymacs-doxygen-style "C++")

            (local-set-key [(control J)] 'doxymacs-insert-function-comment)
            (local-set-key [(control return)] 'reindent-then-newline-and-indent)))

(add-hook 'c-mode-hook
          (lambda()
            (one-file-c-compilation)
            (local-set-key [f1] 'compile)))

(font-lock-add-keywords 'c-mode '(("^[^\n]\\{80\\}\\(.*\\)$"
                                   1 font-lock-warning-face prepend)))

(setq c-font-lock-extra-types
      '("FILE" "BOOL" "BOOLEAN" "C[A-Z]\\sw+" "\\sw+_t"))

(add-hook 'c++-mode-hook
          (lambda()
            (one-file-c++-compilation)
            (local-set-key [f1] 'compile)))

(font-lock-add-keywords 'c++-mode  '(("^[^\n]\\{100\\}\\(.*\\)$"
                                      1 font-lock-warning-face prepend)))

;; Check C++ code with 'valgrind' (third party)
(autoload 'valgrind "valgrind" "Check code with valgrid." t)

;; Makefile edit setup
(add-hook 'makefile-mode-hook
          (lambda()
            (setq makefile-electric-keys t)
            (setq show-trailing-whitespace t)))

;; Use 'cdecl' program in PATH for complex declarations
(defun do-cdecl ()
  (interactive)
  (shell-command
   (concat "cdecl explain \"" (buffer-substring (region-beginning)
                                                (region-end)) "\"")))

;;_______________________________________________________________________________
;;                                                                        Python

;; Jedi - python code completion library (third party)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method '(pos-tip))

(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; 'pyflakes' should be installed (third party)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; 'ipython' should be installed
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(require 'python)

(defun python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

;; Conditionals: if i == 5: import pytest; pytest.set_trace() ## DEBUG ##
(defvar python--pdb-breakpoint-string "import pytest; pytest.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pytest'"
  (interactive)
  (back-to-indentation)
  (split-line)
  (insert python--pdb-breakpoint-string))

(define-key python-mode-map (kbd "<f10>") 'python-insert-breakpoint)

;;_______________________________________________________________________________
;;                                                                    JavaScript

;; Requires 'v8' to be build
;; NOTE: Change GCC_TREAT_WARNINGS_AS_ERRORS in 'build/standalone.gypi' form YES to NO
(add-to-list 'load-path "~/.emacs.d/jslint-v8/")
(require 'flymake-jslint)
(add-hook 'js-mode-hook
          (lambda () (flymake-mode t)))

;;_______________________________________________________________________________
;;                                                       EShell (Debian dialect)

(setq eshell-banner-message `(format-time-string
                              "Eshell startup: %T, %A %d %B %Y\n\n"))

(setq eshell-output-filter-functions (list 'eshell-handle-control-codes
                                           'eshell-handle-ansi-color
                                           'eshell-watch-for-password-prompt))

(setq eshell-directory-name "~/.emacs.d/data/eshell/"
      eshell-hist-ignoredups t
      eshell-ask-to-save-history 'always
      eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\|\\.hg\\)/\\'")

(setq eshell-prompt-function
      (lambda nil
        (concat (getenv "USER") "@" (eshell/pwd)
                (if (= (user-uid) 0) " # " " $ "))))

(require 'ls-lisp)
(setq dired-listing-switches "-lh")

(defun eshell/deb (&rest args)
  (eshell-eval-using-options
   "deb" args
   '((?f "find" t find "list available packages matching a pattern")
     (?i "installed" t installed "list installed debs matching a pattern")
     (?l "list-files" t list-files "list files of a package")
     (?s "show" t show "show an available package")
     (?v "version" t version "show the version of an installed package")
     (?w "where" t where "find the package containing the given file")
     (nil "help" nil nil "show this usage information")
     :show-usage)
   (eshell-do-eval
    (eshell-parse-command
     (cond
      (find
       (format "apt-cache search %s" find))
      (installed
       (format "dlocate -l %s | grep '^.i'" installed))
      (list-files
       (format "dlocate -L %s | sort" list-files))
      (show
       (format "apt-cache show %s" show))
      (version
       (format "dlocate -s %s | egrep '^(Package|Status|Version):'" version))
      (where
       (format "dlocate %s" where))))
    t)))

(defun eshell/e (file)
  (find-file file))

(defun eshell/less (&rest args)
  (while args
    (view-file (pop args))))

(defun eshell/w3m (file)
  (w3m-find-file file))

(defun eshell/info ()
  (info))

(defun eshell/ll (&rest args)
  (eshell/ls "-alh" args))

(defun eshell/l (&rest args)
  (eshell/ls "-lh" args))

(defun my/toggle-eshell ()
  "Bring up a full-screen eshell or restore previous configuration"
  (interactive)
  (if (string= "eshell-mode" major-mode)
      (jump-to-register :eshell-fullscreen)
    (progn
      (window-configuration-to-register :eshell-fullscreen)
      (eshell)
      (delete-other-windows))))

;;_______________________________________________________________________________
;;                                                              USEFUL FUNCTIONS

(defun reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs.d/init.el"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun sudo-edit (&optional arg)
  "Edit files as super user"
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun insert-file-name (filename &optional args)
  "Insert full qualified file name in place"
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun dos2unix nil
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))

(defun iwb ()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun copy-line (n)
  (interactive "p")
  (kill-ring-save (line-beginning-position) (line-beginning-position (1+ n))))

(defun selective-folding ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;;_______________________________________________________________________________
;;                                                            Org-mode and Email

(load "~/.emacs.d/init-org.el")

;;_______________________________________________________________________________
;;                                                             Personal settings

(setq default-fill-column 100
      default-major-mode 'text-mode
      initial-buffer-choice "~/projects/"
      default-directory "~/projects/")

;; Use M-x describe-bindings for more information

(global-set-key (kbd "s-/") 'hippie-expand)
;; "s-a" 'org-agenda-list
(global-set-key (kbd "s-b") 'pop-global-mark)    ; back to prev location
(global-set-key (kbd "s-d") 'delete-this-buffer-and-file)
(global-set-key (kbd "s-f") 'ffap)
(global-set-key (kbd "s-g") 'grep-find)
(global-set-key (kbd "s-h") 'my/toggle-eshell)
(global-set-key (kbd "s-i") 'insert-file-name)
(global-set-key (kbd "s-o") 'occur)
(global-set-key (kbd "s-y") 'yas-ido-expand)

(global-set-key (kbd "s-.") 'goto-last-change)
(global-set-key (kbd "s-,") 'goto-last-change-reverse)

;; open files
(global-set-key (kbd "s-c")
                (lambda() (interactive)(find-file "~/.emacs.d/init-cedet.el")))
(global-set-key (kbd "s-e")
                (lambda() (interactive)(find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "s-r")
                (lambda() (interactive)(switch-to-buffer "*scratch*")))

;; Align the whole buffer
(global-set-key (kbd "\C-F") 'iwb)

(global-set-key (kbd "\C-c;") 'comment-or-uncomment-region)
(global-set-key (kbd "\C-cb") 'browse-kill-ring)
(global-set-key (kbd "\C-cc") 'copy-line)
(global-set-key (kbd "\C-cs") 'my/ido-choose-from-recentf)
(global-set-key (kbd "\C-cw") 'my/wordnet-current-word)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

(global-set-key [(control f1)] 'webjump)
(global-set-key [(control f2)] 'rename-file-and-buffer)
(global-set-key [(control f3)] 'iedit-mode)
;; [(control f4)]    'diary
(global-set-key [(control f5)] 'linum-mode)
(global-set-key [(control f6)] (lambda (beg end)
                                 (interactive "r") (my/htmlize-region beg end)))
(global-set-key [(control f7)] 'selective-folding)
(global-set-key [(control f8)] 'gdb-restore-windows)

;; Defined in .cedet

;; [(control f11)]   'ecb-toggle-methods
;; [(control f12)]   'ecb-toggle-dir

;;_______________________________________________________________________________
;;                                                                 Emacs managed

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dictionary-use-single-buffer t)
 '(diff-switches "-u")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-support-shell-wildcards nil)
 '(ls-lisp-use-insert-directory-program nil)
 '(comint-prompt-read-only t)
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("~/" "Home Directory"))))
 '(ecb-version-check nil)
 '(flymake-log-level 3)
 '(flymake-no-changes-timeout 15.0)
 '(flymake-start-syntax-check-on-newline nil)
 '(tooltip-use-echo-area t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :bold t :foreground "green4"))))
 '(diff-removed ((t (:inherit diff-changed :bold t))))
 '(ecb-tag-header-face ((((class color) (background light)) (:background "seagreen1"))))
 '(flymake-errline ((((class color)) (:underline "OrangeRed"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#bbbbbb")))))

;; Everything is loaded?
(message "All done, %s%s" (user-login-name) ". Let's the source be with you!")
