;; mode:-*-emacs-lisp-*- -*- coding: utf-8; -*-

;; Time-stamp: <2014-01-30 14:24:28 (zzhelyaz)>

;;________________________________________________________________________________
;;                                                                  Minimal CEDET

;; If you install want latest CEDET and build it with Make
;; bzr checkout bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk cedet-bzr

;; Specify CEDET install directory
;; (setq cedet-root-path (file-name-as-directory "/opt/emacs-tools/cedet"))
;;
;; (load-file (concat cedet-root-path "cedet-devel-load.el"))
;; (add-to-list 'load-path (concat cedet-root-path "contrib"))

;; select which sub-modes we want to activate
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; Activate semantic
(semantic-mode 1)

;; Load contrib library
(require 'eassist)

;; Customisation of modes
(defun my/cedet-hook ()
  ;; whatever the symbol you are typing, this hot key automatically complete it for you
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  ;; another way to complete the symbol you are typing
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;; when you typed . or -> after an object name, use this key to show possible public member functions or data members
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  ;; visit the header file under cursor
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  ;; jump to the definition of the symbol under cursor
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  ;; show documentation
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  ;; toggle between the implementation and a prototype of symbol under cursor
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))

(add-hook 'c-mode-common-hook 'my/cedet-hook)

(defun my/c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  ;; show references of the symbol under cursor
  (local-set-key "\C-c\C-r" 'semantic-symref))

(add-hook 'c-mode-common-hook 'my/c-mode-cedet-hook)

(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; SRecode
(global-srecode-minor-mode 1)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

;;________________________________________________________________________________
;;                                                                 Advanced CEDET

;; (semantic-load-enable-excessive-code-helpers)

;; Third party libraries - global for all projects (location may vary)
(defconst cedet-user-include-dirs
  (list ".." "/opt/local/include/glog" "/opt/local/include/boost"))

;; System paths
(setq cedet-sys-include-dirs (list "/usr/include" "/usr/local/include"))

(let ((include-dirs cedet-user-include-dirs))
  (setq include-dirs (append include-dirs cedet-sys-include-dirs))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))

(setq semantic-c-dependency-system-include-path "/usr/include")

;; Function to load all include files in the specified directory
(defun load-headers-from-dir (dir)
  "Add all header files in DIR to `semanticdb-implied-include-tags'."
  (let ((files (directory-files dir t "^.+\\.h[hp]*$" t)))
    (defvar-mode-local c++-mode semanticdb-implied-include-tags
      (mapcar (lambda (header)
                (semantic-tag-new-include
                 header
                 nil
                 :filename header))
              files))))

(load-headers-from-dir "/path/to/root") ; <-- insert yours

;;________________________________________________________________________________
;;                                                       auto-complete with Clang

;; Global for all projects
(defcustom mycustom-system-include-paths '("." ".."
                                           "/opt/local/include/glog"
                                           "/opt/local/include/boost"
                                           "/usr/include"
                                           "/usr/local/include")
  "Clang will complete using this directories"
  :group 'mycustom
  :type '(repeat directory))

(require 'auto-complete-clang)
(setq clang-completion-suppress-error 't)

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (append
               mycustom-system-include-paths)))

(defun my-ac-clang-mode-common-hook()
  (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang))

(add-hook 'c-mode-common-hook 'my-ac-clang-mode-common-hook)

;;_______________________________________________________________________________
;;                                                             C/C++ Compilation

(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output 'first-error)
(setq compilation-window-height 12)
(setq mode-compile-always-save-buffer-p t)

(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "compilation errors, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))

(defun one-file-c-compilation ()
  (cond ((file-exists-p "Makefile")
         (let ((file (file-name-nondirectory buffer-file-name)))
           (concat "make " "-j2")))
        (t
         (set (make-local-variable 'compile-command)
              (let ((file (file-name-nondirectory buffer-file-name)))
                (concat "gcc -g3 -std=c99 -pedantic -Wall -Wextra -DDEBUG -o "
                        (file-name-sans-extension file) " " file))))))

(defun one-file-c++-compilation ()
  (cond ((file-exists-p "Makefile")
         (let ((file (file-name-nondirectory buffer-file-name)))
           (concat "make " "-j2")))
        (t
         (set (make-local-variable 'compile-command)
              (let ((file (file-name-nondirectory buffer-file-name)))
                (concat "g++ -g3 -ggdb -ansi -pedantic -fno-inline -Wall -Wextra -DDEBUG -o "
                        (file-name-sans-extension file) " " file))))))

(defun my-javadoc-return ()
  (interactive)
  (setq last (point))
  (setq is-inside
        (if (search-backward "*/" nil t)
            ;; there are some comment endings - search forward
            (if (search-forward "/*" last t)
                't
              'nil)
          ;; it's the only comment - search backward
          (goto-char last)
          (if (search-backward "/*" nil t)
              't
            'nil)))
  ;; go to last char position
  (goto-char last)
  ;; the point is inside some comment, insert `*'
  (if is-inside
      (progn
        (insert "\n* ")
        (indent-for-tab-command))
    ;; else insert only new-line
    (insert "\n")))

(add-hook 'c++-mode-hook (lambda ()
                           (local-set-key "\r" 'my-javadoc-return)))

;;________________________________________________________________________________
;;                                                                            ECB

;; Loaded by el-get now
;; (add-to-list 'load-path "/opt/emacs-tools/ecb")

(ignore-errors (require 'ecb))
(setq stack-trace-on-error t)

(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.20)

;; Filter unwanted source file
(setq ecb-source-file-regexps
      (quote
       ((".*" (".*_flymake.cpp\\|\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(pyc\\|elc\\|fasl\\|o\\|class\\|a\\|so\\|dep\\)$\\)\\)")))))

;; Left-click mouse button to work
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))

;; Switch on and off ECB
(defun ecb-toggle-methods ()
  (interactive)
  (setq ecb-layout-name "left9")
  (if ecb-minor-mode
      (ecb-deactivate)
    (ecb-activate)))
(global-set-key [(control f11)] 'ecb-toggle-methods)

(defun ecb-toggle-dir ()
  (interactive)
  (setq ecb-layout-name "left5")
  (if ecb-minor-mode
      (ecb-deactivate)
    (ecb-activate)))
(global-set-key [(control f12)] 'ecb-toggle-dir)


;; If project file is open - project will be compiled with closest Makefile, else
;; file will be compiled with general rule if there is no Makefile
;;________________________________________________________________________________
;;                                                                 Projects Build

(defun project-compile ()
  (interactive)
  (save-some-buffers t)
  (shell-command (funcall 'project-compile-string)))
(global-set-key [(control f9)] 'project-compile)

(defun project-root ()
  (setq current-dir (file-name-directory
                     (or (buffer-file-name (current-buffer)) default-directory)))
  (setq prj (ede-current-project current-dir))
  (when prj
    (let* ((root-dir (ede-project-root-directory prj)))
      (concat "" root-dir))))

(defun project-compile-string ()
  "Generates compile string for compiling whole project"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj)))
    (concat "cd " root-dir "; make -j2")))

;;_______________________________________________________________________________
;;                                                              PROJECT TEMPLATE

;; project.template as example
(ede-cpp-root-project "C++ project.template"
                      :name "C++ project.template"
                      :file "~/projects/various-code-fragments/cpp/project.template/CMakeLists.txt"
                      :include-path '("/"
                                      ;; project specific
                                      "/extern/glog/include"
                                      "/extern/boost/include")
                      :system-include-path '("/usr/include"
                                             "/usr/include/c++/4.3"))

;;_______________________________________________________________________________
;;
