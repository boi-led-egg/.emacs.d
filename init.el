
;; packages
;; install system packages from here: https://realpython.com/blog/python/emacs-the-best-python-editor/
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("user42" . "http://download.tuxfamily.org/user42/elpa/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))
;;# Either of these
;; pip install rope
;; pip install jedi
;; # flake8 for code checks
;; pip install flake8
;; # importmagic for automatic imports
;; pip install importmagic
;; # and autopep8 for automatic PEP8 formatting
;; pip install autopep8
;; # and yapf for code formatting
;; pip install yapf

;; pip install jedi flake8 importmagic autopep8

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv)))
 '(flymake-start-syntax-check-on-newline t)
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (elpy highlight-numbers tao-theme cmake-mode projectile flycheck elpy zenburn-theme company)))
 '(safe-local-variable-values
   (quote
    ((whitespace-newline . t)
     (whitespace-style face trailing lines-tail space-before-tab indentation empty))))
 '(tool-bar-mode nil))

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; (defun ensure-package-installed (&rest packages)
;;   "Assure every package is installed, ask for installation if it’s not.

;; Return a list of installed packages or nil for every skipped package."
;;   (mapcar
;;    (lambda (package)
;;      ;; (package-installed-p 'evil)
;;      (if (package-installed-p package)
;;          nil
;;        (if (y-or-n-p (format "Package %s is missing. Install it? " package))
;;            (package-install package)
;;          package)))
;;    packages))



;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
;; (or (file-exists-p package-user-dir)
;;     (package-refresh-contents))
;; (package-refresh-contents)
;; (ensure-package-installed 'zenburn-theme 'flycheck 'cmake-mode 'company 'highlight-numbers 'elpy)




;; (setq do-init-updates nil)
;; (with-temp-buffer
;;   (insert-file-contents (concat user-emacs-directory "init.el"))
;;   (setq current-initel-md5 (secure-hash 'md5 (buffer-string))))

;; (if (file-exists-p (concat user-emacs-directory "init.el.hash"))
;;     (progn
;;       (with-temp-buffer
;;         (insert-file-contents (concat user-emacs-directory "init.el.hash"))
;;         (setq old-initel-md5 (buffer-string)))
;;       (if (not (string-equal old-initel-md5 current-initel-md5))
;;           (setq do-init-updates t))
;;   (progn
;;     (setq do-init-updates t))))

;; (if (eq do-init-updates t)
;;     (progn
;;       (print "needs updates")
;;       (with-temp-file (concat user-emacs-directory "init.el.hash")
;;         (insert current-initel-md5))))
;;       ;; (package-refresh-contents)
;;       ;; (setq auto-install-packages
;;       ;;       '(flycheck
;;       ;;         cmake-mode
;;       ;;         company
;;       ;;         zenburn-theme
;;       ;;         highlight-numbers
;;       ;;         elpy
;;       ;;         ))
;;       ;; (dolist (pkg auto-install-packages)
;;       ;;   (unless (package-installed-p pkg)
;;       ;;     (package-install pkg)))))

;;   ;; 4) search for cmake file




;; appearance
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(if (display-graphic-p)
    (progn
      (load-theme 'zenburn t)
      (custom-theme-set-faces
       'zenburn
       '(highlight-numbers-number ((t (:foreground "#DC8CC3")))))
      (if (eq system-type 'gnu/linux)
          (set-default-font "Inconsolata 13")))
  (progn
    (load-theme 'zenburn-tty t)
    (setq linum-format "%4d \u2502")))

(global-hl-line-mode 1)
(setq inhibit-startup-message t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (nyan-mode t)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(setq blink-cursor-blinks 0)

(setq visible-bell nil)
(setq ring-bell-function (lambda ()
(invert-face 'mode-line)
(run-with-timer 0.1 nil 'invert-face 'mode-line)))

; Enable parenthesis highlighting
(show-paren-mode t)
(setq show-paren-delay 0)
(put 'scroll-left 'disabled nil)
;;(global-linum-mode t) ;; enable line numbers globally


;; custom functions
(defun smart-beginning-of-line ()
"Move point to first non-whitespace character or beginning-of-line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (beginning-of-line)
    (and (= oldpos (point))
	 (back-to-indentation))))

(defun select-string ()
"Select a string under cursor. String is considered as any sequence
enclosed by \" ... \" characters, therefore, may
actually became a place between strings instead"
 (interactive)
 (let (b1 b2)
   (skip-chars-backward "^\"")
   (setq b1 (point))
   (skip-chars-forward "^\"")
   (setq b2 (point))
   (set-mark b1)))

(defun comment-or-uncomment-region-or-line ()
"Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(defun select-string-or-search-forward ()
"Select a word if selection is empty, selects next same region otherwise."
  (interactive)
  (let (str b1 b2)
    (if (region-active-p)
        (progn
          (setq str (buffer-substring-no-properties
                     (region-beginning)
                     (region-end)))
          (if (search-forward str nil (lambda ()))
              (progn
                (deactivate-mark)
                (set-mark (match-end 0))
                (set-mark (match-beginning 0)))
            (progn
              (deactivate-mark)
              (message "Starting search from beginning of the file")
              (goto-char (point-min))
              (search-forward str)
              (set-mark (match-end 0))
              (set-mark (match-beginning 0)))))
      (progn
        (skip-chars-backward "-_A-Za-z0-9")
        (setq b1 (point))
        (skip-chars-forward "-_A-Za-z0-9")
        (setq b2 (point))
        (setq str (buffer-substring-no-properties b1 b2))
        (set-mark b2)
        (set-mark b1)))))

(defun select-string-or-search-backward ()
"Select a word if selection is empty, selects previous same region otherwise."
  (interactive)
  (let (str b1 b2)
    (if (region-active-p)
        (progn
          (setq str (buffer-substring-no-properties
                     (region-beginning)
                     (region-end)))
          (if (search-backward str nil (lambda ()))
              (progn
                (deactivate-mark)
                (set-mark (match-beginning 0))
                (set-mark (match-end 0)))
            (progn
              (deactivate-mark)
              (message "Starting search from beginning of the file")
              (goto-char (point-max))
              (search-backward str)
              (set-mark (match-beginning 0))
              (set-mark (match-end 0)))))
      (progn
        (skip-chars-backward "-_A-Za-z0-9")
        (setq b1 (point))
        (skip-chars-forward "-_A-Za-z0-9")
        (setq b2 (point))
        (setq str (buffer-substring-no-properties b1 b2))
        (set-mark b2)
        (set-mark b1)))))

(defun copy-word ()
 "Select a word under cursor and copy it into kill ring.
“word” here is considered any alphanumeric sequence with “_” or “-”."
  (interactive)
  (let (b1 b2)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq b1 (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (setq b2 (point))
    (kill-ring-save b1 b2)
    (set-mark b1)))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(defun prev-window ()
  (interactive)
  (other-window -1))





;; keybindings and mouse
;; space cadet mode
(define-key key-translation-map (kbd "<f13> p") (kbd "δ"))
(define-key key-translation-map (kbd "<f13> t") (kbd "⊂"))
(define-key key-translation-map (kbd "<f13> y") (kbd "⊃"))
(define-key key-translation-map (kbd "<f13> j") (kbd "←"))
(define-key key-translation-map (kbd "<f13> k") (kbd "→"))
(define-key key-translation-map (kbd "<f13> l") (kbd "↔"))
(define-key key-translation-map (kbd "<f13> i") (kbd "∞"))
(define-key key-translation-map (kbd "<f13> e") (kbd "∩"))
(define-key key-translation-map (kbd "<f13> w") (kbd "∨"))
(define-key key-translation-map (kbd "<f13> c") (kbd "≠"))
(define-key key-translation-map (kbd "<f13> n") (kbd "≤"))
(define-key key-translation-map (kbd "<f13> m") (kbd "≥"))
(define-key key-translation-map (kbd "<f13> a") (kbd "∧"))
(define-key key-translation-map (kbd "<f13> r") (kbd "∪"))

;; (global-set-key (kbd "<f2>") 'dired)
(global-set-key (kbd "<f1>") 'ibuffer)
(global-set-key (kbd "<f12>") 'flycheck-mode)
(global-set-key (kbd "<f9>") 'global-linum-mode)
;; (global-set-key "\C-x\C-m" 'execute-extended-command)
;; (global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [(hyper h)] 'help-command)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key [(super z)] 'undo)                ; undo
(global-set-key [(super x)] 'kill-region)         ; cut
(global-set-key [(super c)] 'copy-region-as-kill) ; copy
(global-set-key [(super v)] 'yank)                ; paste

(global-set-key [(super o)] 'other-window)
(global-set-key [(super p)] 'prev-window)

;; Navigation, press [s-f<n>] to mark a point, and then f<n> to jump back to it
(global-set-key [(super f5)]         (lambda ()(interactive) (point-to-register 1)))
(global-set-key [f5] (lambda ()(interactive) (jump-to-register 1)))
(global-set-key [(super f6)]         (lambda ()(interactive) (point-to-register 2)))
(global-set-key [f6] (lambda ()(interactive) (jump-to-register 2)))
(global-set-key [(super f7)]         (lambda ()(interactive) (point-to-register 3)))
(global-set-key [f7] (lambda ()(interactive) (jump-to-register 3)))
(global-set-key [(super f8)]         (lambda ()(interactive) (point-to-register 4)))
(global-set-key [f8] (lambda ()(interactive) (jump-to-register 4)))

(global-set-key [M-up] (lambda () (interactive) (scroll-down 4)))
(global-set-key [M-down] (lambda () (interactive) (scroll-up 4)))
(global-set-key [?\s-/] 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "s-d") 'select-string-or-search-forward)
(global-set-key (kbd "s-D") 'select-string-or-search-backward)
(global-set-key (kbd "s-w") 'copy-word)
(global-set-key (kbd "s-<left>") 'windmove-left)          ; move to left window
(global-set-key (kbd "s-<right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "s-<up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "s-<down>") 'windmove-down)          ; move to lower window
; Macroses
;(global-set-key (kbd "s-9") 'kmacro-start-macro-or-insert-counter)
;(global-set-key (kbd "s-0") 'kmacro-end-or-call-macro)
; Buffers manipulation
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)
;; (global-set-key (kbd "s-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-;") 'company-complete)
;; Yank menu
(global-set-key (kbd "C-c C-y") '(lambda ()
    (interactive) (popup-menu 'yank-menu)))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling






;; modes and hooks
(elpy-enable)
(autoload 'ibuffer "ibuffer" "List buffers." t)

; Always use one buffer in dired mode
(add-hook 'dired-mode-hook
          (lambda ()
                                        ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
            (define-key dired-mode-map (kbd "^")
              (lambda ()
                (interactive)
                (find-alternate-file "..")))))

(electric-indent-mode -1)
;; Disable all version control
(setq vc-handled-backends nil)

; Add cmake listfile names to the mode list.
;(setq auto-mode-alist
;	  (append
;	   '(("CMakeLists\\.txt\\'" . cmake-mode))
;	   '(("\\.cmake\\'" . cmake-mode))
;	   auto-mode-alist))

;; TODO read file location from save file
(autoload 'cmake-mode "/opt/local/share/cmake-3.7/editors/emacs/cmake-mode.el" t)
(setq save-interprogram-paste-before-kill t)
(global-auto-revert-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(when window-system (set-exec-path-from-shell-PATH))
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; misc

;; (autoload 'python-pylint "python-pylint")
;; (autoload 'pylint "python-pylint")

;(toggle-diredp-find-file-reuse-dir 1)

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

;; c style settings
(setq c-default-style "linux"
          c-basic-offset 4
          tab-width 4
          indent-tabs-mode nil)

(defun linux-c-mode ()
  (interactive)
  (c-mode)
  (setq c-indent-level 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

; Modify C/C++ syntax tables to thread underscore sign '_' as a part of word
(add-hook 'c-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
;; (add-hook 'c-mode-common-hook (lambda ()
;;     (font-lock-add-keywords nil '(

;;         ; Valid hex number (will highlight invalid suffix though)
;;         ;("\\b0x[[:xdigit:]]+[uUlL]*\\b" . font-lock-string-face)

;;         ; Invalid hex number
;;         ;("\\b0x\\(\\w\\|\\.\\)+\\b" . font-lock-warning-face)

;;         ; Valid floating point number.
;;         ("\\(\\b[0-9]+\\|\\)\\(\\.\\)\\([0-9]+\\(e[-]?[0-9]+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b" (1 font-lock-type-face) (3 font-lock-type-face))

;;         ; Invalid floating point number.  Must be before valid decimal.
;;         ;("\\b[0-9].*?\\..+?\\b" . font-lock-warning-face)

;;         ; Valid decimal number.  Must be before octal regexes otherwise 0 and 0l
;;         ; will be highlighted as errors.  Will highlight invalid suffix though.
;;         ("\\b\\(\\(0\\|[1-9][0-9]*\\)[uUlL]*\\)\\b" 1 font-lock-type-face)

;;         ; Valid octal number
;;         ;("\\b0[0-7]+[uUlL]*\\b" . font-lock-type-face)

;;         ; Floating point number with no digits after the period.  This must be
;;         ; after the invalid numbers, otherwise it will "steal" some invalid
;;         ; numbers and highlight them as valid.
;;         ;("\\b\\([0-9]+\\)\\." (1 font-lock-type-face))

;;         ; Invalid number.  Must be last so it only highlights anything not
;;         ; matched above.
;;         ;("\\b[0-9]\\(\\w\\|\\.\\)+?\\b" . font-lock-warning-face)
;;     ))
;; ))

;;(when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable gtags in mac os
(if (eq system-type 'darwin)
    (setq load-path (cons "/opt/local/bin/gtags/" load-path)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
