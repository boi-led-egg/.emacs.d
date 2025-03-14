;; add melpa to the list of packages
(require 'package)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; ---------------------------------- appearance -------------------------------

(use-package emacs
	:hook
	(prog-mode . display-fill-column-indicator-mode)

	:config
	;; increase gc pool
	(setq gc-cons-threshold (* 1024 1024 16))

	;; always highlight current line
	(global-hl-line-mode 1)
	;; hide extras
	(setq inhibit-startup-message t)
	(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
	(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
	(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
	;; don't blink cursor
	(blink-cursor-mode -1)
	;; replace bell with modeline blink
	(setq visible-bell nil)
	(setq ring-bell-function (lambda ()
								 (invert-face 'mode-line)
								 (run-with-timer 0.1 nil 'invert-face 'mode-line)))

	;; text mode doesn't need to set the font. Also ignore if not found
	(when (and (display-graphic-p) (find-font (font-spec :name "Iosevka")))
		(if (eq system-type 'darwin)
			(set-face-attribute 'default nil :font "Iosevka" :height 140 :weight 'medium)
			(set-face-attribute 'default nil :font "Iosevka" :height 120)))

	;; show column numbers in the modeline
	(setq column-number-mode t)
	;; make modeline compact if it doesn't fit on the screen
	(setq mode-line-compact 'long)
	;; remove dashes in the end of the modeline (in -nw)
	(setopt mode-line-end-spaces nil)
	;; right margin indicator in source code (prog mode)
	(setq-default fill-column 120)
	;; replace vertical border divider in text mode with a neat one
	(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
	(setq org-startup-trunacted nil)
	(setq-default indent-tabs-mode nil))

(use-package zenburn-theme
	:ensure t
	:init
	(setq zenburn-override-colors-alist
        '(("zenburn-bg-2"     . "#000000")
			 ("zenburn-bg-1"     . "#101010")
			 ("zenburn-bg-08"    . "#151515")
			 ("zenburn-bg-05"    . "#181818")
			 ("zenburn-bg"       . "#232320")
			 ("zenburn-bg+05"    . "#2A2A2A")
			 ("zenburn-bg+1"     . "#2F2F2F")
			 ("zenburn-bg+2"     . "#3F3F3F")
			 ("zenburn-bg+3"     . "#4F4F4F")

			 ("zenburn-red-6"    . "#7C1313")
			 ("zenburn-red-5"    . "#8C2323")
			 ("zenburn-red-4"    . "#9C3333")
			 ("zenburn-red-3"    . "#AC4343")
			 ("zenburn-red-2"    . "#BC5353")
			 ("zenburn-red-1"    . "#CC6363")
			 ("zenburn-red"      . "#DC7373")
			 ("zenburn-red+1"    . "#EC8383")
			 ("zenburn-red+2"    . "#FC9393")

			 ("zenburn-orange"   . "#EFBF8F")

			 ("zenburn-cyan"     . "#93c6e3")
			 ))
	:config
	(load-theme 'zenburn t)
	(custom-theme-set-faces
		'zenburn
		'(region ((t (:background "#491759" :extend t)) (t :inverse-video t)))
		'(cursor ((t (:foreground "#000000" :background "#FFAA00"))))
		'(font-lock-comment-face ((t (:foreground "#666666"))) t)
		'(font-lock-comment-delimiter-face ((t (:foreground "#666666"))))
		'(font-lock-number-face ((t (:foreground "#a999bb"))))
		'(completions-highlight ((t (:foreground "#000000" :background "#FFCCAA"))))
		)
	(enable-theme 'zenburn))

;; --------------------------- custom functions --------------------------------
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

(defun mark-whole-word ()
	"Like mark-word, but the whole word from the beginning"
	(interactive)
	(backward-word)
	(mark-word))

(defun comment-or-uncomment-region-or-line ()
	"Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))

(defun prev-window ()
	(interactive)
	(other-window -1))

;; (defun toggle-flymake-diagnostics ()
;;   (interactive)
;;   (cond ((eq (flymake--diagnostics-buffer-name) (window-buffer (selected-window)))
;;          (delete-window (get-buffer-window (flymake--diagnostics-buffer-name))))
;;         ((get-buffer-window (flymake--diagnostics-buffer-name))
;;          (delete-window (get-buffer-window (flymake--diagnostics-buffer-name))))
;;         (t (flymake-show-buffer-diagnostics))))

(defun close-flymake-diagnostics ()
	(interactive)
	(cond ((eq (flymake--diagnostics-buffer-name) (window-buffer (selected-window)))
			  (delete-window (get-buffer-window (flymake--diagnostics-buffer-name))))
        ((get-buffer-window (flymake--diagnostics-buffer-name))
			(delete-window (get-buffer-window (flymake--diagnostics-buffer-name))))
        (t (delete-window (get-buffer-window (flymake--diagnostics-buffer-name))))))

(defun flymake-show-buffer-diagnostics-focus ()
	(interactive)
	(flymake-show-buffer-diagnostics)
	(select-window (get-buffer-window (flymake--diagnostics-buffer-name))))

(defun cycle-fill-column ()
	(interactive)
	(cond
		((eq fill-column 100) (setq fill-column 120) (message "Setting fill-column to 120"))
		((eq fill-column 120) (setq fill-column 100) (message "Setting fill-column to 100"))))

(defun my-save-word ()
	(interactive)
	(let ((current-location (point))
			 (word (flyspell-get-word)))
		(when (consp word)
			(flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

;; --------------------------- keybindings and mouse ---------------------------
;; TODO: replace f1 with help (eldoc show info custom function)
(global-set-key (kbd "<f1>") 'ibuffer)
;; f2 -- termux meta key
(global-set-key [S-f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f3] 'kmacro-end-or-call-macro)
;; (global-set-key [f4])
(global-set-key [f5] 'flyspell-prog-mode)
;; (global-set-key [S-f5] 'flyspell-mode)
;; (global-set-key [f6] ')
;; (global-set-key [f7] ')
(global-set-key [f8] 'imenu-list)
;; (global-set-key [C-f8] 'imenu-list-quit-window)
;; display modes
(global-set-key [f9] 'display-line-numbers-mode)
(global-set-key [S-f9] 'whitespace-mode)
(global-set-key [M-f9] 'cycle-fill-column)
;; (global-set-key [M-f9] ') ;; todo: indent guides? also KDE seems to use it
;; f10 - default emacs menu
;; f11 - OS full screen
;; flymake keys
(define-key prog-mode-map [f12] 'flymake-mode)
(define-key prog-mode-map [S-f12] 'flymake-show-buffer-diagnostics-focus)
;; close diagnostics window from either original buffer window or diag window
(global-set-key [M-f12] 'close-flymake-diagnostics)

;; TODO: use tab, backtab?
(define-key completion-in-region-mode-map (kbd "M-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "M-n") #'minibuffer-next-completion)

;; copy behavior from terminal
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-h" 'backward-delete-char)

(global-set-key (kbd "C-c k") 'kill-region)
(global-set-key (kbd "C-c w") 'mark-whole-word)
(global-set-key (kbd "C-c s") 'select-string)
(global-set-key (kbd "C-c i") 'my-save-word)
;; (global-set-key [M-up]   (lambda () (interactive) (scroll-down 4)))
;; (global-set-key [M-down] (lambda () (interactive) (scroll-up 4)))
;; (global-set-key [?\s-/]       'comment-or-uncomment-region-or-line)
;; (global-set-key (kbd "C-c /") 'comment-or-uncomment-region-or-line)

;; reassigned from standard comment-dwim because I don't use it
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)
;; (global-set-key (kbd "s-w") 'copy-word)
;; (global-set-key (kbd "s-<left>")  'windmove-left)          ; move to left window
;; (global-set-key (kbd "s-<right>") 'windmove-right)        ; move to right window
;; (global-set-key (kbd "s-<up>")    'windmove-up)              ; move to upper window
;; (global-set-key (kbd "s-<down>")  'windmove-down)          ; move to lower window

										; Buffers manipulation
;; (global-set-key (kbd "<C-tab>") 'next-buffer)
;; (global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)
;; (global-set-key (kbd "C-;") 'company-complete)
;; Yank menu
(global-set-key (kbd "C-c C-y") '(lambda ()
									 (interactive) (popup-menu 'yank-menu)))

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
										;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; -------------------------- behavior -----------------------------------------
;; mouse integration in terminal
(xterm-mouse-mode 1)

;; completions
(setq completions-max-height 16)
;; (setq completions-format "vertical")
;; suppress useless help header message in completion buffer
(setq completion-show-help nil)
(setq completions-detailed t)
(setopt tab-always-indent 'complete)
(setq read-file-name-completion-ignore-case t)

(autoload 'ibuffer "ibuffer" "List buffers." t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (electric-indent-mode -1)
(setq save-interprogram-paste-before-kill t)
(global-auto-revert-mode 1)
;; (when window-system (set-exec-path-from-shell-PATH))

;; suppress spam from natice compilation
;; (setq native-comp-async-report-warnings-errors 'silent)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq cmake-tab-width 4)

;; (setq indent-line-function 'insert-tab)

(add-hook 'text-mode-hook
    (lambda() (setq indent-line-function 'insert-tab)))

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq use-short-answers t)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; dired
(put 'dired-find-alternate-file 'disabled nil)
(setf dired-kill-when-opening-new-dired-buffer t)

;; Put backup files neatly away
(let ((backup-dir "~/.backups-emacs/backups")
		 (auto-saves-dir "~/.backups-emacs/auto-saves/"))
	(dolist (dir (list backup-dir auto-saves-dir))
		(when (not (file-directory-p dir))
			(make-directory dir t)))
	(setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
    delete-old-versions t  ; Clean up the backups
    version-control t      ; Use version numbers on backups,
    kept-new-versions 5    ; keep some new versions
    kept-old-versions 2)   ; and some old ones, too

;; auto-install treesit libraries
(use-package treesit-auto
	:ensure t
	:custom
	(treesit-auto-install 'prompt)
	:config
	(treesit-auto-add-to-auto-mode-alist 'all)
	(global-treesit-auto-mode))

;; (defun flymake-after-change-function (start stop _len)
;;   "Start syntax check for current buffer if it isn't already running."
;;   (let((new-text (buffer-substring start stop)))
;;     (when (and flymake-start-syntax-check-on-newline (equal new-text "\n"))
;;       (flymake-log 3 "starting syntax check as new-line has been seen")
;;       (flymake-start-syntax-check))
;;     (setq flymake-last-change-time (flymake-float-time))))

;; (use-package flymake
;;   :config
;;   (setq flymake-start-syntax-check-on-newline t))

;; (setq flymake-start-syntax-check-on-newline t)
;; (setq flymake-no-changes-timeout 1)
(use-package eglot
	:config
	(setq-default eglot-workspace-configuration
        '((:pylsp . (:plugins (
								  :pylint (:enabled t)
								  :pycodestyle (:enabled nil)
								  :mccabe (:enabled nil)
								  :pyflakes (:enabled t)
								  :flake8 (:enabled t)
								  :ruff (:enabled nil)
								  :pydocstyle (:enabled nil)
								  :yapf (:enabled nil)
								  :autopep8 (:enabled nil)
								  :mypy (:enabled t)
								  )))))
	;; (setq eglot-extend-to-xref t)
	:hook
	(prog-mode . eglot-ensure))

(add-hook 'python-mode-hook
	(lambda ()
		(setq indent-tabs-mode nil)
		(setq tab-width 4)
		(setq python-indent 4)))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; c/c++ style settings
(defun linux-c-mode ()
	(interactive)
	(c-mode)
	(setq c-indent-level 4)
	(setq c-brace-imaginary-offset 0)
	(setq c-brace-offset -4)
	(setq c-argdecl-indent 4)
	(setq c-label-offset -4)
	(setq c-continued-statement-offset 4)
	(setq c-set-offset 'substatement-open 0)
	(setq indent-tabs-mode nil)
	(setq tab-width 4))

(setq c-default-style "linux"
    c-basic-offset 4
    tab-width 4
    indent-tabs-mode nil)

(setq lisp-indent-offset 4)

;; Modify C/C++ syntax tables to thread underscore sign '_' as a part of word
;; (add-hook 'c-mode-common-hook 'linux-c-mode)
(add-hook 'c-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'python-ts-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
;; (add-hook 'c++-mode-hook (lambda () (setq c-set-offset 'substatement-open 0)))

;; run garbage collect when emacs loses focus
(add-function :after
    after-focus-change-function
    (lambda () (unless (frame-focus-state) (garbage-collect))))

;; enable C-x C-u: upcase-region
;; (put 'upcase-region 'disabled nil)

(use-package helm
	:ensure t)

(use-package marginalia
	:ensure t
	:init
	(marginalia-mode))

(use-package imenu-list
	:ensure t)

(use-package exec-path-from-shell
	:ensure t
	:config
	(when (eq system-type 'darwin)
		(exec-path-from-shell-initialize)))

;; (use-package direnv
;;   :ensure t
;;   :config (direnv-mode))

(use-package envrc
	:hook (after-init . envrc-global-mode))

(use-package flyspell
	:hook
	(emacs-lisp-mode . flyspell-prog-mode)
	(prog-mode . flyspell-prog-mode)
	(org-mode . flyspell-mode)
	:custom
	(ispell-program-name "aspell")
	;; Default dictionary. To change do M-x ispell-change-dictionary RET.
	;; (aspell-dictionary "en_GB-ise-wo_accents")
	(aspell-program-name "/usr/bin/aspell")
	;; (ispell-dictionary "en_GB-ise-wo_accents")
	(ispell-program-name "/usr/bin/aspell"))
	;; :config
	;; (add-hook 'org-mode-hook 'flyspell-mode)
	;; ;; Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.
	;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode))

;; (use-package flyspell-correct
;;   :bind ("C-;" . flyspell-correct-wrapper))

;; (use-package flycheck-aspell
;; 	:ensure t
;; 	:config
;; 	(setq ispell-program-name "aspell")
;; 	:hook (text-mode . flymake-aspell-setup))

;; (setq confirm-kill-emacs 'yes-or-no-p)
