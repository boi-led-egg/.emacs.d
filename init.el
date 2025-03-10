(setq gc-cons-threshold (* 1024 1024 16))

(package-initialize)
(require 'package)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.org/packages/") t)

(dolist (package '(use-package))
	(unless (package-installed-p package)
		(package-install package)))

;; ---------------------------------- appearance -------------------------------
(set-frame-font "Iosevka 12")
(global-hl-line-mode 1)
(setq inhibit-startup-message t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (setq blink-cursor-blinks 0) ;; don't stop blinking
;; don't blink cursor
(blink-cursor-mode -1)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(put 'scroll-left 'disabled nil)
;; show column numbers in the modeline
(setq column-number-mode t)
;; make modeline compact if it doesn't fit on the screen
(setq mode-line-compact 'long)
;; remove dashes in the end of the modeline (in -nw)
(setopt mode-line-end-spaces nil)
;; right margin indicator in source code
(setq-default fill-column 120)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(use-package zenburn-theme
  :ensure t
  :init
  (setq zenburn-override-colors-alist
        '(("zenburn-bg-2"     . "#000000")
          ("zenburn-bg-1"     . "#101010")
          ("zenburn-bg-08"    . "#151515")
          ("zenburn-bg-05"    . "#181818")
          ("zenburn-bg"       . "#202020")
          ("zenburn-bg+05"    . "#2A2A2A")
          ("zenburn-bg+1"     . "#2F2F2F")
          ("zenburn-bg+2"     . "#3F3F3F")
          ("zenburn-bg+3"     . "#4F4F4F")

          ("zenburn-red-6"    . "#7C2323")
          ("zenburn-red-5"    . "#8C3333")
          ("zenburn-red-4"    . "#9C4343")
          ("zenburn-red-3"    . "#AC5353")
          ("zenburn-red-2"    . "#BC6363")
          ("zenburn-red-1"    . "#CC7373")
          ("zenburn-red"      . "#DC8383")
          ("zenburn-red+1"    . "#EC9393")
          ("zenburn-red+2"    . "#FCA3A3")

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
  (enable-theme 'zenburn)
  )

;; (set-face-background 'default "undefined")

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

(defun comment-or-uncomment-region-or-line ()
"Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

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

;; --------------------------- keybindings and mouse ----------------------------------------------

(global-set-key (kbd "<f1>") 'ibuffer)
;; f2 -- termux meta key
(global-set-key [S-f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f3] 'kmacro-end-or-call-macro)
;; (global-set-key [f4] 'git-blame-line)
;; (global-set-key [f5] ')
;; (global-set-key [f6] ')
;; (global-set-key [f7] ')
;; (global-set-key [f8] ')
(global-set-key [f9] 'display-line-numbers-mode)
;; f10 - default emacs menu
;; f11 - OS fullscreen
(define-key prog-mode-map [f12] 'flymake-mode)
(define-key prog-mode-map [S-f12] 'flymake-show-buffer-diagnostics-focus)
;; close diagnostics window from either original buffer window or diag window
(global-set-key [M-f12] 'close-flymake-diagnostics)

;; TODO: use tab, backtab?
(define-key completion-in-region-mode-map (kbd "M-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "M-n") #'minibuffer-next-completion)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [(hyper h)] 'help-command)
;; (global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

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

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; -------------------------- modes and hooks ----------------------------------

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(xterm-mouse-mode 1) ;; mouse integration in terminal
(setq completions-max-height 16)
;; (setq completions-format "vertical")
;; suppress useless help header message in completion buffer
(setq completion-show-help nil)
(setq completions-detailed t)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(electric-indent-mode -1)
(setq save-interprogram-paste-before-kill t)
(global-auto-revert-mode 1)
(when window-system (set-exec-path-from-shell-PATH))
(setq native-comp-async-report-warnings-errors 'silent)

;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-line-function 'insert-tab t)
 '(indent-tabs-mode t)
 '(package-selected-packages nil)
 '(tab-width 4))
(add-hook 'text-mode-hook
      (lambda() (setq indent-line-function 'insert-tab)))
; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; dired
(put 'dired-find-alternate-file 'disabled nil)
(setf dired-kill-when-opening-new-dired-buffer t)

;; Disable all version control
;; (setq vc-handled-backends nil)


(setq cmake-tab-width 4)

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


;; (fido-vertical-mode)

;; (use-package idle-highlight-mode
;;   :config (setq idle-highlight-idle-time 0.2)
;;   :hook ((prog-mode text-mode) . idle-highlight-mode))

;; (defun flymake-after-change-function (start stop _len)
;;   "Start syntax check for current buffer if it isn't already running."
;;   (let((new-text (buffer-substring start stop)))
;;     (when (and flymake-start-syntax-check-on-newline (equal new-text "\n"))
;;       (flymake-log 3 "starting syntax check as new-line has been seen")
;;       (flymake-start-syntax-check))
;;     (setq flymake-last-change-time (flymake-float-time))))

(use-package flymake
  :config
  (setq flymake-start-syntax-check-on-newline t)
  ;; (setq flymake-no-changes-timeout nil)
)
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
  :hook
  (prog-mode . eglot-ensure))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

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

; Modify C/C++ syntax tables to thread underscore sign '_' as a part of word
;; (add-hook 'c-mode-common-hook 'linux-c-mode)
(add-hook 'c-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
;; (add-hook 'c++-mode-hook (lambda () (setq c-set-offset 'substatement-open 0)))

;; run garbage collect when emacs loses focus
(add-function :after
              after-focus-change-function
              (lambda () (unless (frame-focus-state) (garbage-collect))))

;; enable C-x C-u: upcase-region
(put 'upcase-region 'disabled nil)
(setopt tab-always-indent 'complete)
(setq read-file-name-completion-ignore-case t)
