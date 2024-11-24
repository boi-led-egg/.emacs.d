;; --- packages ---
;; install system packages from here: https://realpython.com/blog/python/emacs-the-best-python-editor/
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("Marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ;; ("user42" . "http://download.tuxfamily.org/user42/elpa/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))
;; # Either of these
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

(unless package-archive-contents
  (package-refresh-contents))

(if (version< emacs-version "25")
    (message "Too old version to automatically install packages")
  (package-install-selected-packages))

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


;; --- appearance ---
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq zenburn-override-colors-alist
      '(("zenburn-bg-2"  . "#000000")
        ("zenburn-bg-1"  . "#101010")
        ("zenburn-bg-08" . "#151515")
        ("zenburn-bg-05" . "#1A1A1A")
        ("zenburn-bg"    . "#282828")
        ("zenburn-bg+05" . "#2A2A2A")
        ("zenburn-bg+1"  . "#2F2F2F")
        ("zenburn-bg+2"  . "#3F3F3F")
        ("zenburn-bg+3"  . "#4F4F4F")))
(load-theme 'zenburn t)

(custom-theme-set-faces
 'zenburn
 ;; '(highlight-numbers-number ((t (:foreground "#DC8CC3"))))
 '(region ((t (:background "#600060" :extend t))
           (t :inverse-video t)))
 '(cursor ((t (:foreground "#000000" :background "#FFAA00"))))
 '(font-lock-comment-face ((t (:foreground "#666666"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#606060"))))
 '(linum ((t (:foreground "#666666" :background "#282828"))))
)

(setq linum-format "%4d \u2502")

(setq highlight-indent-guides-auto-odd-face-perc 3)
(setq highlight-indent-guides-auto-even-face-perc 4)
(setq highlight-indent-guides-method 'column)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(global-hl-line-mode 1)
(setq inhibit-startup-message t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (nyan-mode t)
(when (require 'highlight-numbers nil 'noerror)
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))
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

;; --- keybindings and mouse ---


(global-set-key (kbd "<f1>") 'ibuffer)
(global-set-key [f5] 'highlight-symbol-at-point)
(global-set-key (kbd "<S-f5>") (lambda() (interactive) (unhighlight-regexp t)))
(global-set-key (kbd "<f6>") 'select-string-or-search-forward)
(global-set-key (kbd "<S-f6>") 'select-string-or-search-backward)
(global-set-key (kbd "<f7>") 'lsp-ui-doc-show)
(global-set-key (kbd "<S-f7>") 'lsp-ui-doc-hide)
(global-set-key (kbd "<f8>") 'lsp-ui-imenu)
(global-set-key (kbd "<f9>") 'global-linum-mode)
(global-set-key (kbd "<f12>") 'flycheck-mode)

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
;; (global-set-key [(super f5)]         (lambda ()(interactive) (point-to-register 1)))
;; (global-set-key [f5] (lambda ()(interactive) (jump-to-register 1)))
;; (global-set-key [(super f6)]         (lambda ()(interactive) (point-to-register 2)))
;; (global-set-key [f6] (lambda ()(interactive) (jump-to-register 2)))
;; (global-set-key [(super f7)]         (lambda ()(interactive) (point-to-register 3)))
;; (global-set-key [f7] (lambda ()(interactive) (jump-to-register 3)))
;; (global-set-key [(super f8)]         (lambda ()(interactive) (point-to-register 4)))
;; (global-set-key [f8] (lambda ()(interactive) (jump-to-register 4)))

(global-set-key [M-up]   (lambda () (interactive) (scroll-down 4)))
(global-set-key [M-down] (lambda () (interactive) (scroll-up 4)))
(global-set-key [?\s-/]       'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-set-key (kbd "s-w") 'copy-word)
(global-set-key (kbd "s-<left>")  'windmove-left)          ; move to left window
(global-set-key (kbd "s-<right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "s-<up>")    'windmove-up)              ; move to upper window
(global-set-key (kbd "s-<down>")  'windmove-down)          ; move to lower window
; Macroses
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
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


;; --- modes and hooks ---
;; (when (require 'elpy nil 'noerror)
;;   (elpy-enable))

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
;; (autoload 'cmake-mode "/opt/local/share/cmake-3.7/editors/emacs/cmake-mode.el" t)
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
(when (load "flycheck" t t)
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

(require 'lsp-pyls)
(setq lsp-pylsp-plugins-flake8-enabled t)
(setq lsp-pylsp-plugins-mccabe-enabled nil)
(setq lsp-pylsp-plugins-mypy-enabled t)
(setq lsp-pylsp-plugins-mypy-live-mode nil)
;; (setq lsp-enable-symbol-highlighting nil)

;; (setq lsp-pylsp-plugins-mypy-strict t)
(setq lsp-pylsp-plugins-pycodestyle-enabled nil)
(setq lsp-pylsp-plugins-pylint-enabled t)
(setq lsp-completion-provider :none)
(with-eval-after-load 'lsp-mode
  (lsp-register-custom-settings '(("pyls.plugins.jedi.extra_paths" ["/home/ikhivrenko/redfish"]))))

(add-hook 'python-mode-hook 'lsp)
;; c style settings
(defun linux-c-mode ()
  (interactive)
  (c-mode)
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq c-set-offset 'substatement-open 0)
  (setq indent-tabs-mode nil)
  (setq tab-width 8))

(setq c-default-style "linux"
      c-basic-offset 8
      tab-width 8
      indent-tabs-mode nil
      )

; Modify C/C++ syntax tables to thread underscore sign '_' as a part of word
;; (add-hook 'c-mode-common-hook 'linux-c-mode)
(add-hook 'c-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook  (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
;; (add-hook 'c++-mode-hook (lambda () (setq c-set-offset 'substatement-open 0)))
(setq cmake-tab-width 4)
(autoload 'cmake-mode "cmake-mode" "Major mode for editing CMake listfiles." t)


;; detect when editing git commit messages
;; TODO: only set width for this mode, maybe create a specific mode

(global-display-fill-column-indicator-mode 1)
(setq display-fill-column-indicator-column 120)
(setq-default fill-column 120)
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . display-fill-column-indicator-mode))

;; (require 'ccls)
;; (setq ccls-executable "/mnt/pb/artifacts/archive/tools/ccls-current/ccls")
;; (setq ccls-args '("--log-file=/tmp/ccls.log"))
;; (setq ccls-initialization-options '(
;;         ; use this option if you want to keep cache directory outside of the purity tree
;;         ; (by default it'll be created under <purity>/.ccls-cache)
;;         :cache (:directory "/home/ikhivrenko/ccls-cache")
;; 		; these options are useful if you're using text-mode emacs and want to reduce clutter
;;         :client (:snippetSupport :json-false)
;;         :completion (:detailedLabel :json-false)
;;         :index (:comments 0)))
;; (add-hook 'c++-mode-hook 'lsp)
;; (defun my-c++-mode-hook ()
;;   (local-set-key [f6] 'lsp-find-references)
;;   (local-set-key [f5] 'lsp-find-definition)
;;   (local-set-key [f7] 'lsp-find-implementation)
;;   (local-set-key [backtab] 'company-complete))
;; (add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; (setq lsp-diagnostics-provider :none)
;; (setq lsp-enable-symbol-highlighting nil)
;; (setq lsp-modeline-code-actions-enable nil)
;; (setq lsp-modeline-diagnostics-enable nil)
;; (setq lsp-signature-auto-activate nil)
;; (setq lsp-signature-render-documentation nil)
;; (setq lsp-enable-file-watchers nil)
;; --- packages ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "b0fc95a71c0d988dbb9a147ae30b11748d87987f8f818fbff84484f6bb7892d7" "23ccf46b0d05ae80ee0661b91a083427a6c61e7a260227d37e36833d862ccffc" "63dd8ce36f352b92dbf4f80e912ac68216c1d7cf6ae98195e287fd7c7f7cb189" default))
 '(elpy-modules '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv))
 '(flycheck-checker-error-threshold 5000)
 '(flycheck-indication-mode 'right-margin)
 '(flymake-start-syntax-check-on-newline t)
 '(indent-tabs-mode nil)
 '(lsp-pyls-plugins-flake8-max-line-length 140)
 '(lsp-ui-sideline-show-symbol t)
 '(package-selected-packages
   '(lsp-ui vterm company ccls highlight-indent-guides highlight-indentation cmake-mode php-mode racer flycheck-rust rust-mode json-mode highlight-numbers tao-theme pymacs projectile nyan-mode idle-highlight-mode helm ggtags flycheck exec-path-from-shell elpy zenburn-theme))
 '(safe-local-variable-values
   '((whitespace-newline . t)
     (whitespace-style face trailing lines-tail space-before-tab indentation empty)))
 '(tool-bar-mode nil))

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
 '(flycheck-delimited-error ((t (:extend t :background "color-233"))))
 '(flycheck-error-delimiter ((t (:extend t :background "color-233"))))
 '(flycheck-error-list-line-number ((t (:weight bold))))
 '(flycheck-warning ((t (:foreground "#F0DFAF" :underline t))))
 '(lsp-ui-sideline-symbol ((t (:background "black" :foreground "blue" :box (:line-width (1 . -1) :color "grey") :height 0.99)))))
(put 'upcase-region 'disabled nil)
