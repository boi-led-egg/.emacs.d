(deftheme zenburn-tty
  "zenburn-tty theme")

(custom-theme-set-faces
 'zenburn-tty

;; basic
 '(button ((t (:underline t))))
 '(default ((t (:background "black" :foreground "white"))))
 '(mouse ((t (:foregound "black"))))
 '(cursor ((t (:foregound "red"))))
 '(border ((t (:foregound "blue"))))
 '(highlight ((t (:underline t))))

 '(bold ((t (:underline t :background "black" :foreground "white"))))
 '(bold-italic ((t (:underline t :foreground "white"))))
 '(calendar-today-face ((t (:underline t))))
 '(diary-face ((t (:foreground "red"))))

 '(holiday-face ((t (:background "cyan"))))
 '(info-menu-5 ((t (:underline t))))
 '(info-node ((t (:italic t :bold t))))
 '(info-xref ((t (:bold t))))
 '(italic ((t (:underline t :background "red"))))
 '(message-cited-text-face ((t (:foreground "red"))))
 '(message-header-cc-face ((t (:bold t :foreground "green"))))
 '(message-header-name-face ((t (:foreground "green"))))
 '(message-header-newsgroups-face ((t (:italic t :bold t :foreground "yellow"))))
 '(message-header-other-face ((t (:foreground "#b00000"))))
 '(message-header-subject-face ((t (:foreground "green"))))
 '(message-header-to-face ((t (:bold t :foreground "green"))))
 '(message-header-xheader-face ((t (:foreground "blue"))))
 '(message-mml-face ((t (:foreground "green"))))
 '(message-separator-face ((t (:foreground "blue"))))

 '(modeline ((t (:background "white" :foreground "blue"))))
 '(modeline-buffer-id ((t (:background "white" :foreground "red"))))
 '(modeline-mousable ((t (:background "white" :foreground "magenta"))))
 '(modeline-mousable-minor-mode ((t (:background "white" :foreground "yellow"))))
 '(region ((t (:background "white" :foreground "black"))))
 '(zmacs-region ((t (:background "cyan" :foreground "black"))))
 '(secondary-selection ((t (:background "blue"))))
 '(show-paren-match-face ((t (:background "red"))))
 '(show-paren-mismatch-face ((t (:background "magenta" :foreground "white"))))
 '(underline ((t (:underline t))))
;; isearch
 '(isearch ((t (:foreground "black" :weight bold :background "white"))))
 '(isearch-fail ((t (:foreground "black" :background "red"))))
 '(lazy-highlight ((t (:foreground "black" :weight bold :background "yellow"))))
;; font lock
 '(font-lock-builtin-face ((t (:foreground "blue" :bold t))))
 '(font-lock-comment-face ((t (:foreground "grey"))))
 '(font-lock-constant-face ((t (:foreground "magenta"))))
 '(font-lock-doc-face ((t (:foreground "green"))))
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:foreground "yellow" :bold t))))
 '(font-lock-preprocessor-face ((t (:foreground "blue"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(font-lock-type-face ((t (:foreground "blue"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(font-lock-warning-face ((t (:bold t :foreground "yellow"))))

;;;;; linum-mode
 '(linum ((t (:foreground "magenta" :background "black")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zenburn-tty)
