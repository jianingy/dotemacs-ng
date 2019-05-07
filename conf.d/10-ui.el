;;; ui --- generic UI configurations
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Color Themes & Layout tuning
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package monokai-theme
  :ensure
  :if window-system
  :custom-face
  (mode-line ((t (:box (:line-width 2 :color "#49483E" :style nil)))))
  (company-tooltip ((t (:inherit default :foreground "white smoke" :background "#333"))))
  (company-scrollbar-bg ((t (:background "#333"))))
  (company-scrollbar-fg ((t (:background "deep sky blue"))))
  (company-tooltip-annotation ((t (:foreground "white smoke"))))
  (company-tooltip-annotation-selection ((t (:foreground "black"))))
  (company-tooltip-selection ((t (:foreground "black" :background "deep sky blue"))))
  (company-tooltip-common ((t (:foreground "orange"))))
  (company-tooltip-common-selection ((t (:foreground "black"))))
  (org-level-1 ((t (:height 1.0))))
  (org-level-2 ((t (:height 1.0))))
  (org-level-3 ((t (:height 1.0))))
  (org-level-4 ((t (:height 1.0))))
  (org-document-title ((t (:height 1.0))))
  (vertical-border ((t (:foreground "#333333"))))
  (nlinum-current-line ((t (:foreground "orange"))))
  :init
  (defun nby/display-header-as-margin ()
    (unless (string-match "^\*" (buffer-name))
      (let ((background (face-attribute 'default :background)))
        (set-face-attribute 'header-line nil
                            :background background
                            :box nil)))
    (setq header-line-format "  "))
  (add-hook 'buffer-list-update-hook 'nby/display-header-as-margin)
  (add-hook 'emacs-startup-hook  'nby/display-header-as-margin)
  :config
  (message "Loading theme monokai")
  (load-theme 'monokai t))


(use-package sublime-themes
  :disabled
  :ensure
  :if window-system
  :after (company nlinum)
  :custom-face
  (company-tooltip ((t (:inherit default :foreground "white smoke" :background "#333"))))
  (company-scrollbar-bg ((t (:background "#333"))))
  (company-scrollbar-fg ((t (:background "deep sky blue"))))
  (company-tooltip-annotation ((t (:foreground "white smoke"))))
  (company-tooltip-annotation-selection ((t (:foreground "black"))))
  (company-tooltip-selection ((t (:foreground "black" :background "deep sky blue"))))
  (company-tooltip-common ((t (:foreground "orange"))))
  (company-tooltip-common-selection ((t (:foreground "black"))))
  (vertical-border ((t (:foreground "#333333"))))
  (nlinum-current-line ((t (:foreground "orange"))))
  :init
  (defun nby/display-header-as-margin ()
    (unless (string-match "^\*" (buffer-name))
      (let ((background (face-attribute 'default :background)))
        (set-face-attribute 'header-line nil
                            :background background
                            :box nil)))
    (setq header-line-format "  "))
  (add-hook 'buffer-list-update-hook 'nby/display-header-as-margin)
  (add-hook 'emacs-startup-hook  'nby/display-header-as-margin)
  :config
  (load-theme 'spolsky t))

(use-package color-theme-modern
  :ensure)

(use-package tangotango-theme
  :ensure
  :after color-theme-modern
  :if (not window-system)
  :custom-face
  (company-tooltip ((t (:inherit default :foreground "white smoke" :background "#333"))))
  (company-scrollbar-bg ((t (:background "#333"))))
  (company-scrollbar-fg ((t (:background "deep sky blue"))))
  (company-tooltip-annotation ((t (:foreground "white smoke"))))
  (company-tooltip-annotation-selection ((t (:foreground "black"))))
  (company-tooltip-selection ((t (:foreground "black" :background "deep sky blue"))))
  (company-tooltip-common ((t (:foreground "orange"))))
  (company-tooltip-common-selection ((t (:foreground "black"))))
  (nlinum-current-line ((t (:foreground "orange"))))
  :config (load-theme 'tangotango t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Behaviors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8)
 (setq locale-coding-system 'utf-8)

(menu-bar-mode -1)
(custom-set-variables
 '(current-language-environment "UTF-8")
 '(left-fringe-width 11)                   ;; left fringe width
 '(right-fringe-width 11)                  ;; right fringe width
 '(line-spacing 3)                         ;; more line spacing
 '(display-time-day-and-date t)            ;; Show date
 '(display-time-24hr-format  t)            ;; Show time in 24-hour format
 '(show-paren-mode     t)                  ;; Togggle visualizatgion of matching parens
 '(visible-bell        nil)                ;; turn off visible bell, it looks ugly on my mac
 '(scroll-bar-mode     nil)                ;; disable scrollbar as it looks ugly
 '(tool-bar-mode       nil)                ;; disable toolbar
 '(column-number-mode  t)                  ;; display column number
 '(display-time-mode   t)                  ;; Show time on status bar
 '(blink-cursor-mode   nil)                ;; Stop cursor blinking
 '(inhibit-startup-message t))             ;; disable splash screen

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fonts: The following is for chinese-english width testing
;;
;; | 中文        | English     | 中文English | English中文 |
;; |-------------+-------------+-------------+-------------|
;; | English     | 中文English | English中文 | 中文        |
;; | 中文English | English中文 | 中文        | English     |
;; | English中文 | 中文        | English     | 中文English |
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package east-asian-ambiguous
  :load-path "site-lisp/east-asian-ambiguous"
  :config
  (message "[config] set east asian ambiguous width to %s" 2)
  (set-east-asian-ambiguous-width 2))

(defvar nby/x-font-latin "Sans"
  "Font for english characters.")

(defvar nby/x-font-cjk "Sans"
  "Font for CJK characters.")

(defvar nby/chinese-font-rescales ()
  "Rescale value for chinese to match latin fonts")

;; refer to http://baohaojun.github.io/perfect-emacs-chinese-font.html
(when nby/chinese-font-rescales
  (dolist (rescale nby/chinese-font-rescales)
    (add-to-list 'face-font-rescale-alist rescale)))

;; set xft font when we are using window system
(when window-system
  (message "[config] set latin font to '%s', set cjk font to '%s'"
           nby/x-font-latin
           nby/x-font-cjk)
  (if nby/x-font-latin
      (set-face-attribute 'default nil :font nby/x-font-latin))

  (if nby/x-font-cjk
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          nby/x-font-cjk))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scrolling: scroll one line a time (less "jumpy" than defaults)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; scroll one line at a time
 '(mouse-wheel-progressive-speed nil)            ;; don't acceleratescrolling
 '(mouse-wheel-follow-mouse t)                   ;; scroll window under mouse
 '(scroll-step 1)                                ;; scroll one line at a time
 '(scroll-margin 3)
 '(scroll-up-aggressively 0.01)
 '(scroll-down-aggressively 0.01)
 '(scroll-conservatively 10000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Winner mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package winner
  :bind ("C-c C-\\" . winner-undo)
  :config (winner-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; smart-mode-line: A powerful and beautiful mode-line for Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smart-mode-line
  :disabled
  :ensure
  :init (setq sml/no-confirm-load-theme t
              sml/theme 'respectful)
  :config (sml/setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; nlinum-mode: display line number
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nlinum
  :ensure
  :config
  (setq nlinum-format "  %4d "
        nlinum-highlight-current-line t)
                                        ;nlinum-relative-redisplay-delay .2
                                        ;nlinum-relative-offset 0)
  (add-hook 'prog-mode-hook #'nlinum-mode))

                                        ; Fix disappearing line numbers in nlinum
(use-package nlinum-hl
  :after nlinum
  :ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; popwin: popwin is a popup window manager for Emacs which makes you free
;; from the hell of annoying buffers such like *Help*, *Completions*,
;; *compilation*, and etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package popwin
  :ensure
  :config (popwin-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Override default buffer-name unique rules with more concise way
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

(use-package dimmer
  :ensure
  :diminish t
  :init (setq dimmer-exclusion-regexp
              "^\*helm.*\\|^\*Calendar.*\\|^\*Minibuf-.*")
  :config (dimmer-mode))

(use-package telephone-line
  :ensure
  :config (telephone-line-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sidebar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ov :ensure)
(use-package frame-local :ensure)
(use-package icons-in-terminal
  :if (file-exists-p (expand-file-name "~/.local/share/icons-in-terminal"))
  :load-path
  (lambda () (expand-file-name "~/.local/share/icons-in-terminal"))
  :config
  (insert (icons-in-terminal 'oct_flame)))
(use-package sidebar
  :after (ov frame-local icons-in-terminal projectile)
  :bind (("C-x t" . sidebar-buffers-open))
  :init (setq sidebar-show-hidden-files nil)
  :load-path "site-lisp/sidebar")

;;; ends here
