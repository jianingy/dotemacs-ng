;;; ui --- generic UI configurations
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Color Themes & Layout tuning
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dracula-theme
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
  (doom-modeline-inactive-bar ((t (:background "#373844"))))
  (nlinum-current-line ((t (:foreground "orange" :slant italic :weight bold))))
  :init
  (add-hook 'buffer-list-update-hook 'nby/display-header-as-margin)
  (add-hook 'emacs-startup-hook  'nby/display-header-as-margin)
  :config
  (message "Loading theme dracula")
  (load-theme 'dracula t))

(use-package monokai-theme
  :ensure
  :disabled
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
  (mode-line ((t (:background "#333" :foreground "#75715E" :box nil :weight normal))))
  (mode-line-highlight ((t (:background "deep sky blue" :foreground "#333" :box nil :weight normal))))
  (mode-line-inactive ((t (:background "#333" :foreground "#75715E" :box nil :weight normal))))
  (vertical-border ((t (:foreground "#333333"))))
  (nlinum-current-line ((t (:foreground "orange"))))
  :init
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


(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)

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
;; ℕ𝓟⧺×≠≥≤±¬∨∧∃∀λ⟿⟹⊥⊤⊢   x ⟹ => {y} => m ⇒ y ⟶ →
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package east-asian-ambiguous
  :load-path "site-lisp/east-asian-ambiguous"
  :config
  (message "[config] set east asian ambiguous width to %s" 2)
  (set-east-asian-ambiguous-width 2))

(defvar nby/x-font-latin "Consolas"
  "Font for english characters.")

(defvar nby/x-font-symbol "Unifont"
  "Font for symbols characters.")

(defvar nby/x-font-cjk "Noto Sans CJK SC"
  "Font for CJK characters.")

(defvar nby/doc-font-latin "Work Sans"
  "Font for english characters.")

(defvar nby/doc-font-symbol "Symbol"
  "Font for symbols characters.")

(defvar nby/doc-font-mono "mono"
  "Font for mono characters.")

(defvar nby/doc-font-cjk "Alibaba Sans"
  "Font for CJK characters.")

(defvar nby/font-rescales ()
  "Rescale value for chinese to match latin fonts")

;; refer to http://baohaojun.github.io/perfect-emacs-chinese-font.html
(when nby/font-rescales
  (dolist (rescale nby/font-rescales)
    (add-to-list 'face-font-rescale-alist rescale)))

;; set xft font when we are using window system
(when window-system
  (set-face-attribute 'default nil :font nby/x-font-latin)
  (set-face-attribute 'fixed-pitch nil :font nby/x-font-latin)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      nby/x-font-cjk))
  ;; set default fontset
  ;; (set-face-font 'default nby/x-font-latin)
  ;; (set-fontset-font t 'unicode nby/x-font-cjk nil 'append)
  ;; (set-fontset-font t 'unicode nby/x-font-symbol nil 'append)
  ;; (dolist (charset '(kana han cjk-misc bopomofo))
  ;;   (set-fontset-font t charset nby/x-font-cjk))

  ;; ;; create fontset for document
  ;; (let* ((fontset-name
  ;;         (create-fontset-from-ascii-font nby/doc-font-latin nil "document")))
  ;;   (set-fontset-font fontset-name 'unicode nby/doc-font-latin)
  ;;   (set-fontset-font fontset-name 'unicode nby/doc-font-cjk nil 'append)
  ;;   (dolist (charset '(kana han cjk-misc bopomofo))
  ;;     (set-fontset-font fontset-name charset (font-spec :family nby/doc-font-cjk)))
  ;;   (message "fontset %s created." fontset-name ))
  ;; (set-face-attribute 'variable-pitch nil
  ;;                     :fontset "fontset-document"
  ;;                     :font "fontset-document")

  (message "[config] set latin font to '%s', set cjk font to '%s'"
           nby/x-font-latin
           nby/x-font-cjk))

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
;; icons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package icons-in-terminal
  :load-path "site-lisp/icons-in-terminal")

(use-package all-the-icons
  :ensure
  :config
  (setq inhibit-compacting-font-caches t))

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
;; doom-modeline: A powerful and beautiful mode-line for Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :ensure
  :after all-the-icons
  :init
  (setq-default doom-modeline-height 20)
  :config
  (doom-modeline-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Treemacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :ensure
  :config
  (setq-default treemacs-space-between-root-nodes nil
                treemacs-sorting 'alphabetic-desc
                treemacs-fringe-indicator-mode nil
                treemacs-follow-mode t
                treemacs-filewatch-mode t)
  (treemacs-resize-icons 18)
  :hook
  (emacs-startup . treemacs)
  :bind
  ("M-ESC" . treemacs)
  :custom-face
  (treemacs-root-face ((t (:height 1.1)))))


(use-package treemacs-projectile
  :after treemacs projectile
  :ensure)

(use-package treemacs-magit
  :after treemacs magit
  :ensure)

;;; ends here
