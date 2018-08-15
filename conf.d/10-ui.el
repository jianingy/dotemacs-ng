;;; ui --- generic UI configurations
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Color Themes & Layout tuning
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sublime-themes
  :ensure t
  :if window-system
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
;             :hook ((buffer-list-update . nby/display-header-as-margin)
;                    (emacs-startup . nby/display-header-as-margin))
  :custom-face
  (vertical-border ((t (:foreground "#333333"))))
  :config
  (load-theme 'spolsky t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Behaviors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(set-language-environment 'UTF-8)
(custom-set-variables
 '(custom-file (nby/find-local-config '(".usercustom.el" "_usercustom.el")))
 '(current-language-environment "UTF-8")
 '(left-fringe-width 11)                   ;; left fringe width
 '(right-fringe-width 11)                  ;; right fringe width
 '(line-spacing 2)                         ;; more line spacing
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

(defvar nby/x-font-latin "Sans"
  "Font for english characters.")

(defvar nby/x-font-cjk "Sans"
  "Font for CJK characters.")

(defvar nby/chinese-font-scale 1.2
  "Rescale value for chinese to match latin fonts")

;; set xft font when we are using window system
(when window-system
  (message "set latin font to '%s', set cjk font to '%s'"
           nby/x-font-latin
           nby/x-font-cjk)
  (if nby/x-font-latin
      (set-face-attribute 'default nil :font nby/x-font-latin))

  ;; refer to http://baohaojun.github.io/perfect-emacs-chinese-font.html
  (setq face-font-rescale-alist `((nby/x-font-cjk . ,nby/chinese-font-scale)))

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
  :ensure t
  :init (setq sml/no-confirm-load-theme t
              sml/theme 'respectful)
  :config (sml/setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; nlinum-mode: display line number
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nlinum
  :ensure t
  :custom-face
  (nlinum-current-line ((t (:foreground "orange"))))
  :config
  (setq nlinum-format "  %4d "
        nlinum-highlight-current-line t)
                                        ;nlinum-relative-redisplay-delay .2
                                        ;nlinum-relative-offset 0)
  (add-hook 'prog-mode-hook #'nlinum-mode))

                                        ; Fix disappearing line numbers in nlinum
(use-package nlinum-hl
  :after nlinum
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; popwin: popwin is a popup window manager for Emacs which makes you free
;; from the hell of annoying buffers such like *Help*, *Completions*,
;; *compilation*, and etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package popwin
  :ensure t
  :config (popwin-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Override default buffer-name unique rules with more concise way
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;;; ends here
