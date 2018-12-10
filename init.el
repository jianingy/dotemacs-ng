;;; ---
;;; Commentary:
;;; Code:

;; try enable debug mode
(when (getenv "EMACS_DEBUG")
  (setq debug-on-error t))

;; install use-package
(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;; load my utilities: fix me
(load (concat user-emacs-directory "site-lisp/nby/nby.el"))

;; load user settings
(when user-info-file
  (nby/safe-load user-info-file))

;; load configurations
(use-package load-dir
             :ensure t
             :init
             (setq load-dirs
                   (concat (file-name-as-directory user-conf-dir) "conf.d")))

;; load local configuration
(when user-local-file
  (nby/safe-load user-local-file))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(current-language-environment "UTF-8")
 '(custom-file
   (nby/find-local-config
    (quote
     (".usercustom.el" "_usercustom.el"))))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(left-fringe-width 11 t)
 '(line-spacing 2)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (dashboard org-bullets js2-refactor xref-js2 lsp-javascript-typescript lsp-vue web-mode rust-playground lsp-rust flycheck-rust rust-mode ein pyvenv flycheck-pyflakes python-mode markdown-mode+ json-mode plantuml-mode company-nginx nginx-mode haskell-mode yaml-mode rainbow-delimiters company-lsp lsp-ui lsp-mode company helm-projectile projectile ivy git-gutter-fringe+ git-gutter+ fringe-helper magit string-inflection column-enforce-mode flycheck-pos-tip flycheck-color-mode-line flycheck helm-bm helm buffer-move pangu-spacing yasnippet-snippets yasnippet symbol-overlay avy expand-region edit-server multiple-cursors boxquote window-numbering frame-local ov telephone-line dimmer popwin nlinum-hl nlinum tangotango-theme color-theme monokai-theme load-dir use-package)))
 '(right-fringe-width 11 t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-down-aggressively 0.01)
 '(scroll-margin 3)
 '(scroll-step 1)
 '(scroll-up-aggressively 0.01)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#333"))))
 '(company-scrollbar-fg ((t (:background "deep sky blue"))))
 '(company-tooltip ((t (:inherit default :foreground "white smoke" :background "#333"))))
 '(company-tooltip-annotation ((t (:foreground "white smoke"))))
 '(company-tooltip-annotation-selection ((t (:foreground "black"))))
 '(company-tooltip-common ((t (:foreground "orange"))))
 '(company-tooltip-common-selection ((t (:foreground "black"))))
 '(company-tooltip-selection ((t (:foreground "black" :background "deep sky blue"))))
 '(mode-line ((t (:box (:line-width 2 :color "#49483E" :style nil)))))
 '(nlinum-current-line ((t (:foreground "orange"))))
 '(org-document-title ((t (:height 1.0))))
 '(org-level-1 ((t (:height 1.0))))
 '(org-level-2 ((t (:height 1.0))))
 '(org-level-3 ((t (:height 1.0))))
 '(org-level-4 ((t (:height 1.0))))
 '(vertical-border ((t (:foreground "#333333")))))
