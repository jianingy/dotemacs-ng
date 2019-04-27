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
 '(package-selected-packages
   (quote
    (color-theme-modern color-theme yasnippet-snippets yaml-mode xref-js2 window-numbering web-mode use-package telephone-line tangotango-theme symbol-overlay string-inflection rust-playground rjsx-mode rainbow-delimiters pyvenv python-mode popwin platformio-mode plantuml-mode pangu-spacing ov org-bullets nlinum-hl nginx-mode monokai-theme markdown-mode+ magit lv lsp-vue lsp-ui lsp-rust lsp-javascript-typescript load-dir json-mode js2-refactor ivy helm-projectile helm-bm haskell-mode git-gutter-fringe+ ghub frame-local flycheck-pos-tip flycheck-irony flycheck-color-mode-line expand-region ein edit-server dimmer dashboard company-nginx company-lsp company-irony column-enforce-mode buffer-move boxquote avy)))
 '(sql-postgres-options (quote ("-P" "pager=off" "-w"))))
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
