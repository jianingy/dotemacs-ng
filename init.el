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
    (monokai-theme yasnippet-bundle yaml-mode window-numbering web-mode vue-mode use-package telephone-line symbol-overlay sublime-themes smart-mode-line rainbow-mode rainbow-delimiters python-mode popwin pangu-spacing nlinum-hl multiple-cursors markdown-mode+ magit lsp-ui lsp-rust lsp-python load-dir js2-mode jedi helm-projectile helm-bm haskell-mode git-gutter-fringe+ flycheck-rust flycheck-pyflakes flycheck-pycheckers flycheck-pos-tip flycheck-color-mode-line expand-region edit-server dimmer diminish dashboard company-tern company-lsp company-jedi column-enforce-mode color-theme boxquote avy auto-virtualenvwrapper anaconda-mode)))
 '(sql-postgres-options (quote ("-P" "pager=off" "-w"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
