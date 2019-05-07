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

;; set custom file
(setq-default
 custom-file (nby/find-local-config '(".usercustom.el" "_usercustom.el")))

;; load user settings
(when user-info-file
  (message "[config] loading user info file %s" user-info-file)
  (nby/safe-load user-info-file))

;; load configurations
(use-package load-dir
             :ensure t
             :config
             (setq load-dirs
                   (concat (file-name-as-directory user-conf-dir) "conf.d")))

