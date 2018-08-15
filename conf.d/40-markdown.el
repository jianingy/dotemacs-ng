;;; markdown --- markdown tools
;;; Commentary:
;;; Code:

(use-package markdown-mode+
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :ensure t)

(use-package livedown
  :load-path "site-lisp/emacs-livedown"
  :bind ("C-M-m" . livedown-preview)
  :init (setq livedown:autostart nil
              livedown:open t
              livedown:port 1337))

;;; ends here
