;;; web --- web developement
;;; Commentary:
;;; Code:

(use-package tern
  :ensure t
  :after js2-mode
  :hook ((js2-mode . tern-mode))
  :init (setq js-indent-level 2
              js2-strict-missing-semi-warning nil
              js2-missing-semi-one-line-override nil)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :ensure t)

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :ensure t)

(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode)
  :ensure t)

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

;;; ends here
