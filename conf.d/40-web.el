;;; web --- web developement
;;; Commentary:
;;; Code:

(use-package tern
  :disabled
  :ensure
  :after js2-mode
  :hook ((js2-mode . tern-mode))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package company-tern
  :disabled
  :ensure
  :hook ((js2-mode . company-mode)
         (js-mode . company-mode))
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 4
        web-mode-style-padding 1
        web-mode-script-padding 1
        web-mode-block-padding 0
        web-mode-css-indent-offset 4
        web-mode-enable-css-colorization t
        web-mode-code-indent-offset 4)
  :ensure)

(use-package rjsx-mode
  :ensure
  :after (lsp-mode)
  :mode ("\\.js\\'" . rjsx-mode)
  :config
  (defun nby/setup-rjsx-flycheck-eslint ()
    (setq flycheck-javascript-eslint-executable
          (concat (projectile-project-root) "/node_modules/.bin/eslint"))
    (with-eval-after-load "lsp-ui-flycheck"
      (flycheck-add-next-checker 'lsp-ui '(error . javascript-eslint))))
  :hook ((rjsx-mode . lsp)
         (rjsx-mode . nby/setup-rjsx-flycheck-eslint))
  :init (setq js-indent-level 4
              js2r-prefered-quote-type 4
              sgml-basic-offset 4
              js2-strict-missing-semi-warning t
              js2-missing-semi-one-line-override t))


(use-package js2-mode
  :disabled
  :ensure
  :after (lsp-mode)
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . lsp)
  :init (setq js-indent-level 4
              js2-strict-missing-semi-warning nil
              js2-missing-semi-one-line-override nil))

(use-package vue-mode
  :disabled
  :mode ("\\.vue\\'" . vue-mode)
  :ensure
  :init (setq js-indent-level 4
              js2-strict-missing-semi-warning nil
              js2-missing-semi-one-line-override nil)
  :config
  (setq mmm-submode-decoration-level 0))

(use-package lsp-vue
  :after (lsp-mode)
  :hook ((vue-mode . lsp-vue-mmm-enable))
  :ensure)

(use-package xref-js2
  :after (js2-mode)
  :ensure)

(use-package js2-refactor
  :after (js2-mode)
  :ensure)

;;; ends here
