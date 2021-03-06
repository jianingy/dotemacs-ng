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

(use-package css-mode
  :ensure
  :config
  (setq css-indent-offset 2))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.wxml\\'" . web-mode)
         ("\\.wxss\\'" . css-mode)
         ("\\.wpy\\'" . web-mode))
  :after (highlight-indent-guides)
  :hook (web-mode . highlight-indent-guides-mode)
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-block-padding 0
        web-mode-css-indent-offset 2
        web-mode-enable-css-colorization t
        web-mode-code-indent-offset 2)
  :ensure)

(use-package rjsx-mode
  :ensure
  :after (highlight-indent-guides)
  :mode ("\\.js\\'" . rjsx-mode)
  :config
  (defun nby/setup-rjsx-flycheck-eslint ()
    ;; install eslint with nvm's npm
    ;; (setq flycheck-javascript-eslint-executable
    ;;       (concat (projectile-project-root) "/node_modules/.bin/eslint"))
    (with-eval-after-load "lsp-ui-flycheck"
      (flycheck-add-next-checker 'lsp-ui '(error . javascript-eslint))))
  :hook ((rjsx-mode . eglot)
         (rjsx-mode . highlight-indent-guides-mode)
         (rjsx-mode . nby/setup-rjsx-flycheck-eslint))
  :init (setq js-indent-level 2
              js2-basic-offset 2
              js2r-prefered-quote-type 2
              sgml-basic-offset 2
              js2-strict-missing-semi-warning t
              js2-missing-semi-one-line-override t))


(use-package js2-mode
  :disabled
  :ensure
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . eglot-ensure)
  :init (setq js-indent-level 2
              js2-strict-missing-semi-warning nil
              js2-missing-semi-one-line-override nil))

(use-package vue-mode
  :disabled
  :after (highlight-indent-guides)
  :mode ("\\.vue\\'" . vue-mode)
  :bind (("C-c C-r" . vue-mode-reparse))
  :hook ((vue-mode . eglot-ensure)
         (vue-mode . highlight-indent-guides-mode))
  :ensure
  :init (setq js-indent-level 2
              js2-strict-missing-semi-warning nil
              js2-missing-semi-one-line-override nil)
  :config
  (setq mmm-submode-decoration-level 0))

(use-package lsp-vue
  :disabled
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
