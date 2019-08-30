(use-package lsp-haskell
  :ensure t)

(use-package haskell-mode
  :ensure t
  :requires lsp-haskell
  :config
  (setq lsp-prefer-flymake nil)
  :hook ((haskell-mode . lsp)))
