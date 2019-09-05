(use-package lsp-haskell
  :ensure t)

(use-package haskell-mode
  :ensure t
  :requires lsp-haskell
  :hook ((haskell-mode . lsp))
  :mode ("\\.hs\\'")
  :config
  (setq lsp-prefer-flymake nil))
