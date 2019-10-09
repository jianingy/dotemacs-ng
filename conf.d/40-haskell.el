(use-package lsp-haskell
  :ensure t)

(use-package haskell-mode
  :ensure t
  :requires lsp-haskell
  :hook ((haskell-mode . lsp)
         (haskell-mode . haskell-mode-startup))
  :mode ("\\.hs\\'")
  :init
  (defconst haskell-mode-symbols-alist
    '(("<=" . ?≤)
      (">=" . ?≥)
      ("/=" . ?≠)
      ("\\" . ?λ)
      ("->" . ?→)
      ("<-" . ?←)
      ("=>" . ?⟹)
      ("<=>" . ?⟺)))
  (defun haskell-mode-startup ()
    (setq prettify-symbols-alist haskell-mode-symbols-alist)
    (prettify-symbols-mode))
  :config
  (setq lsp-prefer-flymake nil))
