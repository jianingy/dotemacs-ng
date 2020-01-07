;; emacs-flymake
(use-package flymake-haskell-multi
  :ensure)

(use-package lsp-haskell
  :disabled
  :ensure t)

(use-package haskell-mode
  :ensure t
  :after (highlight-indent-guides)
  :bind ("C-c C-c" . haskell-compile)
  :hook ((haskell-interactive-mode . centaur-tabs-local-mode)
         (haskell-mode . flymake-haskell-multi-load)
         (haskell-mode . eglot-ensure)
         (haskell-mode . haskell-mode-startup))
  :mode ("\\.hs\\'")
  :init
  (setq haskell-compile-cabal-build-command "stack run")
  (defconst haskell-mode-symbols-alist
    '(("<=" . ?≤)
      (">=" . ?≥)
      ("/=" . ?≠)
      ("\\" . ?λ)
      ("->" . ?→)
      ("<-" . ?←)
      ("=>" . ?⇒)
      ("==" . ?≡)))
  (defun haskell-mode-startup ()
    (setq prettify-symbols-alist haskell-mode-symbols-alist)
    (highlight-indent-guides-mode)
    (prettify-symbols-mode)))

(use-package nix-haskell-mode
  :ensure
  :hook (haskell-mode . nix-haskell-mode))
