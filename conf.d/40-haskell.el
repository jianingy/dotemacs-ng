;; emacs-flymake


(use-package lsp-haskell
  :ensure
  :hook
  (haskell-mode . lsp)
  (haskell-mode . lsp-ui-mode))


(use-package nix-haskell-mode
  :ensure
  :disabled
  :hook
  (haskell-mode . nix-haskell-mode))

(use-package nix-sandbox
  :ensure)

(use-package dante
  :ensure
  :disabled
  :after (haskell-mode)
  :commands dante-mode
  :hook
  (haskell-mode . flycheck-mode)
  (haskell-mode . dante-mode)
  :config
  (add-hook 'dante-mode-hook
            '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                   '(warning . haskell-hlint)))))

(use-package haskell-mode
  :ensure t
  :bind (("C-c C-c" . haskell-compile)
         ("C-c C-x C-c" . comment-region)
         ("C-c C-x C-d" . uncomment-region))
  :hook ( ;; (haskell-interactive-mode . centaur-tabs-local-mode)
         (haskell-mode . haskell-mode-startup))
  :mode ("\\.hs\\'")
  :init
  (setq ;haskell-compile-cabal-build-command "stack run"
                                        ; haskell-process-type 'cabal-new-repl
        haskell-mode-stylish-haskell-path "stylish-haskell"
        haskell-stylish-on-save t)
  ;; (add-to-list 'eglot-server-programs
  ;;              '(haskell-mode . ("hie-wrapper" "--lsp" "--json" "-l" "/tmp/hie.log")))
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
