(use-package scala-mode
  :ensure
  :interpreter
  ("scala" . scala-mode))


(use-package lsp-scala
  :ensure
  :after (lsp scala-mode))
