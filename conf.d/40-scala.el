(use-package scala-mode
  :ensure
  :bind (:map scala-mode-map
              ("C-c C-c" . run-scala))
  :interpreter
  ("scala" . scala-mode))


(use-package lsp-scala
  :ensure
  :after (lsp scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
