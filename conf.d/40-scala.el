(use-package scala-mode
  :ensure
  :bind (:map scala-mode-map
              ("C-c C-c" . run-scala))
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :ensure
  :commands sbt-start sbt-command
  :init
  (defconst scala-mode-symbols-alist
    '(("<=" . ?≤)
      (">=" . ?≥)
      ("->" . ?→)
      ("<-" . ?←)
      ("=>" . ?⇒)
      ("<=>" . ?⇔)))
  (defun setup-scala-mode ()
    (setq prettify-symbols-alist scala-mode-symbols-alist)
    (prettify-symbols-mode))
  :hook (scala-mode . setup-scala-mode)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))


(use-package ensime
  :ensure
  :config
  (setq-default ensime-startup-notification nil))
