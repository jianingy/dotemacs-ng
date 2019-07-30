(use-package scala-mode
  :ensure
  :mode "\\.s\\(cala\\|bt\\)$"
  :interpreter ("scala" . scala-mode)
  :hook ((scala-mode . scala-mode-startup)
         (scala-mode . electric-pair-mode)
         (scala-mode . lsp))
  :init
  (defconst scala-mode-symbols-alist
    '(("<=" . ?≤)
      (">=" . ?≥)
      ("->" . ?→)
      ("<-" . ?←)
      ("=>" . ?⇒)
      ("<=>" . ?⇔)))
  (defun scala-mode-startup ()
    (setq prettify-symbols-alist scala-mode-symbols-alist)
    (prettify-symbols-mode))
  :config
  (setq lsp-prefer-flymake nil))


(use-package sbt-mode
  :ensure
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))


;; (use-package ensime
;;   :disabled
;;   :ensure
;;   :config
;;   (setq-default ensime-startup-notification nil))
