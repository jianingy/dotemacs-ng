;;; coding --- basic coding
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil          ;; dont use tabe in most cases
              compilation-scroll-output t)
(electric-indent-mode t)
;; delete trailing whitespace, hope it's safe
(add-hook 'before-save-hook #'(lambda () (delete-trailing-whitespace)))

;; emacs-flymake
(use-package flymake
  :ensure
  :init
  :config
  (setq flymake-run-in-place nil
        flymake-number-of-errors-to-display 4))

;; syntax check
(use-package flycheck
  :disabled
  :ensure
;  :pin melpa-stable
  :commands (flycheck-add-mode)
  :bind (("C-c n" . next-error))
  :config (global-flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :disabled
  :ensure
  :init (setq flycheck-pos-tip-max-width 100
              flycheck-pos-tip-timeout 30)
  :config (eval-after-load 'flycheck
            '(setq flycheck-display-errors-function
                   #'flycheck-pos-tip-error-messages)))

(use-package column-enforce-mode
  :ensure
  :diminish column-enforce-mode
  :init (setq column-enforce-column 100
              column-enforce-comments nil)
  :config (global-column-enforce-mode t))

(use-package string-inflection
  :ensure
  :bind (("C-c q" . string-inflection-toggle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version Control
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure
  :bind (("C-c g" . magit-status)))

(use-package fringe-helper
  :ensure)

(use-package git-gutter+
  :ensure
  :diminish git-gutter+-mode
  :init (setq git-gutter+-modified-sign "  " ;; two space
              git-gutter+-added-sign "++"    ;; multiple character is OK
              git-gutter+-deleted-sign "--")
  :config (global-git-gutter+-mode t))

(use-package git-gutter-fringe+
  :ensure
  :if window-system)

(use-package projectile
  :ensure
  :diminish projectile-mode
  :after ivy
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-enable-caching t
        projectile-known-projects-file (concat user-emacs-directory
                                               "db/projectile-bookmarks.eld")
        projectile-cache-file (concat user-emacs-directory
                                      "db/projectile-cache.el")
        projectile-completion-system 'ivy
        projectile-file-exists-remote-cache-expire nil)
  :config
  (add-to-list 'projectile-globally-ignored-directories "build")
  :config (projectile-mode))

(use-package counsel-projectile
  :ensure
  :after projectile
  :init
  (counsel-projectile-mode))

(use-package helm-projectile
  :ensure
  :after (projectile helm)
  :config (helm-projectile-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company Mode: auto complete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure
  :bind (:map company-active-map
              ([tab] . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-minimum-width 36)
  (company-tooltip-maximum-width 78)
  :hook (prog-mode . company-mode))

(use-package company-box
  :ensure
  :diminish
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq
   company-box-icons-alist 'company-box-icons-icons-in-terminal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lsp-mode: language server protocol
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure
  :after yasnippet
  :init
  (setq lsp-auto-guess-root nil
        lsp-prefer-flymake nil)
  :config
  (require 'lsp-clients))

(use-package lsp-ui
  :ensure
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil))

(use-package company-lsp
  :ensure
  :after (lsp-mode lsp-ui company)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom (company-lsp-cache-candidates 'auto)
  :init (push 'company-lsp company-backends))

(use-package lsp-treemacs
  :after (lsp treemacs)
  :ensure
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Programming Tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :ensure
  :hook
  (lisp-mode rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Language specific
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package nginx-mode
  :ensure)

(use-package company-nginx
  :ensure
  :after (company nginx-mode)
  :config
  (eval-after-load 'nginx-mode
    '(add-hook 'nginx-mode-hook #'company-nginx-keywords)))

(use-package plantuml-mode
  :ensure
  :mode (("\\.puml\\'" . plantuml-mode))
  :init
  (setq plantuml-output-type "png"))

(use-package json-mode
  :ensure
  :mode (("\\.json\\'" . json-mode)))

(use-package dumb-jump
  :ensure
  :bind
  (:map prog-mode-map
        (("M-[" . dumb-jump-go)
         ("M-]" . dumb-jump-go-prompt)))
  :custom (dumb-jump-selector 'helm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; popup eshell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pop-eshell-mode
  :load-path "site-lisp/pop-eshell-mode"
  :bind ("C-z" . eshell-pop-toggle)
  :hook ((eshell-mode . (lambda () (setq pcomplete-cycle-completions nil))))
  :config
  (pop-eshell-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; emacs jupyter
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package jupyter
  :disabled
  :ensure)


(use-package fira-code-mode
  :load-path "site-lisp/fira-code-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; highlight indentation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-indent-guides
  :ensure
  :demand
  :init
  (setq highlight-indent-guides-method 'character)
  :bind (("C-c i" . highlight-indent-guides-mode)))


(use-package eglot
  :disabled
  :ensure)

(use-package eldoc-box
  :ensure
  :after (eglot)
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; scheme
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser
  :ensure
  :bind (("C-c C-c" . geiser-eval-buffer))
  :config
  (setq geiser-active-implementations '(racket)))

;;; ends here
