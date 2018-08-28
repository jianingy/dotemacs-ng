;;; coding --- basic coding
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil) ;; dont use tabe in most cases
(custom-set-variables '(compilation-scroll-output t))
(electric-indent-mode t)
;; delete trailing whitespace, hope it's safe
(add-hook 'before-save-hook #'(lambda () (delete-trailing-whitespace)))

;; syntax check
(use-package flycheck
  :ensure t
  :pin melpa-stable
  :commands (flycheck-add-mode)
  :bind (("C-c n" . next-error))
  :config (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :ensure t
  :init (setq flycheck-pos-tip-max-width 80
              flycheck-pos-tip-timeout 30)
  :config (eval-after-load 'flycheck
            '(setq flycheck-display-errors-function
                   #'flycheck-pos-tip-error-messages)))

(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :init (setq column-enfore-column 80)
  :config (global-column-enforce-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version Control
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package fringe-helper
  :ensure t)

(use-package git-gutter+
  :ensure t
  :diminish git-gutter+-mode
  :init (setq git-gutter+-modified-sign "  " ;; two space
              git-gutter+-added-sign "++"    ;; multiple character is OK
              git-gutter+-deleted-sign "--")
  :config (global-git-gutter+-mode t))

(use-package git-gutter-fringe+
  :ensure t
  :if window-system)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :custom
  (projectile-known-projects-file (concat user-conf-dir "db/projectile-bookmarks.eld"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode))

;; cannot use requires here???
(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company Mode: auto complete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :bind (:map company-active-map
              ([tab] . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config (global-company-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lsp-mode: language server protocol
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :after (lsp-mode lsp-ui company)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (push 'company-lsp company-backends))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Programming Tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :ensure t
  :hook
  (lisp-mode rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Language specific
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))


(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'"))


(use-package nginx-mode
  :ensure t)

(use-package company-nginx
  :ensure t
  :requires nginx-mode
  :config
  (eval-after-load 'nginx-mode
    '(add-hook 'nginx-mode-hook #'company-nginx-keywords)))

(use-package plantuml-mode
  :ensure t
  :mode (("\\.puml\\'" . plantuml-mode))
  :init
  (setq plantuml-output-type "png"))


;;; ends here
