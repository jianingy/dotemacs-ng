;;; Python --- python configuration
;;; Commentary:
;;; Code:

(defvar nby/python-indentation-size 4
  "Number of spaces for indentation in 'python-mode'.")

(use-package pylookup
  :ensure t
  :disabled
  :custom
  (pylookup-program (concat user-conf-dir ".python-environments/default/bin/pylookup.py"))
  (pylookup-db-file (conat user-conf-dir "db/pylookup.db")))


(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :ensure t
  :custom (tab-width nby/python-indentation-size)
  (py-indent-offset nby/python-indentation-size))

(use-package flycheck-pyflakes
  :requires flycheck
  :ensure t
  :hook (python-mode . (lambda () (flycheck-select-checker 'python-flake8)))
  :config (flycheck-add-mode 'python-flake8 'python-mode))

(use-package virtualenvwrapper
  :requires python-mode
  :ensure t)

(use-package auto-virtualenvwrapper
  :ensure t
  :requires python-mode
  :hook (python-mode . auto-virtualenvwrapper-activate))

(use-package flycheck-pycheckers
  :disabled
  :ensure t
  :after (flycheck python-mode)
  :hook (flycheck-mode . flycheck-pycheckers-setup))

(use-package jedi-core
  :disabled
  :ensure t
  :after python-mode
  :init  (setq jedi:use-shortcuts t
               jedi:complete-on-dot nil))

(use-package company-jedi
  :disabled
  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package lsp-python
  :ensure t
  :after lsp-mode
  :config
  (lsp-define-stdio-client lsp-python "python"
                           #'projectile-project-root
                           '("pyls"))
  (defun lsp-set-cfg ()
    (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
      ;; TODO: check lsp--cur-workspace here to decide per server / project
      (lsp--set-configuration lsp-cfg)))

  (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)
  :init (setq lsp-ui-flycheck-enable nil)
  :hook (python-mode . lsp-python-enable))

(use-package anaconda-mode
  :disabled
  :ensure t
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

;; Emacs IPython Notebook
(use-package ein-loaddefs :after ein)
(use-package ein-notebook :after ein)
(use-package ein-subpackages :after ein)

(use-package ein
  :ensure t
  :bind (:map ein:notebook-mode-map

              ("C-c C-x C-c" . comment-region)
              ("C-c C-x C-d" . uncomment-region))
  :init
  (setq ein:use-auto-complete t)
  :config
  (unbind-key "C-c C-x" ein:notebook-mode-map))


(provide '40-python)
;;; 40-python ends here
