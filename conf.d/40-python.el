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
  :ensure t)

(use-package virtualenvwrapper
  :requires python-mode
  :ensure t)

(use-package auto-virtualenvwrapper
  :ensure t
  :requires python-mode
  :hook (python-mode . auto-virtualenvwrapper-activate))

(use-package flycheck-pycheckers
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-pycheckers-setup)
  :init (add-hook 'python-mode-hook
                  (lambda () (nby/local-set-variables
                              '(flycheck-checker     'python-flake8))))
  :config (flycheck-add-mode 'python-flake8 'python-mode))


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
  :hook (python-mode . lsp-python-enable))

(use-package anaconda-mode
  :disabled
  :ensure t
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(provide '45-python)
;;; 45-python ends here
