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


(use-package pyvenv
  :after projectile
  :ensure t
  :config
  (defun pyvenv-autoload ()
    (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
      (if (file-exists-p pfile)
          (pyvenv-workon (with-temp-buffer
                           (insert-file-contents pfile)
                           (nth 0 (split-string (buffer-string)))))))))

(use-package lsp-python
  :ensure t
  :after (lsp-mode pyvenv)
  :config
  (lsp-define-stdio-client lsp-python "python"
                           #'projectile-project-root
                           '("pyls"))
  (defun lsp-set-cfg ()
    (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
      ;; TODO: check lsp--cur-workspace here to decide per server / project
      (lsp--set-configuration lsp-cfg)))
  (defun nby/lsp-python-enable ()
    (progn
      (pyvenv-autoload)
      (lsp-python-enable)))
  :hook ((python-mode . nby/lsp-python-enable)
         (lsp-after-initialize-hook . lsp-set-cfg)))



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
