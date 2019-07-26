;;; Python --- python configuration
;;; Commentary:
;;; Code:

(defvar nby/python-indentation-size 4
  "Number of spaces for indentation in 'python-mode'.")

(use-package pylookup
  :ensure
  :disabled
  :custom
  (pylookup-program (concat user-conf-dir ".python-environments/default/bin/pylookup.py"))
  (pylookup-db-file (conat user-conf-dir "db/pylookup.db")))

(use-package pyvenv
  :ensure)

(use-package python-mode
  :after (projectile pyvenv)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :ensure
  :init
  (setq-default
   flycheck-flake8rc (expand-file-name "~/.config/flake8")
   lsp-clients-python-command '("pyls" "-v" "--log-file" "/tmp/pyls.log")
   lsp-pyls-configuration-sources ["flake8"]
   lsp-pyls-plugins-pycodestyle-enabled t
   lsp-pyls-plugins-pyflakes-enabled t
   lsp-pyls-plugins-pylint-enabled nil)
  (defun pyvenv-autoload ()
    "Automatically activates pyvenv version if .venv file exists."
    (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
      (message "searching %s %s" pdir pfile)
      (if (file-exists-p pfile)
          (pyvenv-workon (with-temp-buffer
                           (insert-file-contents pfile)
                           (message "activating virtualenv %s" (string-trim (buffer-string)))
                           (nth 0 (split-string (buffer-string))))))))
  :hook ((python-mode . lsp))
  :bind (:map python-mode-map
              ("<tab>" . nby/dwim-tab))
  :custom (tab-width nby/python-indentation-size)
  (py-indent-offset nby/python-indentation-size))

;; Emacs IPython Notebook
(use-package ein-loaddefs :after ein)
(use-package ein-notebook :after ein)
(use-package ein-subpackages :after ein)

(use-package ein
  :ensure
  :bind (:map ein:notebook-mode-map
              ("C-c C-x C-c" . comment-region)
              ("C-c C-x C-d" . uncomment-region))
  :init
  (setq-default ein:use-auto-complete t)
  :config
  (unbind-key "C-c C-x" ein:notebook-mode-map))

(use-package pyimpsort
  :ensure
  :after python-mode)

(provide '40-python)
;;; 40-python ends here
