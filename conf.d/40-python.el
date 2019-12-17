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
  (defun py-workaround ()
    (flycheck-select-checker 'python-flake8))
  (defun pyvenv-autoload ()
    "Automatically activates pyvenv version if .venv file exists."
    (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
      (message "searching %s %s" pdir pfile)
      (if (file-exists-p pfile)
          (pyvenv-workon (with-temp-buffer
                           (insert-file-contents pfile)
                           (message "activating virtualenv %s" (string-trim (buffer-string)))
                           (nth 0 (split-string (buffer-string))))))))
  :hook ((python-mode . lsp)
         (python-mode . highlight-indent-guides-mode)
         (python-mode . py-workaround))
  :bind (:map python-mode-map
              ("<tab>" . nby/dwim-tab))
  :custom (tab-width nby/python-indentation-size)
  (py-indent-offset nby/python-indentation-size))

(use-package pyimpsort
  :ensure
  :after python-mode)

(provide '40-python)
;;; 40-python ends here
