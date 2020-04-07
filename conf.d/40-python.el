;;; Python --- python configuration
;;; Commentary:
;;; Code:

(defvar nby/python-indentation-size 4
  "Number of spaces for indentation in 'python-mode'.")

(use-package virtualenvwrapper
  :ensure
  :init
  (defun nby/venv-projectile-auto-workon ()
    (interactive)
    (let ((filename (concat (file-name-as-directory (projectile-project-root)) ".venv")))
      (when (file-exists-p filename)
          (let ((venv (with-temp-buffer
                         (insert-file-contents filename)
                         (string-trim (buffer-string)))))
            (message "[venv] activate virtualenv %s" venv)
            (venv-workon venv)))))
  (setq projectile-switch-project-action
        '(lambda ()
           (nby/venv-projectile-auto-workon)
           (projectile-find-file))))

(use-package pylookup
  :ensure
  :disabled
  :custom
  (pylookup-program (concat user-conf-dir ".python-environments/default/bin/pylookup.py"))
  (pylookup-db-file (conat user-conf-dir "db/pylookup.db")))

(use-package python-mode
  :after (projectile virtualenvwrapper highlight-indent-guides)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :ensure
  :init
  (setq
   flycheck-flake8rc (expand-file-name "~/.config/flake8")
   lsp-clients-python-command '("pyls" "-v" "--log-file" "/tmp/pyls.log")
   lsp-pyls-configuration-sources ["flake8"]
   lsp-pyls-plugins-pycodestyle-enabled t
   lsp-pyls-plugins-pyflakes-enabled t
   lsp-pyls-plugins-pylint-enabled nil)
  (defun nby/python-mode-init ()
    (nby/venv-projectile-auto-workon)
    (highlight-indent-guides-mode)
    (lsp))
  :hook (python-mode . nby/python-mode-init)
  :bind (:map python-mode-map
              ("<tab>" . nby/dwim-tab))
  :custom (tab-width nby/python-indentation-size)
  (py-indent-offset nby/python-indentation-size))

(use-package pyimpsort
  :ensure
  :after python-mode)

(provide '40-python)
;;; 40-python ends here
