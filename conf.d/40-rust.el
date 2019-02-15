
;;; rust --- rust developement
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure
  :mode ("\\.rs\\'" . rust-mode))

(use-package lsp-rust
  :ensure
  :after lsp-mode
  :hook (rust-mode . nby/setup-rust-mode)
  :init
  (defun nby/setup-rust-mode ()
    (column-enforce-n 100)
    (lsp)))

(use-package rust-playground :ensure)

;;; ends here
