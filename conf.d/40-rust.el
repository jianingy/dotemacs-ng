
;;; rust --- rust developement
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure
  :mode ("\\.rs\\'" . rust-mode))

(use-package flycheck-rust
  :ensure
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package lsp-rust
  :ensure
  :after lsp-mode
  :hook (rust-mode . lsp-rust-enable))

(use-package rust-playground :ensure)
;;; ends here
