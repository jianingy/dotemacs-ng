
;;; rust --- rust developement
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package lsp-rust
  :ensure t
  :after lsp-mode
  :hook (rust-mode . lsp-rust-enable))

;;; ends here
