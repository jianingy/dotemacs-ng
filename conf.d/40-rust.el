;;; rust --- rust developement
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))


(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp-rust-enable))

;;; ends here
