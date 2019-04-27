
;;; rust --- rust developement
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure
  :hook (rust-mode . lsp)
  :mode ("\\.rs\\'" . rust-mode))

(use-package rust-playground :ensure)

;;; ends here
