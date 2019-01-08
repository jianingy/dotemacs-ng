(use-package platformio-mode
  :ensure
  :hook ((c++-mode . platformio-conditionally-enable)))

(use-package irony
  :ensure
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :bind (([remap completion-at-point] . irony-completion-at-point-async)
         ([remap complete-symbol] . irony-completion-at-point-async)))

(use-package company-irony
  :ensure
  :after (irony company)
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :ensure
  :after irony
  :config
  (flycheck-irony-setup))
