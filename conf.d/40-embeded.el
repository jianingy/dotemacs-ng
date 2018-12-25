(use-package platformio-mode
  :ensure
  :hook ((c++-mode . platformio-conditionally-enable)))

(use-package irony
  :ensure
  :after company
  :hook ((flycheck-mode . flycheck-irony-setup)
         (c++-mode . irony-mode))
  :bind (([remap completion-at-point] . irony-completion-at-point-async)
         ([remap complete-symbol] . irony-completion-at-point-async))
  :init
  (add-to-list 'company-backends 'company-irony))
