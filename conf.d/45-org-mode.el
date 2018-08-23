(use-package org
  :ensure t
  :init (setq
          org-todo-keywords
          '((sequence "TODO(t)" "WORKING" "SUSPENDED" "|"
                      "DONE(d)" "ABANDONED(a)")
            (sequence "CHECK(c)" "|" "VERIFIED(v)"))
          org-todo-keyword-faces
          '(("CHECK" :foreground "orange")
            ("WORKING" :foreground "white" :background "firebrick")
            ("SUSPEND" :foreground "firebrick")
            ("VERIFIED" :foreground "lime green" :weight normal)))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))


(use-package org-clock
  :after org
  :init (setq
          org-clock-in-switch-to-state "WORKING"
          org-clock-out-switch-to-state "SUSPENDED"))
