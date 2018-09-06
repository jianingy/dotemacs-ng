(use-package org
  :ensure t
  :init
  (defun nby/org-agenda-breadcrumb ()
    (concat
     "["
     (mapconcat 'identity
                (mapcar (lambda (x) (format "%s" x))
                        (org-get-outline-path))
                " ≫ ")
     "]"))
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "WORKING" "SUSPENDED" "|"
               "DONE(d)" "ABANDONED(a)")
     (sequence "CHECK(c)" "|" "VERIFIED(v)"))
   org-agenda-prefix-format
   '((agenda . " %(nby/org-agenda-breadcrumb) %?-12t% s ")
     (todo . "%(nby/org-agenda-breadcrumb) %i")
     (tags . " %i %-12:c")
     (search . " %i %-12:c"))
   org-priority-faces
   '((65 :foreground "white" :background "red")
     (66 :foreground "orange")
     (67 . "royal blue"))
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
  :init
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-confirm-babel-evaluate nil
        org-clock-in-switch-to-state "WORKING"
        org-clock-out-switch-to-state "SUSPENDED")
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (plantuml . t))))


(use-package org-bullets
  :ensure t
  :after org
  :init
  (setq org-bullets-bullet-list '("▸" "◆" "●" "○"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
