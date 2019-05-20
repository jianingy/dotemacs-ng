(use-package org
  :ensure
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
   org-startup-indented t
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
  :config
  (require 'org-tempo)
  (require 'ob-python)
  (require 'ob-scala)
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
  :ensure
  :after org
  :init
  (setq org-bullets-bullet-list '("▸" "◆" "●" "○"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ox-s5
  :ensure org-plus-contrib)
