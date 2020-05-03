(use-package org
  :ensure
  :custom-face
  (org-document-title ((t (:height 1.3 ))))
  (org-level-1 ((t (:weight normal :height 1.2 ))))
  (org-level-2 ((t (:weight normal :height 1.1))))
  (org-level-3 ((t (:weight normal :height 1.0))))
  (org-level-4 ((t (:weight normal :height 1.0))))
  (org-block ((t (:inherit default))))
  :init
  (defun nby/org-agenda-breadcrumb ()
    (concat
     "["
     (mapconcat 'identity
                (mapcar (lambda (x) (format "%s" x))
                        (org-get-outline-path))
                " » ")
     "]"))
  (setq
   org-startup-indented t
   org-hide-leading-stars t
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
  ;(require 'org-tempo)
  (require 'ob-python)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))
(use-package org-clock
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
  :hook (org-mode . org-bullets-mode))

(use-package ox-s5
  :disabled
  :ensure org-plus-contrib)

(use-package mixed-pitch
  :ensure
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-special-keyword)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-todo)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-date)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-link)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'py-variable-name-face)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'py-number-face)
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . mixed-pitch-mode)))

(use-package writeroom-mode
  :ensure
  :hook
  (org-mode . writeroom-mode)
  :config
  (setq-default writeroom-width 110))
