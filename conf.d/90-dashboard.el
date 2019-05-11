;;; finish --- final works
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dashboard: a welcome page from spacemacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dashboard
  :ensure
  :init (setq dashboard-banner-logo-title
              (format "Emacs starts in %.2f seconds. "
                      (- (float-time) nby/startup-timestamp))
              initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
              dashboard-center-content t
              show-week-agenda-p t
              dashboard-startup-banner
              (expand-file-name "~/.emacs.d/media/logo.png")
              dashboard-items '((agenda  . 5)
                                (projects . 5)
                                (bookmarks . 5)
                                (recents . 8)))
  :config (dashboard-setup-startup-hook))

;;; ends here
