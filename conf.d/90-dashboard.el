;;; finish --- final works
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dashboard: a welcome page from spacemacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :ensure t
  :init (setq dashboard-banner-logo-title
              (format "Emacs starts in %.2f seconds. "
                      (- (float-time) nby/startup-timestamp))
              dashboard-items '((recents  . 8)
                                (projects . 8)))
  :config (dashboard-setup-startup-hook))

;;; ends here
