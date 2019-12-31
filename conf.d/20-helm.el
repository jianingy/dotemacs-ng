;;; helm --- all helm configurations
;;; Commentary:
;;; Code:

(use-package helm
  :ensure
  :disabled
  :diminish helm-mode
  :init  (setq helm-split-window-in-side-p       t ; open helm buffer inside current
                                                   ; window, not occupy whole other window
               helm-buffers-fuzzy-matching       t ; fuzzy matching buffer names when non--nil
               helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                                   ; when reaching top or bottom of source.
               helm-ff-search-library-in-sexp    t ; search for library in `require' and `declare-function' sexp.
               helm-scroll-amount                8 ; scroll 8 lines other window using M-<next>/M-<prior>
               helm-autoresize-max-height 30
               helm-autoresize-min-height 30
               helm-ff-file-name-history-use-recentf t)
  :bind (([remap find-file] . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-o" . helm-occur)
         ("C-h SPC" . helm-all-mark-rings)
         :map helm-map
         ([tab] . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z
         ("C-x C-x" . helm-toggle-visible-mark))
  :config (helm-mode 1))

(use-package helm-bm
  :ensure
  :disabled
  :after helm
  :bind (("C-c b" . helm-bm)))


;;; ends here
