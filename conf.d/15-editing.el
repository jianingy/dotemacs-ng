;;; editing --- editing behaviors
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; basic behaviors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(backup-by-copying t)
 '(tab-width               8)               ;; default tab width
 '(fill-column            79)               ;; default column width
 '(mouse-yank-at-point     t)               ;; dont insert at mouse point
 '(kill-ring-max         512)               ;; size of killing ring
 '(delete-old-versions     t)
 '(kept-new-versions      10)
 '(kept-old-versions       5)
 '(version-control         t)               ;; use versioned backups
 '(enable-recursive-minibuffers t)
 '(recentf-save-file (concat user-conf-dir "db/recentf.el"))
 '(nsm-settings-file (concat user-conf-dir "db/network-security.el"))
 '(ac-comphist-file (concat user-conf-dir "db/ac-comphist.el"))
 '(frame-title-format  "emacs@%b"))     ;; display buffer name at title bar

;; move mouse pointer away while cursor is near
(mouse-avoidance-mode 'animate)

;; anwser y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)


;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)  ;; narrow-mode

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; set temporary directory
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((temporary-directory
       (concat (file-name-as-directory temporary-file-directory) "emacs")))
  (message (concat "[config] backup directory is " temporary-directory))
  (unless (file-exists-p temporary-directory)
    (make-directory temporary-directory))
  (custom-set-variables
   '(backup-directory-alist `(("." . ,temporary-directory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing shortcuts bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-g")            'goto-line)
(global-set-key (kbd "C-c f")          'grep-find)
(global-set-key (kbd "C-c SPC")        'set-mark-command)
(global-set-key (kbd "C-c C-x C-c")    'comment-region)
(global-set-key (kbd "C-c C-x C-d")    'uncomment-region)
(global-set-key (kbd "C-x F")          'nby/find-file-as-root)
(global-set-key (kbd "M-p")            'backward-sexp)
(global-set-key (kbd "M-n")            'forward-sexp)

(use-package boxquote
  :ensure t
  :bind (("C-c M-b" . boxquote-region)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multiple Cursors Mode: Edit multiple line at once
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Quick buffer switch: C-x C-x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-x\C-x" #'(lambda () (interactive)
                               (switch-to-buffer (other-buffer))))
(use-package bs
  :config (global-set-key "\C-x\C-b" 'bs-show))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing Server
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package edit-server
  :if window-system
  :ensure t
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand-region: enlarge your current selection
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :ensure t
  :bind (("C--" . er/contract-region)
         ("C-=" . er/expand-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; avy: jump to character
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)
         ("C-'" . avy-goto-char))
  :init (avy-setup-default))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; enable symbol overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package symbol-overlay
  :ensure t
  :bind (("M-i". symbol-overlay-put)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YASnippet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure yasnippet-bundle
  :diminish yas/minor-mode
  :config
  (yas/global-mode 1))

;;; ends here
