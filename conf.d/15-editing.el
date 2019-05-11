;;; editing --- editing behaviors
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; basic behavior
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
 backup-by-copying                 t
 tab-width                         8               ;; default tab width
 fill-column                      79               ;; default column width
 mouse-yank-at-point               t               ;; dont insert at mouse point
 kill-ring-max                   512               ;; size of killing ring
 delete-old-versions               t
 kept-new-versions                10
 kept-old-versions                 5
 version-control                   t               ;; use versioned backups
 enable-recursive-minibuffers      t
 create-lockfiles                  nil             ;; dont use file lock
 tramp-default-method              "rsync"         ;; tramp default method
 abbrev-file-name    (concat user-conf-dir "db/abbrev.el")
 recentf-save-file   (concat user-conf-dir "db/recentf.el")
 nsm-settings-file   (concat user-conf-dir "db/network-security.el")
 ac-comphist-file    (concat user-conf-dir "db/ac-comphist.el")
 frame-title-format  "emacs@%b")     ;; display buffer name at title bar

;; move mouse pointer away while cursor is near
(mouse-avoidance-mode 'animate)

;; anwser y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)


;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)  ;; narrow-mode

(use-package window-numbering
  :ensure
  :config (window-numbering-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; set temporary directory
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((temporary-directory
       (concat (file-name-as-directory temporary-file-directory)
               (format "emacs-%d" (user-uid)))))
  (message (concat "[config] backup directory is " temporary-directory))
  (unless (file-exists-p temporary-directory)
    (make-directory temporary-directory))
  (setq
   auto-save-directory-fallback "/tmp/"
   auto-save-hash-p nil
   auto-save-timeout 100
   auto-save-interval 300
   auto-save-list-file-prefix temporary-file-directory
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   temporary-file-directory temporary-directory
   backup-directory-alist `(("." . ,temporary-directory))))

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
(global-set-key (kbd "C-S-k")          'kill-current-buffer)

(use-package boxquote
  :ensure
  :bind (("C-c M-b" . boxquote-region)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multiple Cursors Mode: Edit multiple line at once
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure
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
  :ensure
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand-region: enlarge your current selection
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :ensure
  :bind (("C--" . er/contract-region)
         ("C-=" . er/expand-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; avy: jump to character
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure
  :bind (("C-;" . avy-goto-char-timer)
         ("C-'" . avy-goto-char))
  :init (avy-setup-default))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; enable symbol overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package symbol-overlay
  :ensure
  :bind (("M-i". symbol-overlay-put)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YASnippet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure yasnippet
  :pin melpa-stable
  :diminish yas/minor-mode
  :config
  (add-to-list 'yas/root-directory (concat user-conf-dir "snippets"))
  (yas/reload-all)
  (yas/global-mode 1))

(use-package yasnippet-snippets
  :ensure
  :pin melpa-stable
  :requires yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Others
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pangu-spacing
  :ensure
  :hook
  ((org-mode . real-pangu-spacing-mode)
   (markdown-mode . real-pangu-spacing-mode))
  :init
  (defun real-pangu-spacing-mode ()
    (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)
    (pangu-spacing-mode t)))

(use-package buffer-move
  :ensure
  :commands (buf-move-up buf-move-down buf-move-left buf-moveright))

;;; ends here
