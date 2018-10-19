;;; workaround --- some workaround and fixes
;;; Commentary:
;;; Code:

(define-coding-system-alias 'UTF-8 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Linux specific UI settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'gnu/linux)
  ;; double bold font looks ugly on linux, so disable bold for all fonts.
  (mapc #'(lambda (x) (set-face-attribute x nil :weight 'normal)) (face-list)))

(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile"))))
;;; ends here
