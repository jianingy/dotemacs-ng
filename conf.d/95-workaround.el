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

;;; 下面一段是 zwz 的, 作者声明只适应 org-mode 8.0 以及以上版本
(defun clear-single-linebreak-in-cjk-string (string)
  "clear single line-break between cjk characters that is usually soft line-breaks"
  (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
         (start (string-match regexp string)))
    (while start
      (setq string (replace-match "\\1\\2" nil nil string)
            start (string-match regexp string start))))
  string)

(defun ox-html-clear-single-linebreak-for-cjk (string backend info)
  (when (org-export-derived-backend-p backend 'html)
    (clear-single-linebreak-in-cjk-string string)))

(add-to-list 'org-export-filter-final-output-functions
             'ox-html-clear-single-linebreak-for-cjk)
;;; ends here
