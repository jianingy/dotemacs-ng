;;; autotemplate --- Configuration for file initialization template
;;; Commentary:
;;; Code:

(defadvice auto-insert  (around yasnippet-expand-after-auto-insert activate)
  "Expand auto-inserted content as yasnippet templete."
  (let ((is-new-file (and (not buffer-read-only)
                          (or (eq this-command 'auto-insert)
                              (and auto-insert (bobp) (eobp))))))
    ad-do-it
    (let ((old-point-max (point-max))
          (yas-indent-line nil))
      (when is-new-file
        (goto-char old-point-max)
        (yas/expand-snippet (buffer-substring-no-properties (point-min) (point-max)))
        (delete-region (point-min) old-point-max)))))


(auto-insert-mode)
(setq
 auto-insert-query nil ;; apply template without question
 auto-insert-directory (concat user-conf-dir "template.d"))

(define-auto-insert "\\.org$" "template.org")
(define-auto-insert "\\.ino$" "template.ino")
(define-auto-insert "\\.\\([C]\\|cc\\|cpp\\)$"  "template.cc")
(define-auto-insert "\\.\\([Hh]\\|hh\\|hpp\\)$" "template.h")
(define-auto-insert "\\.go$" "template.go")
(define-auto-insert "\\.tex$" "template.tex")
(define-auto-insert "\\.sh$" "template.sh")
(define-auto-insert "\\.rb$" "template.rb")
(define-auto-insert "\\.rs$" "template.rs")
(define-auto-insert "\\.el$" "template.el")
(define-auto-insert "\\.py$" "template.py")  ;; conflicts with python-mode
(define-auto-insert "\\.pl$" "template.pl")
(define-auto-insert "\\.pm$" "template.pm")
(define-auto-insert "\\.ml$" "template.ml")
(define-auto-insert "\\.lua$" "template.lua")
(define-auto-insert "\\.mq4$" "template.mql")
(define-auto-insert "\\.mq5$" "template.mql")
(define-auto-insert "\\.mql$" "template.mql")

;;; ends here
