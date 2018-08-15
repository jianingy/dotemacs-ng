(require 'cl)

(defun nby/find-local-config (c)
  "Find first exist directory in list C."
  (car (remove-if-not'file-exists-p
        (mapcar (lambda (x) (concat user-home-dir "/" x)) c))))

(defun nby/safe-load (filename)
  "Load FILENAME and return nil if error occurred."
  (condition-case nil
      (load filename)
    (error (progn (message "File %s cannot be loaded" filename) nil))))

(defvar nby/startup-timestamp (float-time))
(defvar user-home-dir (getenv "HOME"))
(defvar user-conf-dir user-emacs-directory)
(defvar user-info-file
  (nby/find-local-config '(".userinfo.el" "_userinfo.el")))
(defvar user-local-file
  (nby/find-local-config '(".userlocal.el" "_userlocal.el")))

(defun nby/find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      ;; use "/sudo::" to prevent asking ssh passphrase
      (setq file (concat "/sudo::" file)))
    (find-file file)))

(defun nby/insert-separator ()
  "Insert a line seperator."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (open-line 1)
    (let ((cur (point)))
      (insert "--8<-----------------------separator------------------>8---")
      (comment-region cur (point)))))

(defface nby/font-lock-leading-whitespace-face
  '((t (:underline (:color "deep sky blue" :style wave))))
  "Face for leading whitespaces"
  :group 'whitespace-detection-faces)

(defun nby/whitespace-detection-mode (mode &key tab)
  "MODE which detect improper whitespaces and long lines.
If TAB is not nil, hightlight tab characters"
  (when tab
    (font-lock-add-keywords
     mode
     '(("\t+" (0 'nby/font-lock-leading-whitespace-face t)))))
  (font-lock-add-keywords
   mode
   '(("^\s+$" (0 'nby/font-lock-leading-whitespace-face t))
     ("\s+$" (0 'nby/font-lock-leading-whitespace-face t))
     ("^.\\{79\\}\\(.+\\)$" (1 'nby/font-lock-trailing-whitespace-face t)))))

(defmacro nby/local-set-variables (&rest pairs)
  "Make variable buffer-local and set according to variable/value PAIRS."
  `(progn
     ,@(mapcar #'(lambda (x) `(set (make-local-variable (quote ,(car (cadr x)))) ,(cadr (cadr x)))) pairs)))

(provide 'nby)
