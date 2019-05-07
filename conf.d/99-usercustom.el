;; load emacs custom configuration
(when custom-file
  (message "[config] loading emacs custom file %s" custom-file)
  (load custom-file))

;; load local configuration
(when user-local-file
  (message "[config] loading user local file %s" user-local-file)
  (nby/safe-load user-local-file))
