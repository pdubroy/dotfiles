; -*- mode: Emacs-lisp; -*-

;; This file can be symlinked as ~/.emacs, or if .emacs already exists and
;; contains site-specific configuration, just include this file like so:
;;     (load "/path/to/this/file")

(let ((tempdir (concat "/tmp/" (user-login-name) "/")))

  ;; Put autosave files (ie #foo#) and backup files (ie foo~) under /tmp.
  (setq auto-save-file-name-transforms
	`((".*" ,(concat tempdir "emacs_autosaves/\\1") t)))
  (setq  backup-directory-alist
	 `((".*" . ,(concat tempdir "emacs_backups/"))))

  ;; Ensure that the autosave directory exists.
  (make-directory (concat tempdir "emacs_autosaves/") t))

(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 2)

(server-start)
