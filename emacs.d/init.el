; -*- mode: Emacs-lisp; -*-

;; This file can be symlinked as ~/.emacs.d/init.el, or if init.el (or .emacs)
;; already exists and contains site-specific configuration, just do:
;;     (load "/path/to/this/file")

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(menu-bar-mode -1)

(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 2
              js-indent-level 2)

(set-face-foreground 'minibuffer-prompt "cyan")

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; C/C++ mode customization
(defun my-c-mode-hook ()
  (load "google-c-style.el")
  (google-set-c-style)
  (local-set-key  (kbd "C-c o") 'ff-find-other-file))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; MELPA
;; Setup code based on  https://melpa.org/#/getting-started
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
   (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages (quote (typescript-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; use-package initialization (https://github.com/jwiegley/use-package)
(eval-when-compile
  (require 'use-package))

(use-package typescript-mode
  :ensure t)

(setq column-number-mode t)


(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

;; use the function in conjunction with :eval and format-mode-line in your mode-line-format
(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line "%* %b (%m)")
                        ;; right
                        (format-mode-line "%l:%c  ")))))
