
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)

;; bootstrap utils

(defconst my-custom-file "~/.emacs.d/custom.el")

(setq package-enable-at-startup t)
(setq auto-save-default nil)
(setq make-backup-files t)
(put 'erase-buffer 'disabled nil)

;; turn off startup screen

(setq inhibit-splash-screen nil)
(setq inhibit-startup-message nil)

;; load extensions

(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/init-real.el")))
