(require 'package)

;; bootstrap utils

(defconst my-custom-file "~/.emacs.d/custom.el")

(setq package-enable-at-startup t)
(setq auto-save-default nil)
(setq make-backup-files t)
(put 'erase-buffer 'disabled nil)

;; turn off startup screen

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; load extensions

(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/init-real.el")))
