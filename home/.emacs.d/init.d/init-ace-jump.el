(require 'req-package)

;; ;; ace-jump mode

;; (req-package ace-jump-mode
;;   :bind ("ESC SPC" . ace-jump-mode))


(req-package avy-menu
  :ensure t
  :bind ("M-SPC" . avy-goto-char-2))


(provide 'init-ace-jump)
