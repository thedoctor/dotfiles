(require 'req-package)

;; js2 mode

(req-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-include-node-externs t)
  (setq js2-init-hook (define-key js2-mode-map (kbd "M-j") nil)))

(provide 'init-js)
