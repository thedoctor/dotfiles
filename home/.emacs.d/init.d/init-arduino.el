(require 'req-package)

;; arduino mode

(req-package arduino-mode
  :mode "\\.sketch\\'"
  :defer t)

(provide 'init-arduino)
