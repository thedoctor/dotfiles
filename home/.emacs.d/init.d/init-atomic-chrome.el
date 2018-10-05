(require 'req-package)

;; ace-jump mode

(req-package atomic-chrome
  :commands (atomic-chrome-start-server
             atomic-chrome-stop-server))

(provide 'init-atomic-chrome)
