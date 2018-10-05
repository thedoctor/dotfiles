(require 'req-package)

;; md mode

(req-package markdown-mode
  :mode "\\.text\\'" "\\.md\\'" "\\.markdown\\'")

(provide 'init-markdown)
