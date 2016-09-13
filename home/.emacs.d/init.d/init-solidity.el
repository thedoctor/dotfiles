(require 'req-package)

;; solidity-mode

(req-package solidity-mode
  :require flymake-solidity
  :mode "\\.sol\\'"
  :init (setq solidity-solc-path "/usr/local/bin/solc")
  :bind ((:map solidity-mode-map ("M-j" . nil))
         (:map solidity-mode-map ("M-a" . nil))
         (:map solidity-mode-map ("C-M-j" . nil))))

(provide 'init-solidity)
