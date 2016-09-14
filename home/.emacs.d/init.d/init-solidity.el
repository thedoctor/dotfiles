(require 'req-package)

;; solidity-mode

(req-package solidity-mode
  :require flymake-solidity
  :mode "\\.sol\\'"
  :init
  (setq solidity-solc-path "/usr/local/bin/solc")
  (add-hook 'after-change-major-mode-hook
            (lambda() (if (equal major-mode 'solidity-mode)
                          (progn (setq c-basic-offset 4)
                                 (c-set-offset (quote statement-cont) 0 nil)))))
  :bind ((:map solidity-mode-map ("M-j" . nil))
         (:map solidity-mode-map ("M-a" . nil))
         (:map solidity-mode-map ("C-M-j" . nil))))

(provide 'init-solidity)
