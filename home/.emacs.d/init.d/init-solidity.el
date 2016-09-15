(require 'req-package)

;; solidity-mode

(defun solidity-align-modifiers (langelem)
  (let ((pos (cdr langelem)))
    (if (string= (buffer-substring-no-properties pos (+ pos 8))
            "function")
        0
      c-basic-offset)))

(req-package solidity-mode
  :require flymake-solidity
  :mode "\\.sol\\'"
  :config
  (setq solidity-solc-path "/usr/local/bin/solc")
  :init
  (add-hook
   'after-change-major-mode-hook
   (lambda()
     (if (equal major-mode 'solidity-mode)
         (progn (setq c-basic-offset 4)
                (c-set-offset 'statement-cont
                              'solidity-align-modifiers)))))

  :bind ((:map solidity-mode-map ("M-j" . nil))
         (:map solidity-mode-map ("M-a" . nil))
         (:map solidity-mode-map ("C-M-j" . nil))))

(provide 'init-solidity)
