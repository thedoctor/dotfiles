(require 'req-package)

;; go-errcheck mode

(req-package go-errcheck
  :require go-mode
  :init
  (add-hook 'after-change-major-mode-hook (lambda() (if (equal major-mode 'go-mode)
                                                        (add-hook 'before-save-hook 'gofmt-before-save)
                                                      (remove-hook 'before-save-hook 'gofmt-before-save)))))

(provide 'init-go-errcheck)
