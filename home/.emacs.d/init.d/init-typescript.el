(require 'req-package)

;; typescript mode

(req-package typescript-mode
  :require flycheck-typescript-tslint
  :mode "\\.ts\\'" "\\.tsx\\'"
  :config
  (setq typescript-indent-level 2)
  :init
  (add-hook-exec 'typescript-mode 'flycheck-typescript-tslint-setup))

(provide 'init-typescript)
