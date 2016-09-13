(require 'req-package)

;; lua mode

(req-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :config (setq lua-indent-level 4))

(provide 'init-lua)
