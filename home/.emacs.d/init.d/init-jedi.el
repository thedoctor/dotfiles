(require 'req-package)

;; jedi mode for python

(req-package jedi
  :mode ("\\.py\\'" . python-mode) ( "\\.pythonrc\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init (setq jedi:complete-on-dot t)
  :config
  ((add-hook 'python-mode-hook 'jedi:setup)
   (add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))))

(provide 'init-jedi)
