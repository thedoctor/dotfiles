(require 'req-package)

;; jedi mode for python

(req-package jedi
  :mode ("\\.py\\'" . python-mode) ( "\\.pythonrc\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  ((setq jedi:complete-on-dot t)
   (setq jedi:environment-root "jedi")  ; or any other name you like
   (setq jedi:environment-virtualenv
         (append python-environment-virtualenv
                 '("--python" "/home/matt/.pyenv/shims/python"))))
  :config
  ((add-hook 'python-mode-hook 'jedi:setup)
   (add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))))

(provide 'init-jedi)
