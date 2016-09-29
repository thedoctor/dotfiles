(require 'req-package)

;; ruby mode for rakefile

(req-package ruby-mode
  :mode "\\.rb\\'" )

;; (req-package rubocop
;;   :mode "\\.rb\\'" "Rakefile"
;;   :interpreter "ruby"
;;   :config (add-hook 'ruby-mode-hook 'rubocop-mode))

(req-package rake :commands rake)

(provide 'init-ruby)
