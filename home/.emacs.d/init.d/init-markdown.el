(require 'req-package)

;; md mode

(req-package markdown-mode
  :ensure t :mode "\\.md\\'"
  :bind (("C-<up>" . outline-previous-heading)
         ("C-<down>" . outline-next-heading)))

(provide 'init-markdown)
