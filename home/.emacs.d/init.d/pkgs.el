(req-package smex)

(req-package ace-jump-mode
  :bind ("ESC SPC" . ace-jump-mode))

(req-package elpy
  :require python-mode
  :config (enable-elpy))

(req-package python-mode
  :mode "\\.py\\'" "\\.pythonrc\\'"
  :config (elpy-mode))

(req-package arduino-mode
  :mode "\\.sketch\\'"
  :defer t)

(req-package html-mode
  :mode "\\.html\\'" "\\.htm\\'" "\\.erb\\'"
  :defer t)

(req-package scss-mode
  :mode "\\.scss\\'"
  :defer t)

(req-package coffee-mode
  :mode "\\.coffee\\'"
  :defer t)

(req-package php-mode
  :mode "\\.php\\'" "\\.module\\'" "\\.css.php\\'" "\\.inc\\'"
  :interpreter "php")

;; (req-package jedi
;;   :mode ("\\.py\\'" . python-mode) ( "\\.pythonrc\\'" . python-mode)
;;   :interpreter ("python" . python-mode)
;;   :init (setq jedi:complete-on-dot t)
;;   :config
;;   ((add-hook 'python-mode-hook 'jedi:setup)
;;    (add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))))

(req-package rubocop
  :mode "\\.rb\\'" "Rakefile"
  :interpreter "ruby"
  :config (add-hook 'ruby-mode-hook 'rubocop-mode))

(req-package markdown-mode
  :mode "\\.text\\'" "\\.md\\'" "\\.markdown\\'")

(req-package json-mode
  :require json-snatcher json-reformat
  :mode "\\.json$")

(req-package go-errcheck
  :require go-mode
  :init
  (add-hook 'after-change-major-mode-hook (lambda() (if (equal major-mode 'go-mode)
                                                        (add-hook 'before-save-hook 'gofmt-before-save)
                                                      (remove-hook 'before-save-hook 'gofmt-before-save)))))

(req-package c++-mode
  :mode ("\\.h\\'" . c++-mode) ("\\.cpp\\'" . c++-mode)
  :config
  ((c-add-style
    "my-stroustrup"
    '("stroustrup"
      (indent-tabs-mode . nil)              ; use spaces not tabs
      (c-basic-offset . 4)                  ; indent by four spaces
      (c-offsets-alist . ((inline-open . 0) ; custom indentation rules
                          (brace-list-open . 0)
                          (statement-case-open . +)))))
   (defun my-c++-mode-hook ()
     (c-set-style "my-stroustrup")
     (auto-fill-mode)
     (c-toggle-auto-hungry-state 1))
   (add-hook 'c++-mode-hook 'my-c++-mode-hook)
   (add-hook 'c-mode-hook 'my-c++-mode-hook)))

(req-package flycheck)
(req-package helm)
(req-package helm-ls-git)
(req-package helm-fuzzy-find)
(req-package helm-flycheck)
(req-package helm-gtags)
(req-package helm-config
  :commands (helm-M-x helm-find-files revert-all-buffers revert-buffer-no-confirm
                      helm-gtags-find-tag helm-flycheck flycheck-add-mode)
  :require helm-ls-git helm-fuzzy-find helm-flycheck helm-gtags helm flycheck
  :bind (("M-a" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x r" . revert-buffer-no-confirm)
         ("C-x C-r" . revert-all-buffers)
         ("C-x g" . helm-gtags-find-tag)
         (:map flycheck-mode-map ("C-M-f" . helm-flycheck)))
  :init
  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        flycheck-disabled-checkers '(javascript-jshint)
        helm-ff-file-name-history-use-recentf t)
  (setq-default helm-autoresize-max-height 30
                helm-mode-fuzzy-match t
                helm-completion-in-region-fuzzy-match t)
  :config
  ((flycheck-add-mode 'javascript-eslint 'web-mode)
   (helm-mode-1)
   (helm-autoresize-mode 1)))


;; Column-marker - highlight column at max line length
(req-package column-marker
  :commands column-marker-1
  :init
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              ;; Usually limit is 80 characters
              (unless (equal major-mode 'fundamental-mode) (column-marker-1 81))
              ;; Python is actually 79
              (if (or (equal major-mode 'python-mode)
                      (equal major-mode 'jedi-mode)) (column-marker-1 80))
              ;; And the fatties are 100
              (if (or (equal major-mode 'java-mode)
                      (equal major-mode 'json-mode)
                      (equal major-mode 'js-mode)
                      (equal major-mode 'js2-mode)
                      (equal major-mode 'solidity-mode)
                      (equal major-mode 'dart-mode)
                      (equal major-mode 'html-mode)) (column-marker-1 101)))))

(req-package highlight-chars
  :commands (hc-highlight-tabs hc-highlight-trailing-whitespace)
  :init
  (add-hook 'font-lock-mode-hook #'hc-highlight-tabs)
  (add-hook 'font-lock-mode-hook #'hc-highlight-trailing-whitespace)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(req-package-force stripe-buffer
  :commands (turn-on-stripe-buffer-mode stripe-listify-buffer)
  :init
  ;;(add-hook 'font-lock-mode-hook #'turn-on-stripe-buffer-mode)
  (add-hook 'dired-mode-hook #'stripe-listify-buffer))

(req-package windmove)
(req-package buffer-move)
(req-package revive)
(req-package dash)
(req-package web-mode)

;; Highlight uncommited git changes: https://github.com/dgutov/diff-hl
(req-package diff-hl-margin
  :require diff-hl
  :init
  (add-hook 'after-change-major-mode-hook #'diff-hl-margin-mode)
  :config (global-diff-hl-mode))
