;;; ematts-config --- Matt's emacs configuration
;;; Commentary:
;;;   flycheck told me to put this here lol

;;; Code:
;; Disable defadvice redefinition warnings
;; (should reevaluate if this is safe every few months)
;; (setq ad-redefinition-action 'accept)

;; Package sources
(add-to-list 'load-path "~/.emacs.d/elisp/")
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (add-to-list 'load-path "~/.emacs.d/elisp/cl-lib/")
;; (add-to-list 'load-path "~/.emacs.d/elisp/req-package/")
;; (add-to-list 'load-path "~/.emacs.d/elpa/")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;; (el-get 'sync)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(require 'req-package)

(req-package elpy
  :mode "\\.py\\'"  "\\.pythonrc\\'"
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

;; (req-package flycheck)
;; (req-package helm)
;; (req-package helm-ls-git)
;; (req-package helm-fuzzy-find)
;; (req-package helm-flycheck)
;; (req-package helm-gtags)
;; (req-package helm-config
;;   :commands (helm-M-x helm-find-files revert-all-buffers revert-buffer-no-confirm
;;                       helm-gtags-find-tag helm-flycheck flycheck-add-mode)
;;   :require helm-ls-git helm-fuzzy-find helm-flycheck helm-gtags helm flycheck
;;   :bind (("M-a" . helm-M-x)
;;          ("C-x C-f" . helm-find-files)
;;          ("C-x r" . revert-buffer-no-confirm)
;;          ("C-x C-r" . revert-all-buffers)
;;          ("C-x g" . helm-gtags-find-tag)
;;          (:map flycheck-mode-map ("C-M-f" . helm-flycheck)))
;;   :init
;;   (setq helm-split-window-in-side-p t
;;         helm-move-to-line-cycle-in-source t
;;         helm-ff-search-library-in-sexp t
;;         helm-scroll-amount 8
;;         flycheck-disabled-checkers '(javascript-jshint)
;;         helm-ff-file-name-history-use-recentf t)
;;   (setq-default helm-autoresize-max-height 30
;;                 helm-mode-fuzzy-match t
;;                 helm-completion-in-region-fuzzy-match t)
;;   :config
;;   ((flycheck-add-mode 'javascript-eslint 'web-mode)
;;    (helm-mode-1)
;;    (helm-autoresize-mode 1)))

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
                      (equal major-mode 'typescript-mode)
                      (equal major-mode 'js-mode)
                      (equal major-mode 'js2-mode)
                      (equal major-mode 'solidity-mode)
                      (equal major-mode 'dart-mode)
                      (equal major-mode 'html-mode)) (column-marker-1 101)))))

;; (add-hook 'python-mode-hook
;;           '(lambda () (setq python-indent-offset 4)))

(req-package-force highlight-chars
  :commands (hc-highlight-tabs hc-highlight-trailing-whitespace)
  :init
  (add-hook 'font-lock-mode-hook #'hc-highlight-tabs)
  (add-hook 'font-lock-mode-hook #'hc-highlight-trailing-whitespace)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

;; (req-package-force stripe-buffer
;;   :commands (turn-on-stripe-buffer-mode stripe-listify-buffer)
;;   :init
;;   ;;(add-hook 'font-lock-mode-hook #'turn-on-stripe-buffer-mode)
;;   (add-hook 'dired-mode-hook #'stripe-listify-buffer))

(req-package windmove)
(req-package buffer-move)
(req-package revive)
(req-package dash)
(req-package web-mode)

;; ;; Highlight uncommited git changes: https://github.com/dgutov/diff-hl
;; (req-package diff-hl-margin
;;   :require diff-hl
;;   :init
;;   (add-hook 'after-change-major-mode-hook #'diff-hl-margin-mode)
;;   :config (global-diff-hl-mode))

;; (req-package-finish)

;;;;---------------------------------------------------------------------------
;; PACKAGES
;;;;---------------------------------------------------------------------------

;; (define-minor-mode respectful-mode
;;   "I use this to tell emacs not to care that other people do shitty things like use tabs or leave trailing whitespace. -_-"
;;   :init-value nil
;;   " respectful"
;;   nil
;;   nil
;;   :global 1
;;   (funcall (lambda()
;;              (if (equal hc-highlight-trailing-whitespace-p 't)
;;                  (progn
;;                    (remove-hook 'before-save-hook 'delete-trailing-whitespace)
;;                    (hc-toggle-highlight-trailing-whitespace 0)
;;                    (hc-toggle-highlight-tabs 0)
;;                    (message "respectful-mode enabled"))
;;                (progn
;;                  (add-hook 'before-save-hook 'delete-trailing-whitespace)
;;                  (hc-toggle-highlight-trailing-whitespace 't)
;;                  (hc-toggle-highlight-tabs 't)
;;                  (message "respectful-mode disabled; fuck yeah")))
;;              )))

;; Run these every time we change modes.
;; (add-hook
;;  'after-change-major-mode-hook
;;  (lambda ()
;;    ;; Highlight tabs - we almost always want spaces. (exception: Go-mode)
;;    (unless (or (equal major-mode 'go-mode)
;;                (equal major-mode 'fundamental-mode)
;;                (equal major-mode 'term-mode))
;;      (progn (hc-toggle-highlight-trailing-whitespace 't)
;;             (hc-toggle-highlight-tabs 't)))
;;    (if (or (equal major-mode 'fundamental-mode)
;;            (equal major-mode 'term-mode))
;;        ;; Don't because we're not editing code, or we must permit evil.
;;        (remove-hook 'before-save-hook 'delete-trailing-whitespace)
;;      ;; Delete trailing whitespace on save.
;;      (add-hook 'before-save-hook 'delete-trailing-whitespace))
;;    ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elpy zygospore ztree zoom-frm yoshi-theme yaml-mode xmlgen wgrep-helm web-mode visual-regexp-steroids use-package-chords undercover unbound typescript-mode typed-clojure-mode twittering-mode symon swoop suscolors-theme sunrise-x-loop sudo-ext sublime-themes stripe-buffer string-edit sotclojure sos soothe-theme solidity-mode smex smartrep smartparens smart-shift smart-mode-line slamhound shift-number shell-pop scss-mode scratch-persist scratch-ext savekill ruby-mode rubocop rotate reykjavik-theme revive req-package rectangle-utils rake rainbow-delimiters python-environment psvn prodigy plan9-theme pkgbuild-mode php-mode peek-mode paradox overseer org-trello org-dashboard org-cliplink org-bullets nyan-mode neotree narrow-indirect multifiles move-text mmm-mode mbo70s-theme markdown-mode malabar-mode makefile-runner magit-svn magit-gitflow magit-gh-pulls lua-mode load-dir litable list-processes+ kotlin-mode kibit-helper keyfreq json-mode js2-mode jazz-theme jabber igrep ido-at-point idle-highlight-mode httprepl howdoi hl-defined hindent highlight-numbers highlight-chars helm-themes helm-systemd helm-swoop helm-proc helm-package helm-org-rifle helm-open-github helm-make helm-ls-git helm-helm-commands helm-gtags helm-google helm-gitignore helm-github-stars helm-fuzzy-find helm-flycheck helm-descbinds helm-company helm-circe helm-cider hc-zenburn-theme haskell-snippets guide-key gruvbox-theme gotham-theme google-translate google-this go-mode go-errcheck glsl-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-blame gist flymake-solidity flycheck-protobuf flycheck-pos-tip flycheck-clojure flx-ido fireplace firebelly-theme find-temp-file fic-mode expand-region ert-modeline ert-expectations epc ensime emr emmet-mode elm-yasnippets elm-mode elisp-slime-nav elfeed el-mock edit-server duplicate-thing dummy-h-mode dracula-theme dockerfile-mode docker django-mode dired-rainbow dired-open diff-hl define-word debbugs darktooth-theme danneskjold-theme cyberpunk-theme css-mode company-restclient company-quickhelp company-irony company-ghc column-marker coffee-mode clojure-snippets clojure-mode-extra-font-locking clojure-cheatsheet cljsbuild-mode cljr-helm cider-profile camcorder buffer-move beacon batch-mode bash-completion aurel atomic-chrome arduino-mode anzu ant align-cljlet ace-window ace-link ace-jump-mode ace-jump-helm-line ace-jump-buffer 4clojure)))
 '(python-guess-indent nil)
 '(python-indent 4)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

;;;;---------------------------------------------------------------------------
;; Section: Preferences
;;;;---------------------------------------------------------------------------

(defun split-horizontally-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (and (one-window-p t)
             (not (active-minibuffer-window)))
    (split-window-horizontally)))
(add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

(setq-default indent-tabs-mode nil)

(setq-default column-number-mode t)

(custom-set-variables
 '(python-guess-indent nil)
 '(python-indent 4))

;; ace-window set window selection keys to home row instead of numbers
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Follow symlinks
(setq-default vc-follow-symlinks t)

;; Turn on syntax hightlighting.
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs

;; Show me dem line-numbers
(setq linum-format "%d ")
(add-hook 'after-change-major-mode-hook 'linum-mode)

;; Reload file from disk - without a verbose yes/no confirm
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun enlarge-window-three ()
  "Grow current window vertically in increments of 3 rows"
  (interactive)
  (setq current-prefix-arg '(3))
  (call-interactively 'enlarge-window))
(defun enlarge-window-horizontally-five ()
  "Grow current window horizontally in increments of 5 columns"
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'enlarge-window-horizontally))

(defun shrink-window-three ()
  "Shrink current window vertically in increments of 3 rows"
  (interactive)
  (setq current-prefix-arg '(3))
  (call-interactively 'shrink-window))
(defun shrink-window-horizontally-five ()
  "Shrink current window horizontally in increments of five columns"
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'shrink-window-horizontally))

(defun toggle-term-mode ()
  "Toggle setting tab widths between 4 and 8"
  (interactive)
  (call-interactively (if (term-in-char-mode) 'term-line-mode 'term-char-mode)))

(defun copy-to-sys-clipboard (&optional b e)
  "Copy region to system clipboard"
  (interactive "r")
  (shell-command-on-region b e "pbcopy"))

;;;;---------------------------------------------------------------------------
;; SECTION: Key Bindings
;;;;---------------------------------------------------------------------------

;; respectful mode
;; (global-set-key (kbd "C-x e") 'respectful-mode)

;; TODO Figure out indent region.
;; (global-set-key (kbd "<C-tab>") 'indent-region)

;; line-mode for editing, char-mode for terminal
(global-set-key (kbd "C-x x") 'toggle-term-mode)

;; Copy to system clipboard
(global-set-key (kbd "ESC c") 'copy-to-sys-clipboard)

;; Replace string (in region or rest of file)
(global-set-key (kbd "ESC s") 'replace-string)

;; Revive - lets you maintain your open buffers and frame configuration.
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
;; C-x 5 save
(define-key ctl-x-map "5" 'save-current-configuration)
;; C-x 1 load
(define-key ctl-x-map "1" 'resume)
;; C-x . forget
(define-key ctl-x-map "." 'wipe)

(global-set-key (kbd "C-x w") 'delete-trailing-whitespace)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-w") 'helm-imenu)
;; (global-set-key (kbd "C-x C-d") 'helm-browse-project)
;; (global-set-key (kbd "M-m") 'helm-do-grep)

;; QWERTY (ergodox)
;; Resizing windows
(global-unset-key (kbd "C-M-i"))
(add-hook 'help-mode-hook
          (lambda()
            (local-unset-key (kbd "C-M-i"))))
(add-hook 'after-change-major-mode-hook
          (lambda()
            (local-unset-key (kbd "C-M-i"))))
(global-set-key (kbd "C-M-i")             'enlarge-window-three)
(global-set-key (kbd "C-M-k")              'shrink-window-three)
(global-set-key (kbd "C-M-j")  'shrink-window-horizontally-five)
(global-set-key (kbd "C-M-l") 'enlarge-window-horizontally-five)

;; Windmove - for switching between frames -- easier than C-x o.
(global-set-key (kbd "M-i")    'windmove-up)
(global-set-key (kbd "M-k")  'windmove-down)
(global-set-key (kbd "M-j")  'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)

;; Buffer-move - for swapping buffers between frames.
(global-set-key (kbd "C-x i")    'buf-move-up)
(global-set-key (kbd "C-x k")  'buf-move-down)
(global-set-key (kbd "C-x j")  'buf-move-left)
(global-set-key (kbd "C-x l") 'buf-move-right)

;; QWERTY
;; Resizing windows
;; (global-set-key (kbd "ESC i")             'enlarge-window-three)
;; (global-set-key (kbd "ESC k")              'shrink-window-three)
;; (global-set-key (kbd "ESC j")  'shrink-window-horizontally-five)
;; (global-set-key (kbd "ESC l") 'enlarge-window-horizontally-five)

;; Windmove - for switching between frames -- easier than C-x o.
(global-set-key (kbd "ESC 8")    'windmove-up)
(global-set-key (kbd "ESC 5")  'windmove-down)
(global-set-key (kbd "ESC 4")  'windmove-left)
(global-set-key (kbd "ESC 6") 'windmove-right)

;; Buffer-move - for swapping buffers between frames.
(global-set-key (kbd "C-x <kp-8>")    'buf-move-up)
(global-set-key (kbd "C-x <kp-5>")  'buf-move-down)
(global-set-key (kbd "C-x <kp-4>")  'buf-move-left)
(global-set-key (kbd "C-x <kp-6>") 'buf-move-right)

;; COLEMAK (ergodox)
;; Resizing windows
;; (global-set-key (kbd "ESC y")             'enlarge-window-three)
;; (global-set-key (kbd "ESC u")              'shrink-window-three)
;; (global-set-key (kbd "ESC l")  'shrink-window-horizontally-five)
;; (global-set-key (kbd "ESC ;") 'enlarge-window-horizontally-five)

;; ;; Windmove - for switching between frames -- easier than C-x o.
;; (global-set-key (kbd "ESC i")    'windmove-up)
;; (global-set-key (kbd "ESC e")  'windmove-down)
;; (global-set-key (kbd "ESC n")  'windmove-left)
;; (global-set-key (kbd "ESC o") 'windmove-right)

;; ;; Buffer-move - for swapping buffers between frames.
;; (global-set-key (kbd "C-x i")    'buf-move-up)
;; (global-set-key (kbd "C-x e")  'buf-move-down)
;; (global-set-key (kbd "C-x n")  'buf-move-left)
;; (global-set-key (kbd "C-x o") 'buf-move-right)

;; disable bell
(setq ring-bell-function 'ignore)

(provide 'custom)
;;; custom.el ends here
