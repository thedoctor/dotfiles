(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (hl-defined narrow-indirect zoom-frm dummy-h-mode zygospore ztree yoshi-theme yaml-mode xmlgen wotd wgrep visual-regexp-steroids use-package-el-get use-package-chords unbound twittering-mode toml-mode swoop sudo-ext sublime-themes string-edit sos soothe-theme smartrep smartparens smart-shift smart-mode-line slime-company shift-number shell-pop scratch-ext scala-mode sbt-mode savekill rotate restclient req-package rectangle-utils rake rainbow-delimiters racer psvn protobuf-mode prodigy podcaster plan9-theme pkgbuild-mode peek-mode paradox overseer org-dashboard org-cliplink org-bullets nyan-mode nameless multiple-cursors multifiles move-text mastodon makefile-runner magit lua-mode load-dir litable kibit-helper kaolin-themes js2-mode jazz-theme igrep ido-at-point idle-highlight-mode hyperbole httprepl howdoi hindent highlight-numbers helm-themes helm-swoop helm-proc helm-org-rifle helm-ls-git helm-helm-commands helm-google helm-gitignore helm-github-stars helm-descbinds helm-company helm-books haskell-snippets guide-key gruvbox-theme groovy-mode grizzl gotham-theme google-translate google-this glsl-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gist flx-ido fireplace find-temp-file fic-mode expand-region exec-path-from-shell ert-modeline emr emmet-mode elpy elm-mode elisp-slime-nav edit-server duplicate-thing dockerfile-mode docker django-mode dired-rainbow dired-open dired-launch dired-details diff-hl define-word debbugs darktooth-theme company-shell company-quickhelp company-ghc clojure-mode cljsbuild-mode cargo camcorder bool-flip batch-mode bash-completion anzu ant ace-window ace-link ace-jump-helm-line ace-jump-buffer 4clojure))))

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

;; Disable smartparens
;; (setq-default smartparens-global-mode nil)

;; Enable elpy
(elpy-enable)

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

;; disable bell
(setq ring-bell-function 'ignore)

(provide 'custom)
;;; custom.el ends here
