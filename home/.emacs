;;;;---------------------------------------------------------------------------
;; EMACS ROCKS
;;;;---------------------------------------------------------------------------

;; This is the directory where we keep plugins.
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/elisp/cl-lib/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

(when (>= emacs-major-version 24)
  (setq package-list
        '(arduino-mode
          column-marker
          dart-mode
          egg
          go-errcheck
          go-mode
          idomenu
          json-mode
          json-snatcher
          json-reformat
          json-reformat
          json-snatcher
          magit
          git-rebase-mode
          git-commit-mode
          php-mode
          revive
          rubocop
          dash
          web-mode))

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)
  )

(add-hook 'before-save-hook 'gofmt-before-save)

;;;;---------------------------------------------------------------------------
;; SECTION: MODES
;;;;---------------------------------------------------------------------------

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; PHP Mode
(autoload 'php-mode "php-mode" "Major mode for PHP." t)

;; JSON Mode
(autoload 'json-mode "json-mode" "Major mode for JSON." t)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; JS2 Mode
(autoload 'js2-mode "js2-mode" "Alternate major mode for JavaScript." t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; SCSS Mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(require 'dart-mode)
;; Dart Mode
(autoload 'dart-mode "dart-mode")
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;; Add Rubocop to ruby-mode
(add-hook 'ruby-mode-hook 'rubocop-mode)

;; Arduino Mode
(autoload 'dart-mode "arduino-mode")
(add-to-list 'auto-mode-alist '("\\.sketch\\'" . arduino-mode))

;; CoffeeScript Mode
(require 'cl-lib)
(autoload 'coffee-mode "coffee-mode" "Major mode for CoffeeScript." t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(setq-default auto-mode-alist
  (append '(("\.css.php$" . css-mode)
            ("\.php$" . php-mode)
            ("\.module$" . php-mode)
            ("\.inc$" . php-mode)
            (".pythonrc" . python-mode)
            ("Rakefile" . ruby-mode))
          auto-mode-alist))


;;;;---------------------------------------------------------------------------
;; SECTION: Preferences
;;;;---------------------------------------------------------------------------

(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)

;; Follow symlinks
(setq-default vc-follow-symlinks t)

;; Turn on syntax hightlighting.
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show me dem line-numbers
(setq linum-format "%d ")
(add-hook 'after-change-major-mode-hook 'linum-mode)

;; Set magit colors.
(setq magit-auto-revert-mode nil)
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "black")
     (set-face-foreground 'magit-diff-del "red3")
     (unless window-system
              (set-face-background 'magit-item-highlight "white"))))

;; Reload file from disk - without a verbose yes/no confirm
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

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

;; Reload from file
(global-set-key (kbd "C-x r") 'revert-buffer-no-confirm)

(global-set-key (kbd "ESC <up>")  'scroll-down)
(global-set-key (kbd "ESC <down>")  'scroll-up)
(global-set-key (kbd "<C-tab>") 'indent-region)

;; line-mode for editing, char-mode for terminal
(global-set-key (kbd "C-x x") 'toggle-term-mode)
;; (define-key term-mode-map (kbd "M-x") 'nil)

;; Resizing windows
(global-set-key (kbd "ESC k")             'enlarge-window-three)
(global-set-key (kbd "ESC j")              'shrink-window-three)
(global-set-key (kbd "ESC h")  'shrink-window-horizontally-five)
(global-set-key (kbd "ESC l") 'enlarge-window-horizontally-five)
;; (global-set-key (kbd "ESC k")    'windmove-up)
;; (global-set-key (kbd "ESC j")  'windmove-down)
;; (global-set-key (kbd "ESC h")  'windmove-left)
;; (global-set-key (kbd "ESC l") 'windmove-right)

;; Copy to system clipboard
(global-set-key (kbd "ESC c") 'copy-to-sys-clipboard)

;; Launch magit - should probably do something with egg instead.
(global-set-key (kbd "C-x g") 'magit-status)

;;

;;;;---------------------------------------------------------------------------
;; SECTION: Plugins
;;;;---------------------------------------------------------------------------

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

;; Windmove - for switching between frames -- easier than C-x o.
(require 'windmove)
(global-set-key (kbd "ESC 8")    'windmove-up)
(global-set-key (kbd "ESC 5")  'windmove-down)
(global-set-key (kbd "ESC 4")  'windmove-left)
(global-set-key (kbd "ESC 6") 'windmove-right)

;; Buffer-move - for swapping buffers between frames. Numpad is great for this.
(require 'buffer-move)
(global-set-key (kbd "C-x <kp-8>")    'buf-move-up)
(global-set-key (kbd "C-x <kp-5>")  'buf-move-down)
(global-set-key (kbd "C-x <kp-4>")  'buf-move-left)
(global-set-key (kbd "C-x <kp-6>") 'buf-move-right)

(global-set-key (kbd "C-x w") 'delete-trailing-whitespace)

;; Unbind M-numpad-4 and M-numpad-6, cause I use those for nav
;; (global-set-key (kbd "M-s-4") 'windmove-left)

;; DoReMi - Incrementally perform action with arrow keys
;; (require 'doremi)
;; TODO (doremi)

;; IDO - interactive do, basically auto-completion for switching buffers and finding files. Replaces main C-x f and C-x b.
(require 'ido)
(ido-mode t)

;; Stripes - sets the background color of every even line. In this case, it's set to #141414 -- change in stripes.el
(require 'stripes)
;;(add-hook 'after-change-major-mode-hook 'turn-on-stripes-mode)

;; Column-marker - let's highlight column 80 so we know where to trim lines. Love me dat PEP
(require 'column-marker)
(add-hook 'after-change-major-mode-hook (lambda () (interactive) (column-marker-2 80)))
(add-hook 'after-change-major-mode-hook 'column-number-mode)

;; Highlight-chars - Customizable regex highlighting.
(require 'highlight-chars)
;; Highlight tabs - we almost always want spaces. (exception: Go-mode)
(unless (equal major-mode 'go-mode)
  (add-hook 'after-change-major-mode-hook 'hc-highlight-tabs))
;; Highlight trailing whitespace.
(unless (equal major-mode 'term-mode)
  (add-hook 'after-change-major-mode-hook 'hc-highlight-trailing-whitespace))
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(egg-confirm-next-action t)
 '(egg-enable-tooltip t)
 '(split-height-threshold nil)
 '(split-width-threshold 0)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
