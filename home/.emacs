;;;;---------------------------------------------------------------------------
;; .emacs configuration file
;; author: Matt Smith
;; tested on: GNU Emacs 23.1.1 (x86_64-redhat-linux-gnu, GTK+ Version 2.18.9)
;;
;; packages used:
;;   revive (session management)
;;   buffer-move (shoving around frames)
;;   windmove (switching between frames; buffer-move dependency)
;;   ido (buffer/file selection)
;;   stripes (alternating background colors.
;;
;; about:
;;   This config file solves some of my big problems with emacs, mostly having
;;   to do with frames.
;;
;; last mod: 2012-08-21
;;;;---------------------------------------------------------------------------
;; This is the directory where we keep plugins.
(add-to-list 'load-path "~/.emacs.d/elisp/")

;;;;---------------------------------------------------------------------------
;; SECTION: MODES
;;;;---------------------------------------------------------------------------

;; PHP Mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

;; JS2 Mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq-default auto-mode-alist
  (append '(("\.css.php$" . css-mode)
            ("\.php$" . php-mode)
            ("\.module$" . php-mode)
            ("\.inc$" . php-mode))
          ;;("\.ctp$" . web-mode))
          auto-mode-alist))


;;;;---------------------------------------------------------------------------
;; SECTION: PREFERENCES
;;;;---------------------------------------------------------------------------

;; Indenting style - follow Python conventions.
;; i.e. Never tabs: always replace with 4 spaces.
(setq-default c-basic-indent 2)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Turn on syntax hightlighting.
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show me dem line-numbers
(setq linum-format "%d ")
(add-hook 'after-change-major-mode-hook 'linum-mode)
(global-set-key (kbd "ESC <up>")  scroll-down)
(global-set-key (kbd "ESC <down>") 'scroll-up)

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

;; Resizing windows - not a plugin, but related.
(global-set-key (kbd "ESC i")              'enlarge-window)
(global-set-key (kbd "ESC k")               'shrink-window)
(global-set-key (kbd "ESC j")  'shrink-window-horizontally)
(global-set-key (kbd "ESC l") 'enlarge-window-horizontally)

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
;; Highlight tabs - we almost always want spaces.
(add-hook 'after-change-major-mode-hook 'hc-highlight-tabs)
;; Highlight trailing whitespace.
(add-hook 'after-change-major-mode-hook 'hc-highlight-trailing-whitespace)
