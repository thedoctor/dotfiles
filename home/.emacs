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
          go-errcheck
          go-mode
          helm
          helm-ls-git
          ace-jump-mode
          jedi
          json-mode
          json-snatcher
          json-reformat
          json-reformat
          json-snatcher
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

  ; fetch the list of packages available
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  ; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;;;;---------------------------------------------------------------------------
;; PACKAGES
;;;;---------------------------------------------------------------------------

(require 'windmove)
(require 'buffer-move)

;; Highlight uncommited git changes: https://github.com/dgutov/diff-hl
(require 'diff-hl)
(require 'diff-hl-margin)
(global-diff-hl-mode)

;; DEPRECATED AS FUCK
;; IDO - interactive do, basically auto-completion for switching buffers and
;;       finding files. Replaces main C-x f and C-x b.
;; (require 'ido)
;; (ido-mode t)
;; (ido-vertical-mode t)
;;
;; Helm! Which replaced IDO and does dope shit for finding files/buffers
(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-fuzzy-find)
;; open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p           t
      ;; move to end or beginning of source when reaching top or bottom of source
      helm-move-to-line-cycle-in-source     t
      ;; search for library in `require' and `declare-function' sexp
      helm-ff-search-library-in-sexp        t
      ;; scroll 8 lines other window using M-<next>/M-<prior>
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t)

;; Stripes - sets the background color of every even line. In this case, it's set
;; to #141414 -- change in stripes.el
(require 'stripes)
;;(add-hook 'after-change-major-mode-hook 'turn-on-stripes-mode)

;; Column-marker - let's highlight column 80 so we know where to trim lines.
;; Love me dat PEP
(require 'column-marker)
;; Highlight-chars - Customizable regex highlighting.
(require 'highlight-chars)

(define-minor-mode respectful-mode
  "I use this to tell emacs not to care that other people do shitty things like use tabs or leave trailing whitespace. -_-"
  :init-value nil
  " respectful"
  nil
  nil
  :global 1
  (funcall (lambda()
             (if (equal hc-highlight-trailing-whitespace-p 't)
                 (progn
                   (remove-hook 'before-save-hook 'delete-trailing-whitespace)
                   (hc-toggle-highlight-trailing-whitespace 0)
                   (hc-toggle-highlight-tabs 0)
                   (message "respectful-mode enabled"))
               (progn
                 (add-hook 'before-save-hook 'delete-trailing-whitespace)
                 (hc-toggle-highlight-trailing-whitespace 't)
                 (hc-toggle-highlight-tabs 't)
                 (message "respectful-mode disabled; fuck yeah")))
             )))


;; Run these every time we change modes.
(add-hook
 'after-change-major-mode-hook
 (lambda ()

   ;; Highlight tabs - we almost always want spaces. (exception: Go-mode)
   (unless (or (equal major-mode 'go-mode)
               (equal major-mode 'fundamental-mode)
               (equal major-mode 'term-mode))
     (progn (hc-toggle-highlight-trailing-whitespace 't)
            (hc-toggle-highlight-tabs 't)))

   (if (or (equal major-mode 'fundamental-mode)
           (equal major-mode 'term-mode))
       ;; Don't because we're not editing code, or we must permit evil.
       (remove-hook 'before-save-hook 'delete-trailing-whitespace)
     ;; Delete trailing whitespace on save.
     (add-hook 'before-save-hook 'delete-trailing-whitespace))


   ;; Go format.
   (if (equal major-mode 'go-mode)
       (add-hook 'before-save-hook 'gofmt-before-save)
     (remove-hook 'before-save-hook 'gofmt-before-save))

   ;; Column-number - show c # in the line at the bottom (what's that called?)
   (column-number-mode)

   ;; Highlight uncommited git changes: https://github.com/dgutov/diff-hl
   (diff-hl-margin-mode)

   ;; Line-limit's usually 80 characters
   (unless (equal major-mode 'fundamental-mode) (column-marker-1 81))
   ;; But not always
   (if (or (equal major-mode 'java-mode)
           (equal major-mode 'json-mode)
           (equal major-mode 'dart-mode)
           (equal major-mode 'html-mode)) (column-marker-1 101))

   ))

(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(split-height-threshold nil)
 '(split-width-threshold 0)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;---------------------------------------------------------------------------
;; SECTION: MODES
;;;;---------------------------------------------------------------------------

;; Python mode
;; Jedi mode (python autocompletion)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Ruby mode

;; Add Rubocop to ruby-mode
(add-hook 'ruby-mode-hook 'rubocop-mode)

;; Ace Jump Mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;; C++ Mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(c-add-style
 "my-stroustrup"
 '("stroustrup"
   (indent-tabs-mode . nil)               ; use spaces rather than tabs
   (c-basic-offset . 4)                   ; indent by four spaces
   (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                       (brace-list-open . 0)
                       (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-stroustrup")
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)

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
(setq js2-basic-offset 2)

;; SCSS Mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

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
            (".erb" . html-mode)
            ("Rakefile" . ruby-mode))
          auto-mode-alist))


;;;;---------------------------------------------------------------------------
;; SECTION: Preferences
;;;;---------------------------------------------------------------------------

(defun split-horizontally-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (and (one-window-p t)
             (not (active-minibuffer-window)))
    (split-window-horizontally)))
(add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)


;; Helm-mode on
(helm-mode 1)

;; Resize the helm window according to its content
(helm-autoresize-mode 1)

;; Keep it tight, tho
(setq-default helm-autoresize-max-height 30)
(setq-default helm-mode-fuzzy-match t)
(setq-default helm-completion-in-region-fuzzy-match t)

(setq-default indent-tabs-mode nil)

;; Follow symlinks
(setq-default vc-follow-symlinks t)

;; Turn on syntax hightlighting.
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs

;; Show me dem line-numbers
(setq linum-format "%d ")
(add-hook 'after-change-major-mode-hook 'linum-mode)

;; Set magit colors.
;; (setq magit-auto-revert-mode nil)
;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "black")
;;      (set-face-foreground 'magit-diff-del "red3")
;;      (unless window-system
;;               (set-face-background 'magit-item-highlight "white"))))
(setq magit-last-seen-setup-instructions "1.4.0")

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

;; Universal

;; Map M-x to M-a
(global-set-key (kbd "M-a") 'execute-extended-command)

;; Reload from file
(global-set-key (kbd "C-x r") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-x C-r") 'revert-all-buffers)

;; respectful mode
(global-set-key (kbd "C-x e") 'respectful-mode)

;; gtags-find-symbol
(global-set-key (kbd "C-x g") 'helm-gtags-find-tag)

;; TODO Figure out indent region.
;; (global-set-key (kbd "<C-tab>") 'indent-region)

;; line-mode for editing, char-mode for terminal
(global-set-key (kbd "C-x x") 'toggle-term-mode)

;; Copy to system clipboard
(global-set-key (kbd "ESC c") 'copy-to-sys-clipboard)

;; Ace jump
(define-key global-map (kbd "ESC SPC") 'ace-jump-mode)

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

;; Helm, has a great command auto-completion interface, so we'll assign it to M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-w") 'helm-imenu)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "M-m") 'helm-do-grep)


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
(global-set-key (kbd "ESC i")    'windmove-up)
(global-set-key (kbd "ESC k")  'windmove-down)
(global-set-key (kbd "ESC j")  'windmove-left)
(global-set-key (kbd "ESC l") 'windmove-right)

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
