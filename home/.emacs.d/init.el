(require 'package)

;; bootstrap utils

(defconst my-custom-file "~/.emacs.d/custom.el")

(setq package-enable-at-startup t)
(setq auto-save-default nil)
(setq make-backup-files t)
(put 'erase-buffer 'disabled nil)

;; turn off startup screen

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; load extensions

(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/init-real.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (el-get zygospore ztree zoom-frm yoshi-theme yaml-mode xmlgen wgrep-helm web-mode visual-regexp-steroids use-package-chords undercover unbound typed-clojure-mode twittering-mode symon swoop suscolors-theme sunrise-x-loop sudo-ext sublime-themes stripe-buffer string-edit sotclojure sos soothe-theme solidity-mode smex smartrep smartparens smart-shift smart-mode-line slamhound shift-number shell-pop scss-mode scratch-persist scratch-ext savekill ruby-mode rubocop rotate reykjavik-theme revive req-package rectangle-utils rake rainbow-delimiters psvn prodigy plan9-theme pkgbuild-mode php-mode peek-mode paradox overseer org-trello org-dashboard org-cliplink org-bullets nyan-mode neotree narrow-indirect multifiles move-text mmm-mode mbo70s-theme markdown-mode malabar-mode makefile-runner magit-svn magit-gitflow magit-gh-pulls lua-mode load-dir litable list-processes+ kibit-helper keyfreq json-mode js2-mode jedi jazz-theme jabber igrep ido-at-point idle-highlight-mode httprepl howdoi hl-defined hindent highlight-numbers highlight-chars helm-themes helm-systemd helm-swoop helm-proc helm-package helm-org-rifle helm-open-github helm-make helm-ls-git helm-helm-commands helm-gtags helm-google helm-gitignore helm-github-stars helm-fuzzy-find helm-flycheck helm-descbinds helm-company helm-circe helm-cider hc-zenburn-theme haskell-snippets guide-key gruvbox-theme gotham-theme google-translate google-this go-mode go-errcheck glsl-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-blame gist flymake-solidity flycheck-protobuf flycheck-pos-tip flycheck-clojure flx-ido fireplace firebelly-theme find-temp-file fic-mode expand-region ert-modeline ert-expectations ensime emr emmet-mode elm-yasnippets elm-mode elisp-slime-nav elfeed el-mock edit-server duplicate-thing dummy-h-mode dracula-theme dockerfile-mode docker django-mode dired-rainbow dired-open diff-hl define-word debbugs darktooth-theme danneskjold-theme cyberpunk-theme css-mode company-restclient company-quickhelp company-irony company-ghc column-marker coffee-mode clojure-snippets clojure-mode-extra-font-locking clojure-cheatsheet cljsbuild-mode cljr-helm cider-profile camcorder buffer-move beacon batch-mode bash-completion aurel arduino-mode anzu ant align-cljlet ace-window ace-link ace-jump-mode ace-jump-helm-line ace-jump-buffer 4clojure))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
