;; start emacs server

(require 'server)
(unless (server-running-p)
  (server-start))

;; version detection

(defconst emacs-major-version-rad 1000000)

(defun has-emacs-version (major minor)
  (<= (+ (* major emacs-major-version-rad) minor)
      (+ (* emacs-major-version emacs-major-version-rad) emacs-minor-version)))

(if (and (not (has-emacs-version 24 0))
         (not (has-emacs-version 25 0))
         (not (has-emacs-version 26 0)))
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; elpa

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

(defconst my-init-dir "~/.emacs.d/init.d")

(eval-when-compile (package-initialize))

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

;; use package

(require-package 'use-package)
(require 'use-package)

;; custom files

(add-to-list 'load-path "~/.emacs.d/snippets")
(require-package 'highlight-chars)
(require-package 'column-marker)

;; el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require-package 'el-get)
(require 'el-get)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
(el-get 'sync)

;; :el-get keyword for use-package

(use-package use-package-el-get
  :ensure t
  :config (use-package-el-get-setup))

;; chords

(use-package use-package-chords
  :ensure t)

;; req-package

(use-package req-package
  :ensure t
  :config (req-package--log-set-level 'debug))

;; init.d

(random t)
(req-package load-dir
  :ensure t
  :force true
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config
  (load-dir-one my-init-dir)
  (req-package-finish)
  (funcall 'select-theme))
 
(load-file "~/.emacs.d/custom.el")