;; start emacs server

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; (require 'atomic-chrome)
;; (atomic-chrome-start-server)

;; recompile configs

(defconst emacs-major-version-rad 1000000)

(add-hook 'kill-emacs-hook (lambda () (byte-recompile-directory my-init-dir 0 t)))

;; elpa

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
;; unsure if still-in-use
;; (add-to-list 'package-archives '("sunrise" . "http://joseito.republika.pl/sunrise-commander/"))
;; fuckers don't know how to serve an intermediate tls cert
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(defun has-emacs-version (major minor)
  (<= (+ (* major emacs-major-version-rad) minor)
      (+ (* emacs-major-version emacs-major-version-rad) emacs-minor-version)))

(if (and (not (has-emacs-version 24 0))
         (not (has-emacs-version 25 0))
         (not (has-emacs-version 26 0)))
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(eval-when-compile (package-initialize))

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents) package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL (package-install package)))
             (require package))))

;; el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (require-package 'el-get)
;; (use-package el-get
;;   :config
;;   (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;;   (el-get 'sync))

;; use package

(require-package 'use-package)
(require 'use-package)

;; chords

(require-package 'use-package-chords)
(require 'use-package-chords)

;; req-package

(require-package 'req-package)
(require 'req-package)

(req-package--log-set-level 'trace)

;; load custom
(setq custom-file my-custom-file)

;; init.d
;; (req-package highlight-chars)

(random t)
(req-package load-dir
  :force true
  :init
  (setq force-load-messages t)
  (setq load-dir-debug t)
  (setq load-dir-recursive t)
  :config
  (load-dir-one my-init-dir)
  (load my-custom-file t)
  (select-theme)
  (req-package-finish))
