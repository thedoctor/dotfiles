;;; aurel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "aurel" "aurel.el" (22466 3290 428074 673000))
;;; Generated autoloads from aurel.el

(autoload 'aurel-package-info "aurel" "\
Display information about AUR package with NAME.
The buffer for showing results is defined by `aurel-info-buffer-name'.
With prefix (if ARG is non-nil), show results in a new info buffer.

\(fn NAME &optional ARG)" t nil)

(autoload 'aurel-package-search "aurel" "\
Search for AUR packages matching STRING.

STRING can be a string of multiple words separated by spaces.  To
search for a string containing spaces, quote it with double
quotes.  For example, the following search is allowed:

  \"python library\" plot

The buffer for showing results is defined by
`aurel-list-buffer-name'.  With prefix (if ARG is non-nil), show
results in a new buffer.

\(fn STRING &optional ARG)" t nil)

(autoload 'aurel-package-search-by-name "aurel" "\
Search for AUR packages with name containing STRING.

The buffer for showing results is defined by
`aurel-list-buffer-name'.  With prefix (if ARG is non-nil), show
results in a new buffer.

\(fn STRING &optional ARG)" t nil)

(autoload 'aurel-maintainer-search "aurel" "\
Search for AUR packages by maintainer NAME.
The buffer for showing results is defined by `aurel-list-buffer-name'.
With prefix (if ARG is non-nil), show results in a new buffer.

\(fn NAME &optional ARG)" t nil)

(autoload 'aurel-installed-packages "aurel" "\
Display information about AUR packages installed in the system.
The buffer for showing results is defined by `aurel-list-buffer-name'.
With prefix (if ARG is non-nil), show results in a new buffer.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; aurel-autoloads.el ends here
