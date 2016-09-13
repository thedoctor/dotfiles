;;; rectangle-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "rectangle-utils" "rectangle-utils.el" (22466
;;;;;;  3223 808849 630000))
;;; Generated autoloads from rectangle-utils.el

(autoload 'rectangle-utils-extend-rectangle-to-end "rectangle-utils" "\
Create a rectangle based on the longest line of region.

\(fn BEG END)" t nil)

(autoload 'rectangle-utils-menu "rectangle-utils" "\


\(fn BEG END)" t nil)

(autoload 'rectangle-utils-insert-at-right "rectangle-utils" "\
Create a new rectangle based on longest line of regionand insert string at right of it.
With prefix arg, insert string at end of each lines (no rectangle).

\(fn BEG END ARG)" t nil)

(autoload 'rectangle-utils-copy-rectangle "rectangle-utils" "\
Well, copy rectangle, not kill.

\(fn BEG END)" t nil)

(autoload 'rectangle-utils-extend-rectangle-to-space "rectangle-utils" "\
Allow creating a rectangular region up to space.
The rectangle is extended indeed to `rectangle-utils--extend-region-to-space-separator'.

\(fn BEG END)" t nil)

(autoload 'rectangle-utils-extend-rectangle-to-space-or-paren "rectangle-utils" "\
Allow creating a rectangular region up to space or paren i.e \"(\".
Useful to realign let, setq etc...

\(fn BEG END)" t nil)

(autoload 'rectangle-utils-extend-rectangle-to-space-or-dot "rectangle-utils" "\
Allow creating a rectangular region up to space or dot.
Useful to realign alists.

\(fn BEG END)" t nil)

(autoload 'rectangle-utils-extend-rectangle-to-regexp "rectangle-utils" "\
Allow creating a rectangular region up to regexp.

\(fn BEG END)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rectangle-utils-autoloads.el ends here
