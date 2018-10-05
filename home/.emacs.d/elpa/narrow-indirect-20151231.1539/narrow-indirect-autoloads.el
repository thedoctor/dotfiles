;;; narrow-indirect-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "narrow-indirect" "narrow-indirect.el" (22466
;;;;;;  3179 201379 298000))
;;; Generated autoloads from narrow-indirect.el

(autoload 'ni-narrow-to-defun-indirect-other-window "narrow-indirect" "\
`narrow-to-defun' in a cloned indirect buffer in the other window.
The name of the indirect buffer depends on the use of a prefix arg:

* No prefix arg: the current buffer name, but with ` | NAME'
  appended, where NAME is the name of the object defined by the defun.
  (Actually, option `ni-buf-name-separator' prefixes NAME.  \" | \" is
  the default value of this option.)

* Prefix arg < 0 : like no prefix arg, but you are prompted for NAME.

* Prefix arg >= 0: you are prompted for the full buffer name.

However, the buffer name is in any case truncated at
`ni-narrowed-buf-name-max' chars.

Non-interactively:
* Non-nil FULL-NAME is the full buffer name, and TEXT is ignored.
* Non-nil TEXT is used for NAME, if FULL-NAME is nil.

See `clone-indirect-buffer'.

\(fn &optional FULL-NAME TEXT)" t nil)

(autoload 'ni-narrow-to-region-indirect-other-window "narrow-indirect" "\
`narrow-to-region' in a cloned indirect buffer in the other window.
The indirect buffer is named the same as the current buffer, except:

 * It is prefixed by the value of option `ni-buf-name-prefix'.
 * It is suffixed by ` | TEXT', where TEXT is the region text,
   filtered by collapsing whitespace and (for Emacs 24.4+) removing
   invisible text.  (Actually, option `ni-buf-name-separator' prefixes
   TEXT.  \" | \" is the default value of this option.)

However, the buffer name is in any case truncated at
`ni-narrowed-buf-name-max' chars.

Non-interactively:
START and END are the region beginning and end.
HERE is where to place the cursor, relative to START.
TEXT is prefixed by `ni-buf-name-separator' and appended to the
 original buffer name, which is appended to `ni-buf-name-prefix' to
 name the new buffer.
If FULL-NAME is a string then it is used as the complete indirect
buffer name.  (TEXT is then ignored.)

See `clone-indirect-buffer'.

\(fn START END HERE &optional FULL-NAME TEXT MSGP)" t nil)

(autoload 'ni-narrow-to-page-indirect-other-window "narrow-indirect" "\
`narrow-to-page' in a cloned indirect buffer in the other window.

See `clone-indirect-buffer'.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; narrow-indirect-autoloads.el ends here
