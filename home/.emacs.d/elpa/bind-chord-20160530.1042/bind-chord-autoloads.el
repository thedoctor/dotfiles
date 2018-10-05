;;; bind-chord-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (bind-chords bind-chord) "bind-chord" "bind-chord.el"
;;;;;;  (22445 32133 758254 997000))
;;; Generated autoloads from bind-chord.el

(autoload 'bind-chord "bind-chord" "\
Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed).

\(fn CHORD COMMAND &optional KEYMAP)" nil t)

(autoload 'bind-chords "bind-chord" "\
Bind multiple chords at once.

Accepts keyword argument:
:map - a keymap into which the keybindings should be added

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

\(fn &rest ARGS)" nil t)

;;;***

;;;### (autoloads nil nil ("bind-chord-pkg.el") (22445 32133 833339
;;;;;;  337000))

;;;***

(provide 'bind-chord-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bind-chord-autoloads.el ends here
