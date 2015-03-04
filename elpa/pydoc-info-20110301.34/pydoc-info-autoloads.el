;;; pydoc-info-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pydoc-info-add-help) "pydoc-info" "pydoc-info.el"
;;;;;;  (21392 60035 0 0))
;;; Generated autoloads from pydoc-info.el

(require 'info-look)

(autoload 'pydoc-info-add-help "pydoc-info" "\
Add help specifications for a list of Info FILES.

The added specifications are tailored for use with Info files
generated from Sphinx documents.

MORE-SPECS are additional or overriding values passed to
`info-lookup-add-help'.

\(fn FILES &rest MORE-SPECS)" nil nil)

(pydoc-info-add-help '("python"))

;;;***

;;;### (autoloads nil nil ("pydoc-info-pkg.el") (21392 60035 834821
;;;;;;  0))

;;;***

(provide 'pydoc-info-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pydoc-info-autoloads.el ends here
