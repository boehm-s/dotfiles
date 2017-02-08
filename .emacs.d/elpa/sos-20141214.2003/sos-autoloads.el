;;; sos-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "sos" "sos.el" (22619 62249 23714 41000))
;;; Generated autoloads from sos.el

(autoload 'sos "sos" "\
Searches StackOverflow for the given `query'. Displays excerpts from the search results.

API Reference: http://api.stackexchange.com/docs/excerpt-search

\(fn QUERY)" t nil)

(autoload 'sos-answer "sos" "\
Get answers for SO question ID as defined in property block of the current question.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sos-autoloads.el ends here
