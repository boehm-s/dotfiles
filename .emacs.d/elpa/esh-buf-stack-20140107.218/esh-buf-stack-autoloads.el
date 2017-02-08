;;; esh-buf-stack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "esh-buf-stack" "esh-buf-stack.el" (22619 59784
;;;;;;  799636 153000))
;;; Generated autoloads from esh-buf-stack.el

(autoload 'setup-eshell-buf-stack "esh-buf-stack" "\
Setup the buffer stack for Eshell.

\(fn)" t nil)

(autoload 'eshell-pop-stack "esh-buf-stack" "\
Pop a command from the buffer stack.

\(fn)" t nil)

(autoload 'eshell-push-command "esh-buf-stack" "\
Push CMD to the buffer stack.

\(fn CMD)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; esh-buf-stack-autoloads.el ends here
