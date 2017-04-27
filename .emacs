;; package --- summary
;;; code:
;;; commentary:


;; packages
(setq package-list
      (list

       'elscreen              ; tabs
       'emmet-mode            ; emmet mode
       'esqlite               ; sqlite
       'f                     ; Modern API for working with files and directories
       'google-translate      ; translate in emacs
       'ido                   ; manage buffers
       'json-reformat         ; format JSON
       'js-comint             ; js interpreter
       'linum-relative        ; linum relative
       'magit                 ; git integration
       'magit-popup
       'multiple-cursors      ; ST-like multiple cursors
       'neotree               ; menu bar
       'nodejs-repl           ; nodeJS REPL
       'nyan-prompt
       'pcre2el               ; regexp syntax converter
       'phi-search            ; replace isearch
       'rainbow-delimiters    ; rainbowy parens/braces/...
       'request
       'smex                  ; M-x
       'sr-speedbar           ; menu bar
       'tern                  ; Tern-powered JavaScript integration
       'yasnippet             ; snippets for emacs
       'zlc                   ; zsh for emacs

       'zenburn-theme

       ;; popup
       'popup
       'popup-complete
       'popup-imenu
       'popup-kill-ring
       'popup-switcher

       ;; modes
       'angular-mode
       'arduino-mode
       'coffee-mode
       'dockerfile-mode
       'emmet-mode
       'go-mode
       'handlebars-mode
       'jade-mode
       'json-mode
       'js2-mode
       'js3-mode
       'markdown-mode
       'nyan-mode
       'php-mode
       'rust-mode
       'skewer-mode
       'toml-mode
       'top-mode
       'web-mode

       ;; helm
       'helm
       'helm-c-moccur
       'helm-c-yasnippet
       'helm-company
       'helm-core
       'helm-emmet
       'helm-swoop

       ;; company
       'company
       'company-arduino
       'company-c-headers
       'company-go
       'company-irony
       'company-php
       'company-tern
       'company-web

       ;; 'auto-complete         ; autocompletion
       ;; 'autocomplete-config   ; autocompletion conf
       ))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa" . "http://melpa.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")))
  )

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))

(add-to-list 'load-path "~/.emacs.d/elpa")

;; modes per files type
(add-to-list 'auto-mode-alist '("\\.js\\'" . (lambda() (js2-mode) (company-mode) (company-tern) )))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . (lambda() (web-mode) (emmet-mode))))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . (lambda() (markdown-mode) (setq-default indent-tabs-mode nil)) ))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))


(load "~/.emacs.d/custom.el")
(load "~/.emacs.d/experiment.el")
(load "~/.emacs.d/linum.el")
(load "~/.emacs.d/js-config.el")
(load "~/.emacs.d/web-config.el")

(when (display-graphic-p)
  (load "~/.emacs.d/gui.el"))

(smerge-mode)

;; shortcuts
(global-set-key (kbd "C-x C-<right>") 'split-and-find-file-H)
(global-set-key (kbd "C-x C-<left>")  'split-and-find-file-H)
(global-set-key (kbd "C-x C-<up>")    'split-and-find-file-V)
(global-set-key (kbd "C-x C-<down>")  'split-and-find-file-V)

(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;; (global-set-key (kbd "C-g s t")  'magit-status)
;; (global-set-key (kbd "C-g b l")  'magit-blame)
;; (global-set-key (kbd "C-g c o")  'magit-commit)
;; (global-set-key (kbd "C-g p s")  'magit-push)
;; (global-set-key (kbd "C-g p l")  'magit-pull)


(global-set-key (kbd "C-x C-x")  'delete-window)
(global-set-key (kbd "C-x C-m")  'neotree)

;; minor mode for overriding some major-mode keymap
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))

    ;; helm
    (define-key map (kbd "M-x") 'helm-M-x)
    (define-key map (kbd "C-x C-f") 'helm-find-files)
    (define-key map (kbd "C-x C-e") 'emmet-preview)

    (define-key map (kbd "M-z") 'custom-prompt)

    (define-key map (kbd "C-c C-c") 'comment-dwim)
    (global-set-key (kbd "C-d") 'duplicate-line)

    (define-key map (kbd "C-c RET") 'mc/edit-lines)
    (define-key map (kbd "C-c C-s") 'mc/mark-next-like-this-word)
    (define-key map (kbd "C-c C-r") 'mc/mark-previous-like-this-word)

    (define-key map (kbd "C-c C-<left>") 'hs-hide-all)
    (define-key map (kbd "C-c C-<right>") 'hs-show-all)
    (define-key map (kbd "C-c <left>") 'hs-hide-block)
    (define-key map (kbd "C-c <right>") 'hs-show-block)

    (define-key map (kbd "C-f") 'helm-swoop)

    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")
(my-keys-minor-mode 1)

;; active company-mode
(global-company-mode)

;; Header ETNA
(load-file "~/.emacs.d/std_comment.el")

;; delete end of line whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; backup directory for ~ files
(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))

;; spaces for indent
;; (setq-default indent-tabs-mode nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun buffer-contains-substring (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))


 (add-hook 'buffer-list-update-hook 'trigger-smerge)


(defun nodejs-repl-restart ()
  "restart the nodejs REPL"
  (interactive)
  (defvar nodejs-repl-code
    (concat "process.stdout.columns = %d;" "require('repl').start('%s', null, null, true, false)"))
  (with-current-buffer "*nodejs*"
    (kill-process nil comint-ptyp)
    (run-with-timer 0.01 nil (lambda ()
			      (setq nodejs-repl-prompt-re (format nodejs-repl-prompt-re-format nodejs-repl-prompt nodejs-repl-prompt))
			      (with-current-buffer "*nodejs*"
				(apply 'make-comint nodejs-repl-process-name nodejs-repl-command nil `("-e" ,(format nodejs-repl-code (window-width) nodejs-repl-prompt)))
				(nodejs-repl-mode) (erase-buffer) )))))


(defun trigger-smerge ()
  (when (buffer-contains-substring "<<<<<<") nil
      (smerge-mode))
)
