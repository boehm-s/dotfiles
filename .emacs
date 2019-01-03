(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/elpa"))
       load-path))

(require 'package)

(when (>= emacs-major-version 24)
      (require 'package)
      (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                               ("melpa" . "http://melpa.org/packages/")
                               ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                               ("org" . "http://orgmode.org/elpa/"))
      )
)

(setq package-list
      (list

       'emmet-mode            ; emmet mode
       'esqlite               ; sqlite
       'f                     ; Modern API for working with files and directories
       'google-translate      ; translate in emacs
       'ido                   ; manage buffers
       'json-reformat         ; format JSON
       'js-comint             ; js interpreter
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
       'tern                  ; Tern-powered JavaScript integration
       'yasnippet             ; snippets for emacs
       'zlc                   ; zsh for emacs
       'browse-kill-ring      ; kill-ring history

       ;;'zenburn-theme

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
       'racket-mode

       ;; helm
       'helm
       'helm-c-moccur
       'helm-c-yasnippet
       'helm-company
       'helm-smex
       'helm-projectile

       ;;'helm-core
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

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))

(load "~/.emacs.d/font-lock+.el")
(add-to-list 'load-path "~/.emacs.d/sidebar.el/")
(add-to-list 'load-path "~/.local/share/icons-in-terminal/")

(require 'sidebar)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'use-file-dialog) (setq use-file-dialog nil))
(if (fboundp 'use-dialog-box) (setq use-dialog-box nil))

(fringe-mode '(0 . 0))

(when (display-graphic-p)
  (setq inhibit-startup-screen t)
)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes '(zerodark))
 '(custom-safe-themes
   '("cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "6332c9756bde31cf9e34154395868413e45714488507527969f95a61b5f24518" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(fci-rule-color "#383838")
 ;; '(global-whitespace-mode t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(zerodark-theme arduino-mode atomic-chrome desktop-environment docker editorconfig eslint-fix haskell-mode lsp-haskell lsp-intellij lsp-java lsp-javacomp lsp-javascript-typescript lsp-mode lsp-rust lsp-ui nodemcu-mode wiki-summary lyrics helm-spotify helm-spotify-plus jetbrains jekyll-modes helm-smex rainbow-identifiers zlc ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package twittering-mode top-mode toml-mode tern-auto-complete sr-speedbar sos smex skewer-reload-stylesheets skewer-less rust-playground request rainbow-delimiters quelpa projectile popwin popup-switcher popup-kill-ring popup-imenu popup-complete phi-search persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file nyan-prompt nyan-mode nodejs-repl neotree multi-term mpg123 move-text markdown-mode magit macrostep lorem-ipsum livid-mode linum-relative link-hint json-mode js3-mode js2-refactor js-doc js-comint jade-mode isend-mode info+ indent-guide ido-vertical-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt helm-swoop helm-emmet helm-company helm-c-yasnippet helm-c-moccur handlebars-mode hackernews hacker-typer google-translate gh-md gh expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu esqlite eshell-z eshell-up eshell-prompt-extras eshell-git-prompt eshell-fringe-status eshell-did-you-mean eshell-autojump esh-help esh-buf-stack elscreen elisp-slime-nav dumb-jump dockerfile-mode dash-at-point company-web company-tern company-php company-go company-arduino column-enforce-mode coffee-mode clean-aindent-mode cargo browse-kill-ring auto-highlight-symbol auto-complete-c-headers auto-complete-auctex auto-compile angular-mode 2048-game))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
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
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq sml/theme 'respectful)

;; (nyan-mode)

;; (rainbow-delimiters-mode)
;; (put 'dired-find-alternate-file 'disabled nil)

;; (setq load-prefer-newer t)
;; (setq show-smartparens-delay 0)
;; (show-smartparens-global-mode +1)

(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode)) ;; TODO
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) ;; TODO
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . (lambda() (web-mode) (emmet-mode))))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . (lambda() (markdown-mode) (setq-default indent-tabs-mode nil)) ))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.racket\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(add-hook 'csharp-mode-hook 'omnisharp-mode)

(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook #'company-mode)

(require 'company)
(require 'company-tern)

(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)
			   (company-mode)))


;; (eval-after-load 'tern
;;    '(progn
;;       (add-to-list 'company-backends 'company-tern)))

(defun my-web-mode-hook ()
  "Web mode customization."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (set-face-attribute 'web-mode-doctype-face nil :foreground "#1affff")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#999999")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#493e99")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#264d73")
  (set-face-attribute 'web-mode-html-attr-value-face nil :foreground "#336699")

  (set-face-attribute 'web-mode-function-call-face nil :foreground "#33d6ff")
  (set-face-attribute 'web-mode-function-name-face nil :foreground "#33d6ff")
  (setq web-mode-enable-css-colorization t)
  (set-face-attribute 'web-mode-css-at-rule-face nil :foreground "Pink3")

  (setq web-mode-enable-heredoc-fontification t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  )

(add-hook 'web-mode-hook  'my-web-mode-hook)

(global-company-mode)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-selection)
     (define-key company-active-map [tab] 'company-complete-selection)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [backtab] 'tab-indent-or-complete)

(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

;; eval region js and insert
(defun node-js-eval-region-or-buffer ()
  "evaluate the region and 'node' it !"
  (interactive)
    (insert
     (shell-command-to-string
      (concat "node -e '"
	      (buffer-substring (mark) (point))
	      "';")))
    (setq deactivate-mark t))

;; eval region python and insert
(defun python-eval-region-or-buffer ()
  "evaluate the region and 'python' it !"
  (interactive)
    (insert
     (shell-command-to-string
      (concat "python -c '"
	      (buffer-substring (mark) (point))
	      "';")))
    (setq deactivate-mark t))

(defun custom-prompt (str)
  (interactive
   (list
    (read-string "my_prompt : ")))
  (when (string-match "^\:\\(.[[:digit:]]+\\)" str)
    (goto-line (string-to-number (match-string 1 str))))
  (when (string-match "^d\\([[:digit:]]+\\)" str)
    (dotimes (i (string-to-number (match-string 1 str))) (kill-line)))
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2017-01-23"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (line-beginning-position))
      (setq $p2 (line-end-position))))
	     (copy-to-register ?1 $p1 $p2)))


(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun  split-and-find-file-H ()
  "Split the window and open the find-file prompt"
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (helm-find-files ".")
  )

(defun  split-and-find-file-V ()
  "Split the window and open the find-file prompt"
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (helm-find-files ".")
  )

(defvar custom-keys-map (make-keymap) "my custom shortcuts")

(define-key custom-keys-map (kbd "C-x C-<right>") 'split-and-find-file-H)
(define-key custom-keys-map (kbd "C-x C-<left>")  'split-and-find-file-H)
(define-key custom-keys-map (kbd "C-x C-<up>")    'split-and-find-file-V)
(define-key custom-keys-map (kbd "C-x C-<down>")  'split-and-find-file-V)

(define-key custom-keys-map (kbd "s-<left>")  'windmove-left)
(define-key custom-keys-map (kbd "s-<right>") 'windmove-right)
(define-key custom-keys-map (kbd "s-<up>")    'windmove-up)
(define-key custom-keys-map (kbd "s-<down>")  'windmove-down)

(define-key custom-keys-map (kbd "M-<left>")  'windmove-left)
(define-key custom-keys-map (kbd "M-<right>") 'windmove-right)
(define-key custom-keys-map (kbd "M-<up>")    'windmove-up)
(define-key custom-keys-map (kbd "M-<down>")  'windmove-down)

(define-key custom-keys-map (kbd "<f12>")  (lambda() (interactive) (multi-term-dedicated-open) (other-window 1)))
(define-key custom-keys-map (kbd "M-k")  'browse-kill-ring)
(define-key custom-keys-map (kbd "C-x C-x")  'delete-window)
(define-key custom-keys-map (kbd "C-x C-m")  'sidebar-open)

;; helm
(define-key custom-keys-map (kbd "M-x") 'helm-smex)
(define-key custom-keys-map (kbd "C-x C-f") 'helm-projectile)
(define-key custom-keys-map (kbd "C-x C-e") 'emmet-preview)

(define-key custom-keys-map (kbd "M-z") 'custom-prompt)
(define-key custom-keys-map (kbd "M-l") 'goto-line)

(define-key custom-keys-map (kbd "C-c C-c") 'comment-dwim)

(define-key custom-keys-map (kbd "C-d") 'duplicate-line-or-region)
(define-key custom-keys-map (kbd "C-S-c") 'xah-copy-to-register-1)
(define-key custom-keys-map (kbd "C-S-v") 'xah-paste-from-register-1)

(define-key custom-keys-map (kbd "C-c RET") 'mc/edit-lines)
(define-key custom-keys-map (kbd "C-c C-s") 'mc/mark-next-like-this-word)
(define-key custom-keys-map (kbd "C-c C-r") 'mc/mark-previous-like-this-word)

(define-key custom-keys-map (kbd "C-c C-<left>") 'hs-hide-all)
(define-key custom-keys-map (kbd "C-c C-<right>") 'hs-show-all)
(define-key custom-keys-map (kbd "C-c <left>") 'hs-hide-block)
(define-key custom-keys-map (kbd "C-c <right>") 'hs-show-block)

(define-key custom-keys-map (kbd "C-f") 'helm-swoop)

(define-minor-mode my-keys-minor-mode
"A minor mode so that my key settings override annoying major modes."
:init-value t
:lighter " my-keys"
:keymap custom-keys-map)

(defvar term-mode-keymap (make-keymap) "term-mode keymap.")

(define-key term-mode-keymap (kbd "s-<left>")  'windmove-left)
(define-key term-mode-keymap (kbd "s-<right>") 'windmove-right)
(define-key term-mode-keymap (kbd "s-<up>")    'windmove-up)
(define-key term-mode-keymap (kbd "s-<down>")  'windmove-down)

(define-key term-mode-keymap (kbd "M-<left>")  'windmove-left)
(define-key term-mode-keymap (kbd "M-<right>") 'windmove-right)
(define-key term-mode-keymap (kbd "M-<up>")    'windmove-up)
(define-key term-mode-keymap (kbd "M-<down>")  'windmove-down)
(define-key term-mode-keymap (kbd "M-x") 'helm-smex)

(define-key term-mode-keymap (kbd "C-c")  'term-interrupt-subjob)
(define-key term-mode-keymap (kbd "M-DEL") 'term-send-backward-kill-word)
(define-key term-mode-keymap (kbd "C-<right>") (lambda() (interactive) (term-send-raw-string "\e[1;5C")))
(define-key term-mode-keymap (kbd "C-<left>") (lambda() (interactive) (term-send-raw-string "\e[1;5D")))
(define-key term-mode-keymap (kbd "C-r") (lambda()(interactive) (term-send-raw-string "\C-r")))
(define-key term-mode-keymap (kbd "C-d") (lambda()(interactive) (term-send-raw-string "\C-d")))

(define-minor-mode my-term-minor-mode
"A minor mode so that I got a normal terminal."
:init-value nil
:lighter " my-term"
:keymap term-mode-keymap)

(add-hook 'term-mode-hook
(lambda()

(message "%s" "This is in term mode and hook enabled.")

(dolist (key '("\C-a" "\C-b" "\C-c" "\C-d" "\C-e" "\C-f" "\C-g"
"\C-h" "\C-k" "\C-l" "\C-n" "\C-o" "\C-p" "\C-q"
"\C-t" "\C-u" "\C-v" "\C-x" "\C-z" "\C-r" "\M-DEL" "\e"))
(local-unset-key key))

(my-keys-minor-mode -1)
(clean-aindent-mode -1)
(my-term-minor-mode 1)
))

;;     (require 'treemacs)
;;     (require 'grizzl)

;;     (defvar *_treemacs-search-index*  (grizzl-make-index (split-string (shell-command-to-string (concat "find " (treemacs--current-root))) "\n")  :case-sensitive t))
;; ;;      (defvar *treemacs-search-index*  (grizzl-make-index '("one" "two" "three" "four"))) ;; :case-sensitive t))
;;     (defvar *treemacs-current-search* "")
;;     (defvar res-buffer (get-buffer-create "*treemacs-fuzzy-search-RESULT*"))


;;     (defun treemacs-search-change ()  ""
;;     (setq *treemacs-search-index* *_treemacs-search-index*)
;;     ;; chamge value when treemacs root change
;;     ;; find hook for treemacs root change
;;     (setq *treemacs-current-search* (minibuffer-contents))
;;     (setq *treemacs-search-result* (grizzl-search *treemacs-current-search* *treemacs-search-index*))

;;     ;; somehow *treemacs-search-index*  becomes nil in grizzl-search function

;;     (grizzl-result-strings *treemacs-search-result* *treemacs-search-index*
;;     :start 0
;;     :end   100)
;;     ;; (switch-to-buffer-other-window res-buffer)
;;     )

;;   ;;  (let ((inhibit-modification-hooks nil)) (treemacs-search-change))

;;     (defun treemacs-fuzzy-search ()  ""
;;     (interactive)
;;     (setq *treemacs-search-index*  (split-string (shell-command-to-string (concat "find " (treemacs--current-root))) "\n"))
;;     (minibuffer-with-setup-hook
;;     (lambda ()
;;     (add-hook 'post-self-insert-hook #'treemacs-search-change nil t))
;;     (read-string (format "Pattern [%s]: " *treemacs-current-search*) nil nil *treemacs-current-search*))
;;     )

  ;; (setq *treemacs-current-search*



    ;; Hi,

    ;; I'm new to elisp programming, I want to implement a feature that I've seen in many emacs packages (helm, ido, fiplr ...) : calling a function (hook) when user types something in the minibuffer (read-string).

    ;; If someone could write the simpliest piece of code that implements this feature and explain it to me, it would be wonderful.
    ;; Something like writing to the current buffer everything I type on the minibuffer.

    ;; So far, I just know how to use `interactive` and `read-string` to get the user input.

    ;; Best regards,






    ;;  (fiplr-find-file-in-directory (treemacs--current-root) fiplr-ignored-globs)


    (defvar treemacs-mode-keymap (make-keymap) "treemacs-mode keymap.")

    (define-key treemacs-mode-keymap (kbd "<left>")  'treemacs-uproot)
    (define-key treemacs-mode-keymap (kbd "<right>")  'treemacs-RET-action)
    (define-key treemacs-mode-keymap (kbd "SPC")  'treemacs-RET-action)
    (define-key treemacs-mode-keymap (kbd "C-<return>")  'treemacs-change-root)
    (define-key treemacs-mode-keymap (kbd "C-<right>")  'treemacs-change-root)
    (define-key treemacs-mode-keymap (kbd "C-f")  'treemacs-fuzzy-search)

    (define-minor-mode my-treemacs-minor-mode
    "A minor mode for navigating in treemacs"
    :init-value nil
    :lighter " my-treemacs"
    :keymap treemacs-mode-keymap)

;; after-change-major-mode-hook
(add-hook 'after-change-major-mode-hook (lambda()
(when (eq major-mode 'treemacs-mode)
(interactive)
(linum-relative-global-mode nil)
(linum-mode -1)
(my-treemacs-minor-mode)
)
))

(setq org-support-shift-select t)

(my-keys-minor-mode 1)
(my-term-minor-mode -1)

(require 'atomic-chrome)
(unless (zerop (call-process "lsof" nil nil nil "-i" ":64292"))
(atomic-chrome-start-server)
)

(define-derived-mode atomic-edit-mode fundamental-mode "atomic-edit-mode"
  "major mode for editing textareas on chrome."
;;  (delete-other-windows)
  (toggle-frame-maximized)
  (set-frame-size (selected-frame) 40000 20)
  (set-frame-position (selected-frame) 0 10000)
)

(setq atomic-chrome-buffer-open-style 'frame)
(setq atomic-chrome-default-major-mode 'atomic-edit-mode)

(lambda ()
  (interactive)
  (let ((command (concat "-a " (Man-default-man-entry))))
    (man command)
    (other-window 1)))

(setenv "GDK_SCALE" "1")
(setenv "GDK_DPI_SCALE" "1")
(modify-all-frames-parameters '((inhibit-double-buffering . t)))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-sent-messages-behavior 'delete)

;; default
(setq mu4e-maildir "~/Maildir")
(setq mu4e-drafts-folder "/[Gmail].Brouillons")
(setq mu4e-sent-folder   "/[Gmail].Messages envoy&AOk-s")
(setq mu4e-trash-folder  "/[Gmail].Corbeille")

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Messages envoy&AOk-s" . ?s)
        ("/[Gmail].Corbeille"     . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
;; I don't use a signature...
(setq
 user-mail-address "boehm_s@etna-alternance.net"
 user-full-name  "Steven BOEHM"
 ;; message-signature
 ;;  (concat
 ;;    "Foo X. Bar\n"
 ;;    "http://www.example.com\n")
)

(setq message-kill-buffer-on-exit t)


(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

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

;; automatically start helm spotify plus

  (require 'helm-spotify-plus)
  (require 'lyrics)

  (defvar spotify-modeline-title-max-char 25)
  (defvar spotify-modeline-title-offset 0)
  (defvar spotify-modeline-title-to-display "")
  (defvar spotify-modeline-title-display "")

  (defvar spotify-modeline-metadata-bashstring  "metadata=$(dbus-send --print-reply --session --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata');")
  (defvar spotify-modeline-artist-bashstring  "artist=$(echo \"$metadata\" | grep -A2 albumArtist | tail -n1 | cut -d\\\" -f2);")
  (defvar spotify-modeline-song-bashstring  "song=$(echo \"$metadata\" | grep -A1 title | tail -n1 | cut -d\\\" -f2);")

  (defun spotify-modeline-artist ()
    (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat spotify-modeline-metadata-bashstring spotify-modeline-artist-bashstring "echo \"$artist\"")))
  )
  (defun spotify-modeline-song ()
    (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat spotify-modeline-metadata-bashstring spotify-modeline-song-bashstring "echo \"$song\"")))
  )
  (defun spotify-modeline-current ()
    (format "[%s]   %s" (spotify-modeline-artist) (spotify-modeline-song))
  )

  (defvar spotify-modeline-get-playing-music-bashstring  "metadata=$(dbus-send --print-reply --session --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata'); artist=$(echo \"$metadata\" | grep -A2 albumArtist | tail -n1 | cut -d\\\" -f2); song=$(echo \"$metadata\" | grep -A1 title | tail -n1 | cut -d\\\" -f2); echo \"[$artist]   $song\"")
  (defvar spotify-modeline-get-play-pause-bashstring "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'PlaybackStatus' | tail -n1 | cut -d\\\" -f2")

  (defvar current-music (spotify-modeline-current))
  (defvar music-paused (string= "Playing" (replace-regexp-in-string "\n$" "" (shell-command-to-string spotify-modeline-get-play-pause-bashstring))))
  (defun update-current-spotify-data ()
    (setq current-music (spotify-modeline-current))
    (setq spotify-modeline-title-to-display (concat (make-string spotify-modeline-title-max-char ? ) current-music (make-string  spotify-modeline-title-max-char ? )))
    (setq music-paused (string= "Playing" (replace-regexp-in-string "\n$" "" (shell-command-to-string spotify-modeline-get-play-pause-bashstring))))
    (setq spotify-modeline-title-display
      (condition-case err
	(substring spotify-modeline-title-to-display spotify-modeline-title-offset (+ spotify-modeline-title-max-char spotify-modeline-title-offset))
	(args-out-of-range (setq spotify-modeline-title-offset 0))
      )
    )
    (if (> spotify-modeline-title-offset (+ (length current-music) (- spotify-modeline-title-max-char 2)))
      (setq spotify-modeline-title-offset 0)
      (setq spotify-modeline-title-offset (+ spotify-modeline-title-offset 1))
    )
    (force-mode-line-update t)
  )

  (run-with-timer 0 0.2 'update-current-spotify-data)

  (setq-default
   mode-line-format
   '(; Position, including warning for 80 columns
     (:propertize "%5l:" face mode-line-position-face)
     (:eval (propertize "%3c" 'face
			(if (>= (current-column) 80)
			    'mode-line-80col-face
			  'mode-line-position-face)))
					  ; emacsclient [default -- keep?]
     mode-line-client
     " "
					   ; read-only or modified status
     (:eval
      (cond (buffer-read-only
	     (propertize "RO" 'face 'mode-line-read-only-face))
	    ((buffer-modified-p)
	     (propertize "**" 'face 'mode-line-modified-face))
	    (t "  ")))
     " "
					  ; directory and buffer/file name

     (:eval (if (string= "*" (substring (buffer-name) 0 1) )
		(propertize "" 'face 'mode-line-folder-face)
	      (propertize (shorten-directory default-directory 5) 'face
			  'mode-line-folder-face)))
     (:propertize "%b"
		  face mode-line-filename-face)
					  ; narrow [default -- keep?]
     "%n"
					  ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
     (vc-mode vc-mode)

     (:propertize " (" face mode-line-mode-face)
     (:propertize mode-name
		  face mode-line-mode-face)
     (:propertize ")" face mode-line-mode-face)

     (:eval (propertize (format-mode-line minor-mode-alist)
			'face 'mode-line-minor-mode-face))
     (:propertize mode-line-process
		  face mode-line-process-face)
     (global-mode-string global-mode-string)
     " "
					  ; nyan-mode uses nyan cat as an alternative to %p
     ;; (:eval (when nyan-mode (list (nyan-create))))

     (:propertize "     " nil nil)
     (:eval (propertize " ⏪ " 'local-map (make-mode-line-mouse-map 'mouse-1 '(lambda () (interactive) (helm-spotify-plus-previous) (setq spotify-modeline-title-offset 0) (update-current-spotify-data) ) )))
     (:eval (if (eq music-paused t)
		(propertize " ⏸ " 'local-map (make-mode-line-mouse-map 'mouse-1 '(lambda () (interactive) (helm-spotify-plus-toggle-play-pause) (setq music-paused nil)) ))
		(propertize " ⏵ " 'local-map (make-mode-line-mouse-map 'mouse-1 '(lambda () (interactive) (helm-spotify-plus-toggle-play-pause) (setq music-paused t)) ))
     ))
     (:eval (propertize " ⏩ " 'local-map (make-mode-line-mouse-map 'mouse-1 '(lambda () (interactive) (helm-spotify-plus-next) (setq spotify-modeline-title-offset 0) (update-current-spotify-data)) ) ))
     ;; (:propertize "   " nil nil)
     ;; (:eval (propertize " 🔍 " 'local-map (make-mode-line-mouse-map 'mouse-1 '(lambda () (interactive) (helm-spotify-plus) (update-current-spotify-data)) ) ))
     (:propertize "   |" nil nil)
     (:propertize spotify-modeline-title-display)
     (:propertize "| " nil nil)
     (:eval (propertize " ♩♩♩" 'local-map (make-mode-line-mouse-map 'mouse-1 '(lambda () (interactive)

     (setq song-title (string-trim (car (split-string (spotify-modeline-song) "-"))))

     (select-frame (make-frame `((name . ,(concat "[Spotify-Modeline] " (spotify-modeline-artist) " - " song-title)))))
     (lyrics (spotify-modeline-artist) song-title )
     (eww-browse-url (concat (spotify-modeline-artist) " Wikipedia"))

     ;; Set C-q to delete the frame and the re-map to original behavior
     (define-key (current-global-map) (kbd "C-q") '(lambda () (interactive)  (global-set-key (kbd "C-q") 'quoted-insert) (delete-frame)))
     ) ) ))
))






  ;; Helper function
  (defun shorten-directory (dir max-length)
    "Show up to `max-length' characters of a directory name `dir'."
    (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
	  (output ""))
      (when (and path (equal "" (car path)))
	(setq path (cdr path)))
      (while (and path (< (length output) (- max-length 4)))
	(setq output (concat (car path) "/" output))
	(setq path (cdr path)))
      (when path
	(setq output (concat ".../" output)))
      output))

  ;; Extra mode line faces
  (make-face 'mode-line-read-only-face)
  (make-face 'mode-line-modified-face)
  (make-face 'mode-line-folder-face)
  (make-face 'mode-line-filename-face)
  (make-face 'mode-line-position-face)
  (make-face 'mode-line-mode-face)
  (make-face 'mode-line-minor-mode-face)
  (make-face 'mode-line-process-face)
  (make-face 'mode-line-80col-face)

  (set-face-attribute 'mode-line nil
		      :foreground "gray50" :background "gray30"
		      :inverse-video nil
		      :box '(:line-width 6 :color "gray30" :style nil))
  (set-face-attribute 'mode-line-inactive nil
		      :foreground "gray80" :background "gray10"
		      :inverse-video nil
		      :box '(:line-width 6 :color "gray10" :style nil))

  (set-face-attribute 'mode-line-read-only-face nil
		      :inherit 'mode-line-face
		      :foreground "#4271ae"
		      :box '(:line-width 2 :color "#4271ae"))
  (set-face-attribute 'mode-line-modified-face nil
		      :inherit 'mode-line-face
		      :foreground "#c82829"
		      :background "#ffffff"
		      :box '(:line-width 2 :color "#c82829"))
  (set-face-attribute 'mode-line-folder-face nil
		      :inherit 'mode-line-face
		      :foreground "gray60")
  (set-face-attribute 'mode-line-filename-face nil
		      :inherit 'mode-line-face
		      :foreground "#eab700"
		      :weight 'bold)
  (set-face-attribute 'mode-line-position-face nil
		      :inherit 'mode-line-face
		      :height 100
		      :foreground "gray80")
  (set-face-attribute 'mode-line-mode-face nil
		      :inherit 'mode-line-face
		      :foreground "gray80")
  (set-face-attribute 'mode-line-minor-mode-face nil
		      :inherit 'mode-line-mode-face
		      :foreground "gray80"
		      :height 100)
  (set-face-attribute 'mode-line-process-face nil
		      :inherit 'mode-line-face
		      :foreground "#718c00")
  (set-face-attribute 'mode-line-80col-face nil
		      :inherit 'mode-line-position-face
		      :foreground "black" :background "#eab700")

  (provide 'mode-line-format)
