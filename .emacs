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

       'f                     ; Modern API for working with files and directories
       'google-translate      ; translate in emacs
       'json-reformat         ; format JSON
       'js-comint             ; js interpreter
       'magit                 ; git integration
       'magit-popup
       'multiple-cursors      ; ST-like multiple cursors
       'nodejs-repl           ; nodeJS REPL
       'phi-search            ; replace isearch
       'rainbow-delimiters    ; rainbowy parens/braces/...
       'request
       'smex                  ; M-x
       'tern                  ; Tern-powered JavaScript integration
       'yasnippet             ; snippets for emacs
       'zlc                   ; zsh for emacs
       'browse-kill-ring      ; kill-ring history

       'use-package

       'zygospore
       'ws-butler
       'volatile-highlights
       'undo-tree
       'iedit
       'dtrt-indent
       'counsel-projectile
       'clean-aindent-mode
       'anzu

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
       'helm-gtags

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
   '("e39ff005e524c331b08d613109bff0b55fc21c64914c4a243faa70f330015389" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "6332c9756bde31cf9e34154395868413e45714488507527969f95a61b5f24518" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(merlin-eldoc reason-mode zerodark-theme arduino-mode atomic-chrome desktop-environment docker editorconfig eslint-fix haskell-mode lsp-haskell lsp-intellij lsp-java lsp-javacomp lsp-javascript-typescript lsp-mode lsp-rust lsp-ui nodemcu-mode wiki-summary lyrics helm-spotify helm-spotify-plus jetbrains jekyll-modes helm-smex rainbow-identifiers zlc ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package twittering-mode top-mode toml-mode tern-auto-complete sr-speedbar sos smex skewer-reload-stylesheets skewer-less rust-playground request rainbow-delimiters quelpa projectile popwin popup-switcher popup-kill-ring popup-imenu popup-complete phi-search persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file nyan-prompt nyan-mode nodejs-repl neotree multi-term mpg123 move-text markdown-mode magit macrostep lorem-ipsum livid-mode linum-relative link-hint json-mode js3-mode js2-refactor js-doc js-comint jade-mode isend-mode info+ indent-guide ido-vertical-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt helm-swoop helm-emmet helm-company helm-c-yasnippet helm-c-moccur handlebars-mode hackernews hacker-typer google-translate gh-md gh expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu esqlite eshell-z eshell-up eshell-prompt-extras eshell-git-prompt eshell-fringe-status eshell-did-you-mean eshell-autojump esh-help esh-buf-stack elscreen elisp-slime-nav dumb-jump dockerfile-mode dash-at-point company-web company-tern company-php company-go company-arduino column-enforce-mode coffee-mode clean-aindent-mode cargo browse-kill-ring auto-highlight-symbol auto-complete-c-headers auto-complete-auctex auto-compile angular-mode 2048-game))
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

(require 'dashboard)
(dashboard-setup-startup-hook)
(require 'dashboard-hackernews)
(setq dashboard-items '((hackernews . 10)))

(require 'elfeed)

(setq elfeed-feeds '(
("https://hnrss.org/frontpage" IT code hackernews)
("https://github.com/boehm-s.private.atom?token=AOYD4nnn-BKhwsVWNcKRhMZbeylZzUAoks66PLq8wA==" github)
("https://news.google.com/rss?hl=fr&gl=FR&ceid=FR:fr" news google-news)
("https://www.lesnumeriques.com/rss.xml" lesnumeriques hide)
))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "lesnumeriques\\.com"
			      :entry-link "test.html"
                              :add '(test)
			      :remove '(hide)))

(setq-default elfeed-search-filter "@1-week-ago +unread -hide")

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

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; company
(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; Package: projejctile
(use-package projectile
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

;; Package zygospore
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(require 'cc-mode)

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux")  ;; set style to "linux"

(use-package cc-mode
  :init
  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete))

(use-package ivy
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key (kbd "C-c s") 'swiper)))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-c r" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h l" . counsel-load-library)))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

(use-package helm
  :init
  (progn
    (require 'helm-config)
    (require 'helm-grep)
    ;; To fix error at compile:
    ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
    ;; (with-helm-buffer helm-echo-input-in-header-line)
    (if (version< "26.0.50" emacs-version)
        (eval-when-compile (require 'helm-lib)))

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-google-suggest-use-curl-p t
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          ;; helm-quick-update t ; do not display invisible candidates
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

          ;; you can customize helm-do-grep to execute ack-grep
          ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
          ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

          helm-echo-input-in-header-line t

          ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
          helm-buffer-skip-remote-checking t

          helm-mode-fuzzy-match t

          helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
          helm-org-headings-fontify t
          ;; helm-find-files-sort-directories t
          ;; ido-use-virtual-buffers t
          helm-semantic-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          ;; helm-apropos-fuzzy-match t
          helm-buffer-skip-remote-checking t
          helm-locate-fuzzy-match t
          helm-display-header-line nil)

    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-c r") 'helm-recentf)
    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h o") 'helm-occur)

    (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
    (global-set-key (kbd "C-c h g") 'helm-google-suggest)

    (global-set-key (kbd "C-c h x") 'helm-register)
    ;; (global-set-key (kbd "C-x r j") 'jump-to-register)

    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    ;; show minibuffer history with Helm
    (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
    (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

    (define-key global-map [remap find-tag] 'helm-etags-select)

    (define-key global-map [remap list-buffers] 'helm-buffers-list)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; PACKAGE: helm-swoop                ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Locate the helm-swoop folder to your path
    (use-package helm-swoop
      :bind (("C-c h o" . helm-swoop)
             ("C-c s" . helm-multi-swoop-all))
      :config
      ;; When doing isearch, hand the word over to helm-swoop
      (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

      ;; From helm-swoop to helm-multi-swoop-all
      (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t)

      ;; If this value is t, split window inside the current window
      (setq helm-swoop-split-with-multiple-windows t)

      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      (setq helm-swoop-split-direction 'split-window-vertically)

      ;; If nil, you can slightly boost invoke speed in exchange for text color
      (setq helm-swoop-speed-or-color t))

    (helm-mode 1)

    (use-package helm-projectile
      :init
      (helm-projectile-on)
      (setq projectile-completion-system 'helm)
      (setq projectile-indexing-method 'alien))))

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

(require 'cc-mode)
(require 'semantic)
(require 'stickyfunc-enhance)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-stickyfunc-mode 1)

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(setq-local eldoc-documentation-function #'ggtags-eldoc-function)


(semantic-mode 1)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)

;; GROUP: Editing -> Editing Basics
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 4)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

;; Package: volatile-highlights
;; GROUP: Editing -> Volatile Highlights
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))

;; Package: undo-tree
;; GROUP: Editing -> Undo -> Undo Tree
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))


;; Package: yasnippet
;; GROUP: Editing -> Yasnippet
;; Package: yasnippet
(use-package yasnippet
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode))

;; Package: clean-aindent-mode
(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; Package: dtrt-indent
(use-package dtrt-indent
  :init
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

;; Package: ws-butler
(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode))

;; PACKAGE: comment-dwim-2
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2))
  )

;; PACKAGE: anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
(use-package anzu
  :init
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

;; PACKAGE: iedit
(use-package iedit
  :bind (("C-;" . iedit-mode))
  :init
  (setq iedit-toggle-key-default nil))

;; Customized functions
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; add duplicate line function from Prelude
;; taken from prelude-core.el
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-o") 'open-line)

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
(define-key custom-keys-map (kbd "C-x m")  'mu4e)

;; helm
(define-key custom-keys-map (kbd "M-x") 'helm-smex)
(define-key custom-keys-map (kbd "C-x C-f") 'helm-projectile)

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

(setq org-support-shift-select t)

(my-keys-minor-mode 1)
(my-term-minor-mode -1)

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
(setq
  mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
  mu4e-update-interval 300)             ;; update every 5 minutes

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
