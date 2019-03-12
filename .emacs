(setq load-path
      (append
   (list
        (expand-file-name "~/.emacs.d/elpa"))
   load-path))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/"))
)

(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

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


(use-package windmove
  :ensure t
  :init
    (global-hl-line-mode 1)
    (set-face-foreground 'highlight nil)
)

(set-face-attribute 'region nil :background "SkyBlue3")

(use-package magit
  :ensure t)

(use-package helm
  :ensure t)

(use-package projectile
  :ensure t
  :defer 5
  :config
  (projectile-global-mode))

(use-package company
  :ensure t
  :bind (("C-<tab>" . company-complete))
  :config
  (global-company-mode)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))


(use-package helm-company
  :ensure t)

(use-package helm-swoop
  :ensure t
  :bind (("C-f" . helm-swoop)))

(use-package helm-smex
  :ensure t
  :bind (("M-x" . helm-smex)))

(use-package helm-projectile
  :ensure t)

(use-package helm-rg
  :ensure t
  :ensure-system-package rg
)


(use-package lsp-mode
  :ensure t
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package raml-mode
  :load-path "~/.emacs.d/raml-mode"
  :mode "\\.raml\\'")

;; helper funcftions
(defun lsp-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun lsp-js-hook nil
  (make-local-variable 'company-transformers)
  (push 'lsp-company-transformer company-transformers))

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'"
  :ensure-system-package (javascript-typescript-langserver . "sudo npm i -g javascript-typescript-langserver")
  :hook (lsp lsp-javascript-typescript-enable lsp-js-hook))

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json\\'")

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
        ("\\.twig\\'" . web-mode)))

(use-package pug-mode
  :ensure t
  :mode (("\\.pug\\'" . pug-mode)))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

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

(require 'cl)
(use-package windmove
  :ensure t
  :bind (("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)
   ))

(use-package multi-term
  :ensure t
  :bind (("<f12>" . (lambda() (interactive) (multi-term-dedicated-open) (other-window 1)))))

(global-set-key (kbd "C-x C-x")  'delete-window)

(defun  split-and-find-file (&optional HV)
   "Split the window and open the find-file prompt"
  (setq cmd (if (string= HV "V") 
                   'split-window-vertically
                   'split-window-horizontally))

  (lexical-let ((split-cmd cmd))
       #'(lambda ()
           (interactive)
           (funcall split-cmd)
           (other-window 1)
           (call-interactively (key-binding (kbd "C-x C-f")))
)))

(global-set-key (kbd "C-x C-<right>")  (split-and-find-file "H"))
(global-set-key (kbd "C-x C-<left>")  (split-and-find-file "H"))
(global-set-key (kbd "C-x C-<up>")  (split-and-find-file "V"))
(global-set-key (kbd "C-x C-<down>")  (split-and-find-file "V"))


(use-package multiple-cursors
  :ensure t
  :bind (("C-c RET" .  'mc/edit-lines)
         ("C-c C-s" .  'mc/mark-next-like-this-word)
         ("C-c C-r" .  'mc/mark-previous-like-this-word)
))

(global-set-key (kbd "C-S-c") 'xah-copy-to-register-1)
(global-set-key (kbd "C-S-v") 'xah-paste-from-register-1)

(global-set-key (kbd "C-d")  'duplicate-line-or-region)

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

(my-term-minor-mode 1)
))
(my-term-minor-mode -1)

(setq org-support-shift-select t)

(setq backup-directory-alist `(("." . "~/.backups-emacs")))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setenv "GDK_SCALE" "1")
(setenv "GDK_DPI_SCALE" "1")
(modify-all-frames-parameters '((inhibit-double-buffering . t)))

(setq date '(12 21 2017))


(use-package org
  :ensure org-plus-contrib
  :config
    (require 'org-inlinetask)
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (define-key global-map "\C-cc" 'org-capture)

    (custom-set-variables
      '(org-directory "~/org")
      '(org-agenda-files (list org-directory)))


    (setq org-log-done t)
    (setq org-confirm-elisp-link-function nil)

    (setq org-todo-keywords
      '((sequence "MAYBE(m)")
        (sequence "TODO(t)" "WIP(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)"))) 

    (setq org-todo-keyword-faces
      '(("MAYBE" . (:foreground "dodger blue" :weight bold))
        ("TODO" . (:foreground "red" :weight bold))
        ("WIP" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("CANCELED" . (:foreground "magenta" :weight bold))))

(setq org-capture-templates
  '(("a" "Appointment" entry (file  "~/org/gcal.org" ) "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
    ("l" "Link" entry (file+headline "~/org/links.org" "Links") "* %? %^L %^g \n%T" :prepend t)
    ("b" "Blog idea" entry (file+headline "~/org/todo.org" "Blog Topics:") "* %?\n%T" :prepend t)
    ("t" "To Do Item" entry (file+headline "~/org/todo.org" "To Do") "* TODO %?\n%u" :prepend t)
    ("n" "Note" entry (file+headline "~/org/todo.org" "Note space") "* %?\n%u" :prepend t)
    ("j" "Journal" entry (file+datetree "~/org/journal.org") "* %?\nEntered on %U\n  %i\n  %a")
    ("c" "Contacts" entry (file "~/org/contacts.org") "* %(org-contacts-template-name)\n\n:PROPERTIES:\n\n:EMAIL: %(org-contacts-template-email)\n\n")
    ("s" "Screencast" entry (file "~/org/screencastnotes.org") "* %?\n%i\n")))
)


;; Set Up org-projectile

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile-projects-file
          "~/org/projects.org")
;;    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)


;; Set Up org-contacts

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/org/contacts.org"))
  :config
    (custom-set-variables
      '(org-contacts-birthday-property "BORN")
      ;; '(org-contacts-address-property "CITY")
      ;; '(org-contacts-icon-property "PHOTOGRAPH")
    )

    
)

;; Set Up Google Calendar

'(org-agenda-include-diary t)

(use-package org-gcal
  :ensure t
  :config

  (setq package-check-signature nil)

  (setq org-gcal-client-id "174856972518-te2gkd7e9krp7tic68eeqsngbcihdshd.apps.googleusercontent.com"
      org-gcal-client-secret "Za7tXAXaybyHDVkdrAC3nrcS"
      org-gcal-file-alist '(("boehm_s@etna-alternance.net" .  "~/org/gcal.org")))

  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
)

(use-package calfw-org :ensure t)
(use-package calfw-ical :ensure t)

(use-package calfw
  :ensure t
  :config
    (require 'calfw)
    (require 'calfw-org)
    (setq cfw:org-overwrite-default-keybinding t)
    (require 'calfw-ical)
)

(use-package calfw-gcal 
  :ensure t
  :config
    (require 'calfw-gcal)
)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

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

(use-package cl
  :ensure t)
(use-package cc-mode
  :ensure t
  :init
  (require 'cc-mode)
  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete))

(use-package company-c-headers
  :ensure t
  :init
  (add-to-list 'company-backends 'company-c-headers))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

(setq c-default-style "linux") ;; set style to "linux"

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
  :ensure t
  :init
  (volatile-highlights-mode t))

;; Package: undo-tree
;; GROUP: Editing -> Undo -> Undo Tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))

;; Package: yasnippet
;; GROUP: Editing -> Yasnippet
;; Package: yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (yas-global-mode 1)
)

(use-package yasnippet-snippets 
  :ensure t)

;; Package: clean-aindent-mode
(use-package clean-aindent-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; Package: dtrt-indent
(use-package dtrt-indent
  :ensure t
  :init
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

;; Package: ws-butler
(use-package ws-butler
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode))

;; PACKAGE: comment-dwim-2
(use-package comment-dwim-2
  :ensure t
  :bind (("M-;" . comment-dwim-2))
  )

;; PACKAGE: anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
(use-package anzu
  :ensure t
  :init
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

;; PACKAGE: iedit
(use-package iedit
  :ensure t
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

(use-package ggtags
  :ensure t
  :init 
    (require 'ggtags)
    (add-hook 'c-mode-common-hook (lambda ()
      (when (derived-mode-p 
        'c-mode 
        'c++-mode 
        'java-mode 
        'asm-mode)
      (ggtags-mode 1))))
    (dolist (map (list ggtags-mode-map dired-mode-map))
      (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
      (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
      (define-key map (kbd "C-c g r") 'ggtags-find-reference)
      (define-key map (kbd "C-c g f") 'ggtags-find-file)
      (define-key map (kbd "C-c g c") 'ggtags-create-tags)
      (define-key map (kbd "C-c g u") 'ggtags-update-tags)
      (define-key map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key map (kbd "M-.") 'ggtags-find-tag-dwim)
      (define-key map (kbd "M-,") 'pop-tag-mark)
      (define-key map (kbd "C-c <") 'ggtags-prev-mark)
      (define-key map (kbd "C-c >") 'ggtags-next-mark))
)

(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :ensure t
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

(use-package elfeed
  :ensure t
  :init
    (setq elfeed-feeds '(
      ("https://hnrss.org/frontpage" IT code hackernews)
      ("https://futurism.com/feed" IT futurism)
      ("https://github.com/boehm-s.private.atom?token=AOYD4nnn-BKhwsVWNcKRhMZbeylZzUAoks66PLq8wA==" github)
      ("https://news.google.com/rss?hl=fr&gl=FR&ceid=FR:fr" news google-news)
      ("https://www.lesnumeriques.com/rss.xml" lesnumeriques hide)
    ))
    (setq-default elfeed-search-filter "@1-week-ago +unread -hide")
  :config
    (add-hook 'elfeed-new-entry-hook
      (elfeed-make-tagger :feed-url "lesnumeriques\\.com"
			              :entry-link "test.html"
                          :add '(test)
			              :remove '(hide)))
)

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :ensure-system-package offlineimap
  :ensure-system-package mu
  :init
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
)

(use-package smtpmail
  :ensure t
  :init 
    (setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t ))

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

(use-package helm-spotify-plus
  :ensure t)

(use-package lyrics
  :ensure t)

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
