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

(use-package all-the-icons
  :ensure t)



(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :ensure t)


(use-package flycheck
  :ensure t
  :init
    (global-flycheck-mode t))

(use-package dash
  :ensure t
  :config
  (require 'dash)
)

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
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))


(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode)
)

(use-package company-box
  :ensure t
  :custom (company-box-icons-alist 'company-box-icons-all-the-icons)
  :hook (company-mode . company-box-mode)
)

(use-package helm-company
  :ensure t)

(use-package helm-swoop
  :ensure t
  :bind (("C-f" . helm-swoop)))

(use-package helm-smex
  :ensure t
  :bind (("M-x" . helm-smex)))

(use-package helm-rg
  :ensure t
  :ensure-system-package rg
)

(use-package helm-projectile
  :ensure t
  :bind (("C-x C-f" . helm-projectile)
          ("C-x r g" . helm-projectile-rg)
          ("C-x C-p" . helm-projectile-switch-project))
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
  :bind (("C-. RET" .  'mc/edit-lines)
         ("C-. C-s" .  'mc/mark-next-like-this-word)
         ("C-. C-r" .  'mc/mark-previous-like-this-word)
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

(use-package flymake-diagnostic-at-point
  :after flymake
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-error-prefix " ")
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package elpy
  :ensure t
  :config
    (setq elpy-rpc-python-command "python3")
    (elpy-enable))


(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) 

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  :config
    (conda-env-initialize-interactive-shells)
    (conda-env-autoactivate-mode t))


(use-package pyvenv
  :ensure t
  :config
    (setenv "WORKON_HOME" "/home/boehm_s/anaconda3")
    (pyvenv-mode 1))

;; weird trick 
(setq date '(12 21 2017))
(use-package org-indent :ensure nil :after org :delight)

(use-package org
  :ensure org-plus-contrib
  :custom
    (org-directory "~/Dropbox/org-steven")
    (org-agenda-files (list org-directory))
  :config
    (require 'org-inlinetask)
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (define-key global-map "\C-c\C-a" 'cfw:open-org-calendar)
    (define-key global-map "\C-cc" 'org-capture)

    (setq org-log-done t)
    (setq org-confirm-elisp-link-function nil)

    (setq org-todo-keywords
      '((sequence "TO_READ" "GLIMPSED" "DONE" "RECOMMEND"  "|" "BORED" "TOO_HARD")
        (sequence "TODO(t)" "WIP(w)" "|" "DONE(d)")
    ))


    (setq org-todo-keyword-faces '(
        ("TO_READ"   . "Magenta") 
        ("GLIMPSED"  . "DarkGoldenrod") 
        ("DONE"      . "LimeGreen")
        ("RECOMMEND" . "MediumOrchid") 
        ("BORED"     . "red") 
        ("TOO_HARD"  . "red")
        
        ("MAYBE"    . "dodger blue")
        ("TODO"     . "red")
        ("WIP"      . "orange")
        ("DONE"     . "LimeGreen")
        ("CANCELED" . "magenta" )
     ))

(setq org-capture-templates
  '(("a" "Appointment" entry (file  "~/Dropbox/org-steven/gcal.org" ) "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
    ("l" "Link" entry (file+headline "~/Dropbox/org-steven/links.org" "Links") "* %? %^L %^g \n%T" :prepend t)
    ("b" "Blog idea" entry (file+headline "~/Dropbox/org-steven/todo.org" "Blog Topics:") "* %?\n%T" :prepend t)
    ("t" "To Do Item" entry (file+headline "~/Dropbox/org-steven/todo.org" "To Do") "* TODO %?\n%u" :prepend t)
    ("n" "Note" entry (file+headline "~/Dropbox/org-steven/todo.org" "Note space") "* %?\n%u" :prepend t)
    ("j" "Journal" entry (file+datetree "~/Dropbox/org-steven/journal.org") "* %?\nEntered on %U\n  %i\n  %a")
    ("c" "Contacts" entry (file "~/Dropbox/org-steven/contacts.org") "* %(org-contacts-template-name)\n\n:PROPERTIES:\n\n:EMAIL: %(org-contacts-template-email)\n\n")
    ("s" "Screencast" entry (file "~/Dropbox/org-steven/screencastnotes.org") "* %?\n%i\n")))
)

(use-package org-super-agenda
  :ensure t
  :custom (org-super-agenda-groups '((:auto-group t)) (org-agenda-list)))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("●" "▲" "■" "✶" "◉" "○" "○")))


(add-to-list 'org-modules 'org-habit t)
(add-to-list 'org-modules 'org-checklist t)

;; Set Up org-projectile

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile-projects-file
          "~/Dropbox/org-steven/projects.org")
;;    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)


;; Set Up org-contacts

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Dropbox/org-steven/contacts.org"))
  :custom (org-contacts-birthday-property "BORN")
  :config
)


;; checkbox validate parent


(defun my/org-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
  (let ((todo-state (org-get-todo-state)) beg end)
    (unless (not todo-state)
      (save-excursion
    (org-back-to-heading t)
    (setq beg (point))
    (end-of-line)
    (setq end (point))
    (goto-char beg)
    (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                   end t)
        (if (match-end 1)
        (if (equal (match-string 1) "100%")
            (unless (string-equal todo-state "DONE")
              (org-todo 'done))
          (unless (string-equal todo-state "TODO")
            (org-todo 'todo)))
          (if (and (> (match-end 2) (match-beginning 2))
               (equal (match-string 2) (match-string 3)))
          (unless (string-equal todo-state "DONE")
            (org-todo 'done))
        (unless (string-equal todo-state "TODO")
          (org-todo 'todo)))))))))

(add-hook 'org-checkbox-statistics-hook 'my/org-checkbox-todo)

;; Set Up Google Calendar

'(org-agenda-include-diary t)

(use-package org-gcal
  :ensure t
  :config

  (setq package-check-signature nil)

  (setq org-gcal-client-id "174856972518-te2gkd7e9krp7tic68eeqsngbcihdshd.apps.googleusercontent.com"
      org-gcal-client-secret "Za7tXAXaybyHDVkdrAC3nrcS"
      org-gcal-file-alist '(("boehm_s@etna-alternance.net" .  "~/Dropbox/org-steven/gcal.org")))

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

(use-package helm-spotify-plus :ensure t)
    (use-package lyrics :ensure t)

  (defvar sp-dbus-get "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' ")
  (defvar sp-paused-bashstr (concat sp-dbus-get "string:'PlaybackStatus' | tail -n1 | cut -d\\\" -f2"))
  (defvar sp-metadata-bashstr  (concat "metadata=$(" sp-dbus-get  " string:'Metadata');"))
  (defvar sp-artist-bashstr  "artist=$(echo \"$metadata\" | grep -A2 albumArtist | tail -n1 | cut -d\\\" -f2);")
  (defvar sp-song-bashstr  "song=$(echo \"$metadata\" | grep -A1 title | tail -n1 | cut -d\\\" -f2);")

  (defun sp-bash-metadata-echo (arg1 arg2)
    (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat sp-metadata-bashstr arg1 arg2))))

  (defun spotify-artist ()
    (sp-bash-metadata-echo sp-artist-bashstr "echo \"$artist\""))

  (defun spotify-song (&optional trimmed)
    (or trimmed (setq trimmed nil))
    (setq song (sp-bash-metadata-echo sp-song-bashstr "echo \"$song\""))
    (if trimmed
      (string-trim (car (split-string song "-")))
      song))

  (defun spotify-current ()
    (format "[%s]   %s" (spotify-artist) (spotify-song)))

  (defun get-spotify-text (txt offset &optional size)
    (unless size (setq size 25))
    (setq blank        (make-string size ? )
          scroll-txt   (concat blank txt blank)
          max-offset   (+ size (length txt))
          offset       (mod offset max-offset))
          (substring scroll-txt offset (+ offset size)))

  (defvar spotify-playing (string-match-p
    "Playing" 
    (shell-command-to-string sp-paused-bashstr)))
  (defvar spotify-text-counter 0)
  (defvar spotify-text-display "")

  (defun spotify-update-data ()
    (setq spotify-text-counter (+ spotify-text-counter 1))
    (setq spotify-playing (string-match-p "Playing" (shell-command-to-string sp-paused-bashstr)))
    (force-mode-line-update t))


   (defun spotify-music-details ()
     (interactive)
     (setq song-title  (spotify-song t)
           song-artist (spotify-artist)
           frame-name  (concat "[Spotify-Modeline] " song-artist " - " song-title))

   	 (select-frame (make-frame `((name . ,frame-name))))
   	 (lyrics song-artist song-title)
     (eww-browse-url song-artist)
;;     (helm-google-google (mapconcat 'identity (split-string (song-artist) " ") "+" ))
	 
     ;; Use C-q to exit and re-bind to it's original fn
     (define-key (current-global-map) (kbd "C-q") 
       '(lambda () (interactive)  
         (global-set-key (kbd "C-q") 'quoted-insert) 
         (delete-frame))
     ))


   (run-with-timer 0 0.2 'spotify-update-data)

(use-package spaceline :ensure t)

  (use-package spaceline-config 
    :ensure spaceline
    :config
      (spaceline-helm-mode 1)

      (require 'spaceline-all-the-icons)
      (require 'helm-spotify-plus)

      (setq-default
        powerline-height 24
        powerline-default-separator 'slant) 

      (spaceline-define-segment my/spotify-song
        "spotify-current music playing"
        (get-spotify-text (spotify-current) spotify-text-counter))

      (spaceline-define-segment my/spotify-controls
        (list 
          (propertize "⏪ " 'local-map (make-mode-line-mouse-map 'mouse-1 
            '(lambda () (interactive) (helm-spotify-plus-previous))))

          (propertize (if spotify-playing "⏸" "⏵") 'local-map (make-mode-line-mouse-map 'mouse-1 
            '(lambda () (interactive) (helm-spotify-plus-toggle-play-pause) (setq spotify-playing (not spotify-playing)))))

          (propertize " ⏩" 'local-map (make-mode-line-mouse-map 'mouse-1 
            '(lambda () (interactive) (helm-spotify-plus-next))))
       ))

      (spaceline-define-segment my/spotify-details
        (propertize "♩♩♩" 'local-map (make-mode-line-mouse-map 'mouse-1 'spotify-music-details)))


     (spaceline-compile 'main 
       '(((persp-name
         workspace-number
         window-number)
           :fallback evil-state
           :face highlight-face
           :priority 100)
         (anzu :priority 95)
         auto-compile
         ((buffer-modified buffer-size buffer-id remote-host)
           :priority 98)
         (major-mode :priority 79)
         (process :when active)
         ((flycheck-error flycheck-warning flycheck-info)
           :when active
           :priority 89)
         (erc-track :when active)
         ((all-the-icons-vc-icon all-the-icons-vc-status) :priority 90)
         ;; (version-control :when active
         ;;   :priority 78)
         (org-pomodoro :when active)
         (org-clock :when active))

       ; right side
       '(which-function
         (my/spotify-details :priority 99)
         (my/spotify-song :priority 99)
         (my/spotify-controls :priority 99)
         (python-pyvenv :fallback python-pyenv)
         (purpose :priority 94)
         (battery :when active)
         (selection-info :priority 95)
         input-method
         ((buffer-encoding-abbrev
         point-position
         line-column)
           :separator " | "
           :priority 96)
         (global :when active)
         (buffer-position :priority 99)
         (hud :priority 99)))

      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
)
