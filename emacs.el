;; ;; package --- Summary
;;; Code:
;;; Commentary:

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; Header ETNA
(load-file "~/.emacs.d/std_comment.el")

;; Backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))


;; Duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

;; tabs ...
(setq backward-delete-char-untabify-method nil)
(setq-default tab-stop-list (number-sequence 4 200 4))
(electric-indent-mode)

(custom-set-faces
 '(whitespace-space
   ((((class color) (background dark)) (:background "red" :foreground "white"))
    (t (:inverse-video t)))))

;; linum mode conf
(setq column-number-mode t)
(global-linum-mode t)
(set-face-attribute 'linum nil :foreground "black")


(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
			     (count-lines (point-min) (point-max))))))
	 (format (concat "%" (number-to-string width) "d \u2502")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-relative-line-numbers)

(defun my-linum-relative-line-numbers (line-number)
  (let ((offset (- line-number my-linum-current-line-number)))
    (propertize (format my-linum-format-string offset) 'face 'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(defun vim-prompt (str)
  (interactive
   (list
    (read-string "vim command : ")))
  (when (string-match "^\:\\(.[[:digit:]]+\\)" str)
    (goto-line (string-to-number (match-string 1 str))))
  (when (string-match "^d\\([[:digit:]]+\\)" str)
    (dotimes (i (string-to-number (match-string 1 str))) (kill-line)))
  )

;; shortcuts

;; minor mode for overriding some major-mode keymap
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "M-z") 'vim-prompt)

    (define-key map (kbd "C-c C-SPC") 'comment-dwim)
    (global-set-key (kbd "C-d") 'duplicate-line)

    (define-key map (kbd "C-c RET") 'mc/edit-lines)
    (define-key map (kbd "C-c C-s") 'mc/mark-next-like-this-word)
    (define-key map (kbd "C-c C-r") 'mc/mark-previous-like-this-word)

    (define-key map (kbd "C-c C-<left>") 'hs-hide-all)
    (define-key map (kbd "C-c C-<right>") 'hs-show-all)
    (define-key map (kbd "C-c <left>") 'hs-hide-block)
    (define-key map (kbd "C-c <right>") 'hs-show-block)

    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")
(my-keys-minor-mode 1)


(defun  split-and-find-file-H ()
  "Split the window and open the find-file prompt"
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (let ((filename (read-file-name "Enter file name:")))
    (switch-to-buffer (find-file-noselect filename)))
  )

(defun  split-and-find-file-V ()
  "Split the window and open the find-file prompt"
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (let ((filename (read-file-name "Enter file name:")))
    (switch-to-buffer (find-file-noselect filename)))
  )

(global-set-key (kbd "C-x C-<right>") 'split-and-find-file-H)
(global-set-key (kbd "C-x C-<left>")  'split-and-find-file-H)
(global-set-key (kbd "C-x C-<up>")    'split-and-find-file-V)
(global-set-key (kbd "C-x C-<down>")  'split-and-find-file-V)

(global-set-key (kbd "C-x C-<right>") 'split-and-find-file-H)
(global-set-key (kbd "C-x C-<left>")  'split-and-find-file-H)
(global-set-key (kbd "C-x C-<up>")    'split-and-find-file-V)
(global-set-key (kbd "C-x C-<down>")  'split-and-find-file-V)

(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

(global-set-key (kbd "C-x C-x")  'delete-window)

(global-set-key (kbd "C-x C-m")  'neotree)


(add-to-list 'load-path "~/.emacs.d/elpa")

;; choose which package for which file type
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . (lambda() (markdown-mode) (setq-default indent-tabs-mode nil)) ))
(add-to-list 'auto-mode-alist '("\\.c\\'" . (lambda()(c++-mode)(whitespace-mode-conf-c))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . (lambda()(c++-mode)(whitespace-mode-conf-c))))
(add-to-list 'auto-mode-alist
	     '("\\.osql$" . (lambda ()
			      (sql-mode)
			      (sql-highlight-postgres-keywords))))



;; node as REPL
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
	;; We like nice colors
	(ansi-color-for-comint-mode-on)
	;; Deal with some prompt nonsense
	(add-to-list 'comint-preoutput-filter-functions
		     (lambda (output)
		       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
						 (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))

(require 'auto-complete)
					; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; start yasnippet with emacs
(require 'yasnippet)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; Alternatively use Control-c + tab
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

(yas-global-mode 1)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(if (eq system-type 'windows-nt)
    (setq tern-command '("node" "<TERN LOCATION>\\bin\\tern")))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))



;; configure web-mode
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

  )
;; ============================================================== ;;
;;                             add hook                           ;;
;; ============================================================== ;;


(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'tern-mode)
(add-hook 'js-mode-hook (lambda () (imenu-add-menubar-index) (hs-minor-mode t)))




(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(js-indent-level 2))
(provide '.emacs)
;;; .emacs ends here
(custom-set-faces)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
