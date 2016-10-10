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

(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

(global-set-key (kbd "C-x C-x")  'delete-window)
       


(global-set-key (kbd "C-c C-c") 'mc/edit-lines)

(global-set-key (kbd "C-c C-s") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-c C-r") 'mc/mark-previous-like-this-word)


(add-to-list 'load-path "~/.emacs.d/elpa")
                                                                                                                                                                                                                                             
;; choose which package for which file type
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist
	     '("\\.osql$" . (lambda ()
			      (sql-mode)
			      (sql-highlight-postgres-keywords))))


(require 'auto-complete)
					; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(if (eq system-type 'windows-nt)
    (setq tern-command '("node" "<TERN LOCATION>\\bin\\tern")))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'tern-mode)



;; configure web-mode
(defun my-web-mode-hook ()
  "Web mode customization."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (set-face-attribute 'web-mode-doctype-face nil :foreground "#1affff")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#999999")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#4d4d4d")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#264d73")
  (set-face-attribute 'web-mode-html-attr-value-face nil :foreground "#336699")

  (set-face-attribute 'web-mode-function-call-face nil :foreground "#33d6ff")
  (set-face-attribute 'web-mode-function-name-face nil :foreground "#33d6ff")
  (setq web-mode-enable-css-colorization t)
  (set-face-attribute 'web-mode-css-at-rule-face nil :foreground "Pink3")


  (setq web-mode-enable-heredoc-fontification t)

  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
)
(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  )
