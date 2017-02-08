;; more likely to be in an init.el file

(require 'auto-complete)					; do default config for auto-complete
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(inhibit-startup-screen t)
 '(js-indent-level 2))


(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(if (eq system-type 'windows-nt)
    (setq tern-command '("node" "<TERN LOCATION>\\bin\\tern")))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;;__________________________________________________________________________

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

(defun vim-prompt (str)
  (interactive
   (list
    (read-string "vim command : ")))
  (when (string-match "^\:\\(.[[:digit:]]+\\)" str)
    (goto-line (string-to-number (match-string 1 str))))
  (when (string-match "^d\\([[:digit:]]+\\)" str)
    (dotimes (i (string-to-number (match-string 1 str))) (kill-line)))
  )

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
