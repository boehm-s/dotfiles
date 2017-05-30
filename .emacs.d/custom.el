;; more likely to be in an init.el file

;; start yasnippet with emacs
;; (require 'yasnippet)

;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; ;; Set Yasnippet's key binding to shift+tab
;; (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; ;; Alternatively use Control-c + tab
;; (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

;; (yas-global-mode 1)


;;__________________________________________________________________________

;; highlight current line
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)


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

;; company-mode settings

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

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
