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
