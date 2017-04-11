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
    (read-string "vim command : ")))
  (when (string-match "^\:\\(.[[:digit:]]+\\)" str)
    (goto-line (string-to-number (match-string 1 str))))
  (when (string-match "^d\\([[:digit:]]+\\)" str)
    (dotimes (i (string-to-number (match-string 1 str))) (kill-line)))
  )
