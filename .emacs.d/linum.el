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
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
