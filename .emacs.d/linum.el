;; linum mode conf
(setq column-number-mode t)
(linum-relative-global-mode t)

(string-to-number (format-mode-line "%l"))
(setq linum-relative-current-symbol "->")
(setq linum-relative-format "%3s \u2502")
(set-face-attribute 'linum nil :foreground "magenta")

(defadvice linum-update (around my-linum-update)
  (setq linum-relative-current-symbol (number-to-string (current-column)))
  ad-do-it)

(ad-activate 'linum-update)
