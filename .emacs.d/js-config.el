(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; tern autocomplete setup
(if (eq system-type 'windows-nt)
    (setq tern-command '("node" "<TERN LOCATION>\\bin\\tern")))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(add-hook 'js-mode-hook '(lambda () (setq-local company-backends '((company-web company-css company-tern :with company-yasnippet)))))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'tern-mode)
(add-hook 'js-mode-hook (lambda () (imenu-add-menubar-index) (hs-minor-mode t)))


;; Company conf for tab key

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-selection)
     (define-key company-active-map [tab] 'company-complete-selection)))
