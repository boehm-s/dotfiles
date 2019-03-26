(deftheme steven-theme
  "Created 2019-03-13.")

(custom-theme-set-variables
 'steven-theme
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(org-super-agenda-groups '((:auto-group t)))
 '(org-bullets-bullet-list '("●" "▲" "■" "✶" "◉" "○" "○"))
 '(org-contacts-files '("~/Dropbox/org-steven/contacts.org"))
 '(custom-safe-themes '("51ba4e2db6df909499cd1d85b6be2e543a315b004c67d6f72e0b35b4eb1ef3de" default))
 '(org-agenda-files '("~/Dropbox/org-steven"))
 '(org-contacts-birthday-property "BORN")
 '(org-directory "~/Dropbox/org-steven"))

(custom-theme-set-faces
 'steven-theme
 '(custom-link ((t (:inherit (link))))))

(provide-theme 'steven-theme)
