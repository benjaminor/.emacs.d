;;; org-setup - setup org-mode


(use-package org
  :defer t
  :config
  (setq org-directory "~/org"
		org-default-notes-file (concat org-directory "/todo.org"))
  (setq org-agenda-files (directory-files "~/org/" t ".org$" t))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package org-projectile-helm)

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
		org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
			(lambda ()
			  (org-bullets-mode t))))

(use-package htmlize)

(provide 'org-setup)
;; org-setup.et ends here
