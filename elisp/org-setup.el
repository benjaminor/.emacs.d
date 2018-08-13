;;; org-setup -- setup org-mode

;;; Commentary:

;;; Code:

(use-package org
  :defer t
  :config
  (setq org-directory "~/org"
		org-default-notes-file (concat org-directory "/todo.org"))
  (setq org-agenda-files (directory-files "~/org/" t ".org$" t))
  (setq org-agenda-skip-unavailable-files t)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package org-projectile-helm)

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (push (org-projectile-project-todo-entry) org-capture-templates)
  (setq org-projectile-per-project-filepath "todo.org"
		org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
			(lambda ()
			  (org-bullets-mode t))))

(use-package htmlize)

(use-package org-fs-tree
  :quelpa (org-fs-tree :repo "ScriptDevil/org-fs-tree" :fetcher github))

(provide 'org-setup)
;;; org-setup.el ends here
