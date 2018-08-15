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

(use-package org-protocol
  :ensure nil
  :config
  (setq org-capture-templates

		'(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
		   "* TODO %?\n  %i\n  %a")
		  ("w" "Web site" entry
		   (file "")
		   "* %a :website:\n\n%U %?\n\n%:initial")))
  :bind ("C-c c" . org-capture))
;; (use-package org-protocol-capture-html
;;   :after org-protocol)
(use-package org-projectile-helm)

(use-package org-projectile
  :after (org projectile)
  :config
  (org-projectile-per-project)
  ;; (push (org-projectile-project-todo-entry) org-capture-templates)
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
