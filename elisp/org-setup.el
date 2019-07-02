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

  ;;   (defhydra hydra-org-agenda-view (:hint none)
  ;;	"
  ;; _d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
  ;; _w_: ?w? week       _[_: inactive       _A_: arch-files
  ;; _t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
  ;; _m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
  ;; _y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
  ;;	("SPC" org-agenda-reset-view)
  ;;	("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ;;	("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ;;	("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ;;	("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ;;	("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ;;	("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ;;	("L" (org-agenda-log-mode '(4)))
  ;;	("c" (org-agenda-log-mode 'clockcheck))
  ;;	("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ;;	("a" org-agenda-archives-mode)
  ;;	("A" (org-agenda-archives-mode 'files))
  ;;	("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ;;	("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ;;	("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ;;	("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ;;	("!" org-agenda-toggle-deadlines)
  ;;	("[" (let ((org-agenda-include-inactive-timestamps t))
  ;;		   (org-agenda-check-type t 'timeline 'agenda)
  ;;		   (org-agenda-redo)
  ;;		   (message "Display now includes inactive timestamps as well")))
  ;;	("q" (message "Abort") :exit t)
  ;;	("v" nil))

  ;;   ;; Recommended binding:
  ;;   (define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)
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
(use-package org-protocol-capture-html
  :load-path "~/.emacs.d/lisp/org-protocol-capture-html"
  :after org-protocol)
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
