;;; org-setup -- setup org-mode

;;; Commentary:

;;; Code:

(use-package org
  :init
  (defun locate-user-org-file (FILENAME)
    "Return an absolute per-user org file name."
    (concat org-directory FILENAME))
  :config
  (setq org-directory "~/org/"
		org-default-notes-file (locate-user-org-file "notes.org"))
  (defvar org-default-projects-file (locate-user-org-file "projects.org"))
  (push '("pdf" . zathura) org-file-apps)
  (setq org-agenda-files `(,org-default-projects-file)
		org-agenda-skip-scheduled-if-done t
		org-agenda-skip-unavailable-files t
		org-agenda-skip-deadline-if-done t
		org-agenda-include-deadlines t
		org-startup-with-inline-images t
		org-agenda-block-separator nil
		org-agenda-compact-blocks t)

  ;; make html export more beautiful
  (setq org-html-head "<link rel=\"stylesheet\" href=\"https://orthen.net/sakura.css\" type=\"text/css\">")


  (setq org-log-done 'time)

  (use-package org-sidebar)

  (use-package org-noter)

  (use-package org-download
	:after org
	:bind
	(:map org-mode-map
          (("M-p" . org-download-screenshot)
           ("M-P" . org-download-yank))))

  (use-package org-ref
	:demand
	:config
	(require 'org-ref-latex)
	(require 'org-ref-pdf)
	(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

	(defvar my-bibliography-dir "~/bibliography/")
	(setq reftex-default-bibliography  (concat my-bibliography-dir "references.bib"))

	;; see org-ref for use of these variables
	(setq org-ref-bibliography-notes (concat org-directory "bib.org")
		  org-ref-default-bibliography (cons (concat my-bibliography-dir "references.bib") ())
		  org-ref-pdf-directory (concat my-bibliography-dir "bibtex-pdfs/"))
	:bind
	("C-c C-ö" . org-ref-bibtex-hydra/body))

  (use-package org-roam
	:hook
	(after-init . org-roam-mode)
	:custom
	(org-roam-directory (concat org-directory "org-roam/"))
	:bind (:map org-roam-mode-map
				(("C-c n l" . org-roam)
				 ("C-c n f" . org-roam-find-file)
				 ("C-c n b" . org-roam-switch-to-buffer)
				 ("C-c n g" . org-roam-graph-show))
				:map org-mode-map
				(("C-c n i" . org-roam-insert))))

  (use-package org-roam-server
    :after org-roam
    :config
    (setq org-roam-server-host "127.0.0.1"
		  org-roam-server-port 9090
		  org-roam-server-export-inline-images t
		  org-roam-server-authenticate nil
		  org-roam-server-network-poll t
		  org-roam-server-network-arrows nil
		  org-roam-server-network-label-truncate t
		  org-roam-server-network-label-truncate-length 60
		  org-roam-server-network-label-wrap-length 20))


  (use-package org-capture
	:ensure nil
	:config
	(require 'org-protocol)
	(setq org-capture-templates
		  '(("t" "Todo" entry (file org-default-notes-file)
			 "* TODO SCHEDULED: %T\n %?\n %i\n  %a")
			("a" "Urgent simple todo" entry (file org-default-notes-file)
			 "* TODO [#A] %?\n SCHEDULED: %T \n %i\n")
			("b" "Near-future simple todo" entry (file org-default-notes-file)
			 "* TODO [#B] %?\n SCHEDULED: %T \n %i\n")
			("c" "Long-term simple todo" entry (file org-default-notes-file)
			 "* TODO [#C] %?\n SCHEDULED: %T \n %i\n")
			("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
			 "* %?\nEntered on %U\n  %i\n  %a")))

	;; from advice in org-protocol
	(defun transform-square-brackets-to-round-ones(string-to-transform)
      "Transforms [ into ( and ] into ), other chars left unchanged."
      (concat
       (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
      )

	(push '("P" "Protocol" entry (file+headline "notes.org" "Inbox")
			"* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
		  org-capture-templates)
	(push '("L" "Protocol Link" entry (file+headline "notes.org" "Inbox")
			"* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]%(progn (setq kk/delete-frame-after-capture 2) \"\")\nCaptured On: %U")
		  org-capture-templates)

	(require 'org-roam-protocol))


  (defun org-archive-done-tasks ()
	(interactive)
	(org-map-entries
	 (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
	 "/DONE" 'file))

  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))


(use-package org-protocol-capture-html
  :quelpa (org-protocol-capture-html :fetcher github :repo "alphapapa/org-protocol-capture-html")
  :after org-protocol
  :config
  (push '("w" "Web site" entry (file "notes.org") "* %a :website:\n\n%U %?\n\n%:initial") org-capture-templates)
  )

(use-package org-projectile
  :defer 3
  :after (org projectile)
  :config
  (push (org-projectile-project-todo-entry) org-capture-templates)
  (setq org-projectile-projects-file (concat org-directory "projects.org"))
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package helm-org
  :after (helm org))

(use-package org-projectile-helm
  :after (helm-org org-projectile)
  :bind (("C-c n p" . org-projectile-helm-template-or-project)))


(use-package helm-org-rifle
  :bind ("C-c C-h" . helm-org-rifle-agenda-files))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  :hook (org-mode . (lambda ()
					  (org-bullets-mode t))))

(use-package org-fs-tree
  :after org
  :quelpa (org-fs-tree :repo "ScriptDevil/org-fs-tree" :fetcher github))

(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode 1)
  (setq org-super-agenda-groups
		'(;; Each group has an implicit boolean OR operator between its selectors.
		  (:name "Today"  ; Optionally specify section name
				 :time-grid t  ; Items that appear on the time grid
				 :todo "TODAY")  ; Items that have this TODO keyword
		  (:name "Important"
				 ;; Single arguments given alone
				 :tag "bills"
				 :priority "A")
		  ;; Set order of multiple groups at once
		  (:order-multi (2 (:name "Shopping in town"
								  ;; Boolean AND group matches items that match all subgroups
								  :and (:tag "shopping" :tag "@town"))
						   (:name "Food-related"
								  ;; Multiple args given in list with implicit OR
								  :tag ("food" "dinner"))
						   (:name "Personal"
								  :habit t
								  :tag "personal")
						   (:name "Space-related (non-moon-or-planet-related)"
								  ;; Regexps match case-insensitively on the entire entry
								  :and (:regexp ("space" "NASA")
												;; Boolean NOT also has implicit OR between selectors
												:not (:regexp "moon" :tag "planet")))))
		  ;; Groups supply their own section names when none are given
		  (:todo "WAITING" :order 8)  ; Set order of this section
		  (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
				 ;; Show this group at the end of the agenda (since it has the
				 ;; highest number). If you specified this group last, items
				 ;; with these todo keywords that e.g. have priority A would be
				 ;; displayed in that group instead, because items are grouped
				 ;; out in the order the groups are listed.
				 :order 9)
		  (:priority<= "C"
					   ;; Show this section after "Today" and "Important", because
					   ;; their order is unspecified, defaulting to 0. Sections
					   ;; are displayed lowest-number-first.
					   :order 1)
		  ;; After the last group, the agenda will display items that didn't
		  ;; match any of these groups, with the default order position of 99
		  )))


(provide 'org-setup)
;;; org-setup.el ends here
