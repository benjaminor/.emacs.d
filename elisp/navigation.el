;;; package -- emacs navigation
;;; Commentary:
;;; Code:

;; buffer layout
(use-package treemacs
  :bind (("<f9>" . treemacs))
  :config
  (global-set-key (kbd "C-ü") (defhydra treemacs-hydra (:color red :hint nil)
								"Treemacs hydra"
								("b" treemacs-bookmark "Bookmark in treemacs")
								("f" treemacs-find-file "Current file in treemacs")
								("s" treemacs-select-window "Select treemacs window")
								("p" treemacs-projectile "Add a project from projectile to treemacs"))))

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-evil
  :after treemacs evil)
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-icons-dired
  :after treemacs
  :config
  (treemacs-icons-dired-mode))
(use-package treemacs-magit
  :after treemacs magit)

(use-package centaur-tabs
  :after evil
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-modified-marker t)
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-close-button nil)
  :bind
  (:map evil-normal-state-map
		("g t" . centaur-tabs-forward)
		("g T" . centaur-tabs-backward)))

(use-package zoom
  :defer
  :custom
  (zoom-mode t))


(use-package eyebrowse
  :disabled
  :config
  (eyebrowse-mode t))

;; Add more functionality to dired
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-dired.el
(setq-default dired-dwim-target t)
(use-package diredfl
  :defer 4
  :config
  (diredfl-global-mode))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))


(use-package windmove
  :ensure nil
  :bind
  ("C-c k" . windmove-up)
  ("C-c j" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package ace-window
  :init
  (global-set-key [remap other-window] 'ace-window)
  (global-unset-key (kbd "C-x o"))
  (custom-set-faces
   '(aw-leading-char-face
	 ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  :bind
  ("M-ö" . ace-window))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode))

(use-package discover-my-major
  :bind
  ("C-h C-m" . discover-my-major)
  ("C-h M-m" . discover-my-mode))


(provide 'navigation)
;;; navigation.el ends here
