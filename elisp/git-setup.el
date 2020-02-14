;;; package -- git setup
;;; Commentary:
;;; Code:

(use-package git-timemachine)

(use-package magit
  :defer t
  :bind
  ;; Magic
  ("C-x g" . magit-status)
  :config
  (unbind-key "M-p" magit-section-mode-map))

(use-package magit-todos
  :config (magit-todos-mode))

(use-package magit-popup)

(use-package magithub
  :after magit
  :defer t
  :config
  (magithub-feature-autoinject t))

(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))

(provide 'git-setup)
;;; git-setup.el ends here
