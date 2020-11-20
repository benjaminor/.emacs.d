;;; package -- git setup
;;; Commentary:
;;; Code:

(use-package git-timemachine)

(use-package magit
  :defer t
  :bind
  ;; Magic
  ("C-x g" . magit-status))

(use-package magit-todos
  :config (magit-todos-mode))

(use-package magit-popup)

(use-package forge
  :after magit)

(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))

(provide 'git-setup)
;;; git-setup.el ends here
