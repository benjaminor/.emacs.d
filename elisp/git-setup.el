;;; package -- git setup
;;; Commentary:
;;; Code:

(use-package git-timemachine)

(use-package magit
  :bind
  ;; Magic
  ("C-x g" . magit-status))

(use-package magit-todos
  :after magit)

(use-package forge
  :after magit)

(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))

(provide 'git-setup)
;;; git-setup.el ends here
