;;; projectile setup
;;; Commentary:

;;; Code:

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-known-projects-file
	(expand-file-name "projectile-bookmarks.eld" temp-dir))

  (use-package helm-projectile
    :after helm
    :config
    (helm-projectile-on)))

(use-package ripgrep)

(use-package projectile-ripgrep
  :after (ripgrep helm-rg))

(provide 'projectile-setup)
;;; projectile-setup.el ends here
