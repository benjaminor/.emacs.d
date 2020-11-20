;;; projectile-setup --- specify projectile and complementary packages and define keybindings
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
	(helm-projectile-on)
	(setq helm-projectile-set-input-automatically t))

  (use-package projectile-ripgrep
	:after (rg helm-rg helm-projectile)
	:config
	;;TODO: add DIRECTORY arg here
	(defun projectile-ripgrep-no-input ()
	  "Execute projectile-ripgrep, but with empty input"
	  (interactive)
	  (let ((helm-projectile-set-input-automatically nil))
		(helm-projectile-rg)))))



(provide 'projectile-setup)
;;; projectile-setup.el ends here
