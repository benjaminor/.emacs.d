;;; projectile-setup --- specify projectile and complementary packages and define keybindings
;;; Commentary:

;;; Code:

(use-package projectile
  :after hydra
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
	(setq helm-projectile-set-input-automatically nil))

  (use-package projectile-ripgrep
	:after (ripgrep helm-rg))


  (defhydra hydra-projectile (:color teal
									 :hint nil)
	"
	 PROJECTILE: %(projectile-project-root)

	 Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
  _r_: recent file         _s_: rg           _b_: switch to buffer    _x_: remove known project
  _d_: dir                 _m_: multi-occur                         _c_: cache clear
																^^^^_X_: cleanup non-existing
																^^^^_z_: cache current

"
	("s"   helm-projectile-rg)
	("b"   helm-projectile-switch-to-buffer)
	("c"   projectile-invalidate-cache)
	("d"   helm-projectile-find-dir)
	("f"   helm-projectile-find-file)
	("K"   projectile-kill-buffers)
	("m"   projectile-multi-occur)
	("p"   helm-projectile-switch-project)
	("r"   helm-projectile-recentf)
	("x"   projectile-remove-known-project)
	("X"   projectile-cleanup-known-projects)
	("z"   projectile-cache-current-file)
	("q"   nil "cancel" :color blue)
	("C-g" nil "cancel" :color blue))

  (define-key global-map (kbd "C-l") 'hydra-projectile/body))


(provide 'projectile-setup)
;;; projectile-setup.el ends here
