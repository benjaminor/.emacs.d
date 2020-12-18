;;; package -- base extensions
;;; Commentary:
;;; Code:

(use-package general
  :defer t)

(use-package hydra
  :preface
  (defun me/make-hydra-heading (&rest headings)
	"Format HEADINGS to look pretty in a hydra docstring."
	(mapconcat (lambda (it)
				 (propertize (format "%-20s" it) 'face 'font-lock-doc-face))
			   headings
			   nil))
  :bind
  ("<f2>" . hydra-zoom/body)
  ("C-ä" . hydra-misc-helper/body)
  ("C-c f" . hydra-flycheck/body))

(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))


(defhydra hydra-misc-helper (:exit t :color pink
								   :hint nil
								   :columns 5)
  "
^Emacs^                      ^Buffer^                          ^Org^
^^^^^^^^------------------------------------------------------------------------------
_u_: upgrade all packages  _e_: eval-buffer                   _o_: rg through org-dir
_r_: rg in emacs config    _h_: helm-rg for buffer and below

"
  ("u" package-utils-upgrade-all)
  ("e" eval-buffer)
  ("h" helm-rg)
  ("r" my/rg-through-emacs-config)
  ("o" my/rg-through-org-directory)
  ("q" nil "quit" :color blue)
  ("C-g" nil "quit" :color blue))


(defhydra hydra-flycheck (:color blue)
  (concat "\n " (me/make-hydra-heading "Flycheck" "Errors" "Checker")
		  "
 _q_ quit              _j_ previous          _?_ describe
 _m_ manual            _k_ next              _d_ disable
 _v_ verify setup      _f_ check             _s_ select
 ^^                    _l_ list              ^^
")
  ("q" nil)
  ("j" flycheck-previous-error :color red)
  ("k" flycheck-next-error :color red)
  ("?" flycheck-describe-checker)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-manual)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup))

(defhydra hydra-projectile (:color teal
								   :hint nil)
  "
	 PROJECTILE: %(projectile-project-root)

	 Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
  _r_: recent file         _s_: rg           _b_: switch to buffer    _x_: remove known project
  _d_: dir                 _m_: multi-occur                         _c_: cache clear
						 _l_: proj-rg-no-in
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
  ("l"   projectile-ripgrep-no-input)
  ("q"   nil "cancel" :color blue)
  ("C-g" nil "cancel" :color blue))

(define-key global-map (kbd "C-l") 'hydra-projectile/body)

(defhydra hydra-spotify (:hint nil)
  "
^Search^                  ^Control^               ^Manage^
^^^^^^^^-----------------------------------------------------------------
_t_: Track               _SPC_: Play/Pause        _+_: Volume up
_m_: My Playlists        _n_  : Next Track        _-_: Volume down
_f_: Featured Playlists  _p_  : Previous Track    _x_: Mute
_u_: User Playlists      _r_  : Repeat            _d_: Device
^^                       _s_  : Shuffle           _q_: Quit
"
  ("t" spotify-track-search :exit t)
  ("m" spotify-my-playlists :exit t)
  ("f" spotify-featured-playlists :exit t)
  ("u" spotify-user-playlists :exit t)
  ("SPC" spotify-toggle-play :exit nil)
  ("n" spotify-next-track :exit nil)
  ("p" spotify-previous-track :exit nil)
  ("r" spotify-toggle-repeat :exit nil)
  ("s" spotify-toggle-shuffle :exit nil)
  ("+" spotify-volume-up :exit nil)
  ("-" spotify-volume-down :exit nil)
  ("x" spotify-volume-mute-unmute :exit nil)
  ("d" spotify-select-device :exit nil)
  ("q" quit-window "quit" :color blue))

(define-key global-map (kbd "C-c .") 'hydra-spotify/body)


(use-package delight)

;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
  :ensure nil
  :defer t
  :config
  (defun my/ansi-colorize-buffer ()
	(let ((buffer-read-only nil))
	  (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer))


(use-package ediff
  :ensure nil
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  ;; Add GOPATH to shell
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-initialize))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))

(use-package recentf
  :ensure nil
  :commands (recentf-mode
			 recentf-add-file
			 recentf-apply-filename-handlers)
  :config
  (setq recentf-save-file (recentf-expand-file-name (locate-user-emacs-file "private/cache/recentf")))
  (recentf-mode 1))

;; Use smartparens instead of autopair
(use-package smartparens)
(use-package smartparens-config
  :ensure smartparens
  :after smartparens
  :config
  (smartparens-global-mode)
  (setq sp-autoescape-string-quote nil)
  (setq sp-escape-quotes-after-insert nil))

;; == undo-tree ==
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history nil))

(use-package which-key
  :config
  (which-key-mode))

(use-package try
  :defer t)

(use-package wgrep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ARIBAS is an interactive interpreter for big integer arithmetic and multi-precision floating point arithmetic with a Pascal/Modula like syntax. ;;
;; https://www.mathematik.uni-muenchen.de/~forster/sw/aribas.html                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aribas
  :load-path "lisp/aribas"
  :config
  (autoload 'run-aribas "aribas" "Run ARIBAS." t))

(use-package crux
  :bind
  ("C-c o" . crux-open-with)
  ("C-c t" . crux-visit-term-buffer)
  ("C-c r" . crux-rename-file-and-buffer)
  ("C-c I" . crux-find-user-init-file)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c ü" . crux-cleanup-buffer-or-region)
  ("C-c e" . crux-eval-and-replace)
  :config
  (crux-reopen-as-root-mode)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region))


(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-f") #'helpful-function)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))


(use-package dumb-jump
  :disabled ;; I don't use this (atm only making use of lsp)
  :config
  (global-set-key (kbd "C-M-p")
				  (defhydra dumb-jump-hydra (:color blue :columns 3)
					"Dumb Jump"
					("j" dumb-jump-go "Go")
					("o" dumb-jump-go-other-window "Other (when )indow")
					("e" dumb-jump-go-prefer-external "Go external")
					("x" dumb-jump-go-prefer-external-other-window "Go external other window")
					("i" dumb-jump-go-prompt "Prompt")
					("l" dumb-jump-quick-look "Quick look")
					("b" dumb-jump-back "Back"))))

(use-package iedit)

(use-package docker
  :bind ("C-c d" . docker))

;; modern emacs package menu
(use-package paradox
  :config
  (paradox-enable))

;; improved emacs controls for managing packages
(use-package package-utils)

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package direnv
  :config
  (direnv-mode))

(use-package google-this
  :init
  (setq google-this-keybind (kbd "C-c g"))
  :config
  (google-this-mode 1))

(use-package proced
  :ensure nil
  :custom
  (proced-auto-update-interval 1)
  :hook
  (proced-mode . (lambda () (proced-toggle-auto-update 1))))

(use-package vterm)

(use-package matrix-client
  :quelpa (matrix-client :fetcher github :repo "alphapapa/matrix-client.el"
						 :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))

(use-package oauth2)
(use-package spotify
  :after oauth2
  :defer 5
  :quelpa (spotify.el :fetcher github :repo "danielfm/spotify.el"))


(use-package htmlize
  :defer)

(provide 'base-extensions)
;;; base-extensions.el ends here
