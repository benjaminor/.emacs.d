;;; package -- base extensions
;;; Commentary:
;;; Code:

(use-package general
  :ensure t
  :defer t
  )

(use-package hydra
  :ensure t)

(use-package delight
  :ensure t)

;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
  :ensure t
  :defer t
  :config (progn
	    (defun my/ansi-colorize-buffer ()
	      (let ((buffer-read-only nil))
		(ansi-color-apply-on-region (point-min) (point-max))))
	    (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode))

(use-package ace-window
  :ensure t
  :defer t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (global-unset-key (kbd "C-x o"))
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    )
  :bind* ("M-p" . ace-window))

;; (use-package autopair
;;   :ensure t
;;   :config
;;   (autopair-global-mode)
;;   ;; (electric-pair-mode 1)
;;   )

(use-package ediff
  :ensure t
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

(use-package expand-region
  :defer t
  :bind
  ("C-=" . er/expand-region))

(use-package dash
  :ensure t)

(use-package helm
  :init
  (require 'helm-files)
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  :diminish helm-mode
  :defer 2
  :config
  (setq helm-split-window-inside-p          t
	helm-idle-delay                       0.0
	helm-input-idle-delay                 0.01
	helm-yas-display-key-on-candidate     t
	helm-quick-update                     t
	helm-move-to-line-cycle-in-source     t
	helm-ff-search-library-in-sexp        t
	helm-scroll-amount                    8
	helm-M-x-fuzzy-match                  t
	helm-ff-file-name-history-use-recentf t
	helm-split-window-default-side        'below
	helm-ff-skip-boring-files             t)
  (helm-adaptive-mode 1)
  (helm-mode 1)
  ;; (with-eval-after-load 'company
  ;;   (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;;   (define-key company-active-map (kbd "C-:") 'helm-company))


  ;;; does not work right now, how ot look over it later
  ;; TODO
  (defhydra hydra-helm-menu (:color pink
				    :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))

  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'spacemacs//hide-cursor-in-helm-buffer)

  (if (string-equal system-type "gnu/linux")
      (setq helm-grep-default-command
	    "grep --color=always -d skip %e -n%cH -e %p %f"
	    helm-grep-default-recurse-command
	    "grep --color=always -d recurse %e -n%cH -e %p %f"))

  :bind (("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 ("C-h a" . helm-apropos)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x c p" . helm-projectile-ag)
	 :map helm-map
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 ("C-j" . helm-next-line)
	 ("C-k" . helm-previous-line)
	 ("C-h" . helm-next-source)
	 ("C-S-h" . describe-key)
	 ("C-e" . hydra-helm-menu/body)
	 :map helm-find-files-map
	 ("C-l" . helm-execute-persistent-action)
	 ("C-h" . helm-find-files-up-one-level)
	 :map helm-read-file-map
	 ("C-l" . helm-execute-persistent-action)
	 ("C-h" . helm-find-files-up-one-level)))

(use-package helm-google
  :ensure t
  :config
  (global-set-key (kbd "C-h C--") 'helm-google))



;; == ag ==
;; Note that 'ag' (the silver searcher) needs to be installed.
;; Ubuntu: sudo apt-get install silversearcher-ag
;; OSX: brew install ag
(use-package ag
  :ensure t
  :defer t
  )
(use-package helm-ag
  :ensure t
  :defer t
  :after helm
					;TODO: define keybinding
  :config
  (general-define-key :keymaps 'helm-ag-map
		      "C-c C-e" 'helm-ag-edit)
  (bind-key "C-c C-e" 'helm-ag-edit helm-ag-mode-map)
  )


(use-package helm-git-grep)


(use-package helm-flycheck
  :after (helm flycheck)
  :config
  (define-key flycheck-mode-map (kbd "C-c ! l") nil)
  (define-key flycheck-mode-map (kbd "C-c ! l") 'helm-flycheck))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode)
  (use-package helm-projectile
    :ensure t
    :after helm
    :config
    (helm-projectile-on)))



(use-package helm-swoop
  :ensure t
  :config
  (progn
					; Change the keybinds to whatever you like :)
    (global-set-key (kbd "M-i") 'helm-swoop)
    (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

    ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
    (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)

    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)

    ;; If nil, you can slightly boost invoke speed in exchange for text color
    ;;    (setq helm-swoop-speed-or-color nil)

    ;; ;; Go to the opposite side of line from the end or beginning of line
    (setq helm-swoop-move-to-line-cycle t)

    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)

    ;; If you prefer fuzzy matching
    ;;    (setq helm-swoop-use-fuzzy-match t)

    ;; Disable pre-input
    (setq helm-swoop-pre-input-function
	  (lambda () ""))))

(use-package linum
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))

(use-package hlinum
  :config
  (hlinum-activate))



(use-package magit
  :config
  :defer t
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)

(use-package magithub
  :after magit
  :defer t
  :config
  (magithub-feature-autoinject t))


(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))


(use-package org
  :defer t
  :config
  (setq org-directory "~/org"
	org-default-notes-file (concat org-directory "/todo.org"))
  (setq org-agenda-files (directory-files "~/org/" t ".org$" t))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package org-projectile-helm)

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode t))))

(use-package page-break-lines
  :ensure t
  :defer t
  :config
  (global-page-break-lines-mode))

(use-package projectile
  :config
  (setq projectile-known-projects-file
	(expand-file-name "projectile-bookmarks.eld" temp-dir))

  (projectile-mode))

(use-package recentf
  :ensure t
  :defer 10
  :commands (recentf-mode
             recentf-add-file
	     recentf-apply-filename-handlers)
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

;; Use smartparens instead of autopair
(use-package smartparens
  :ensure t)
(use-package smartparens-config
  :ensure nil
  :after smartparens
  :config
  (add-hook 'after-init-hook
	    (lambda () (smartparens-global-mode))))
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;; (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
;; == undo-tree ==
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/")))))

(use-package which-key
  :defer t
  :config
  (which-key-mode))

(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode)
  :diminish ws-butler-mode
  )

(use-package try
  :ensure t
  :defer t)

(use-package tabbar
  :ensure t
  :config
  (tabbar-mode 1))

(use-package windmove
  :bind
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep)

(use-package beacon
  :ensure t
  :defer t
  :config
  (beacon-mode 1)
					; this color looks good for the zenburn theme but not for the one
					; I'm using for the videos
					; (setq beacon-color "#666600")
  )
					; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

					; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :disabled
  :defer t
  :config
  (global-hungry-delete-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARIBAS is an interactive interpreter for big integer arithmetic and multi-precision floating point arithmetic with a Pascal/Modula like syntax. ;;
;; https://www.mathematik.uni-muenchen.de/~forster/sw/aribas.html										   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package aribas
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/aribas"
  :config
  (autoload 'run-aribas "aribas" "Run ARIBAS." t))


(provide 'base-extensions)
;;; base-extensions.el ends here
