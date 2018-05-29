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
  :ensure t
  :config
  (which-key-mode))

(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode)
  :diminish ws-butler-mode)

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
;;ARIBAS is an interactive interpreter for big integer arithmetic and multi-precision floating point arithmetic with a Pascal/Modula like syntax. ;;
;; https://www.mathematik.uni-muenchen.de/~forster/sw/aribas.html                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package aribas
  :ensure nil
  :load-path "~/.emacs.d/aribas"
  :config
  (autoload 'run-aribas "aribas" "Run ARIBAS." t))


(use-package crux
  :ensure t
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  :bind
  ("C-c o" . crux-open-with)
  ("C-c u" . crux-view-url)
  ("C-c t" . crux-visit-term-buffer)
  ("C-c r" . crux-rename-file-and-buffer)
  ("C-c k" . crux-kill-other-buffers)
  ("C-c I" . crux-find-user-init-file)
  ("C-c S" . crux-find-shell-init-file)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c n" . crux-cleanup-buffer-or-region))

;; TODO: complete bindings


(use-package helpful
  :ensure t
  :defer 5
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))


(use-package treemacs
  :ensure t
  :commands treemacs)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Add more functionality to dired
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-dired.el
(setq-default dired-dwim-target t)
(use-package diredfl
  :ensure t
  :defer 4
  :config
  (diredfl-global-mode))

;; tags for code navigation
(use-package ggtags
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))




(use-package dumb-jump
  :ensure t
  :config
  (global-set-key (kbd "C-M-p")
		  (defhydra dumb-jump-hydra (:color blue :columns 3 global-map )
		    "Dumb Jump"
		    ("j" dumb-jump-go "Go")
		    ("o" dumb-jump-go-other-window "Other (when )indow")
		    ("e" dumb-jump-go-prefer-external "Go external")
		    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
		    ("i" dumb-jump-go-prompt "Prompt")
		    ("l" dumb-jump-quick-look "Quick look")
		    ("b" dumb-jump-back "Back"))))

(use-package iedit
  :ensure t)


(provide 'base-extensions)
;;; base-extensions.el ends here
