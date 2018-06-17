;;; package -- base extensions
;;; Commentary:
;;; Code:

(use-package general
  :defer t
  )

(use-package hydra)

(use-package delight)

;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
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



(use-package dash)


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



(use-package page-break-lines
  :defer t
  :config
  (global-page-break-lines-mode))

(use-package projectile
  :config
  (setq projectile-known-projects-file
		(expand-file-name "projectile-bookmarks.eld" temp-dir))

  (projectile-mode))

(use-package recentf
  :defer 10
  :commands (recentf-mode
			 recentf-add-file
			 recentf-apply-filename-handlers)
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

;; Use smartparens instead of autopair
(use-package smartparens)
(use-package smartparens-config
  :after smartparens
  :config
  (add-hook 'after-init-hook
			(lambda () (smartparens-global-mode))))
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;; (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
;; == undo-tree ==
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/")))))

(use-package which-key
  :config
  (which-key-mode))

(use-package ws-butler
  :init
  (ws-butler-global-mode)
  :diminish ws-butler-mode)

(use-package try
  :defer t)

(use-package tabbar
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
  :defer t
  :config
  (beacon-mode 1)
										; this color looks good for the zenburn theme but not for the one
										; I'm using for the videos
										; (setq beacon-color "#666600")
  )

; expand the marked region in semantic increments (negative prefix to reduce region)
; deletes all the whitespace when you hit backspace or delete
(use-package expand-region
  :defer 15
  :bind
  ("C-=" . er/expand-region))
										
(use-package hungry-delete
  :disabled
  :defer t
  :config
  (global-hungry-delete-mode 1))

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ARIBAS is an interactive interpreter for big integer arithmetic and multi-precision floating point arithmetic with a Pascal/Modula like syntax. ;;
;; https://www.mathematik.uni-muenchen.de/~forster/sw/aribas.html                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aribas
  :load-path "~/.emacs.d/lisp/aribas"
  :config
  (autoload 'run-aribas "aribas" "Run ARIBAS." t))



(use-package crux
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
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))


(use-package treemacs
  :commands treemacs)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Add more functionality to dired
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-dired.el
(setq-default dired-dwim-target t)
(use-package diredfl
  :defer 4
  :config
  (diredfl-global-mode))

;; tags for code navigation
(use-package ggtags
  :defer t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))




(use-package dumb-jump
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

(use-package iedit)


(provide 'base-extensions)
;;; base-extensions.el ends here
