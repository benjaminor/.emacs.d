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

(use-package dash)

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))

(use-package recentf
  :ensure nil
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
  :load-path "~/.emacs.d/lisp/aribas"
  :config
  (autoload 'run-aribas "aribas" "Run ARIBAS." t))

(use-package crux
  :bind
  ("C-c o" . crux-open-with)
  ("C-c u" . crux-view-url)
  ("C-c t" . crux-visit-term-buffer)
  ("C-c r" . crux-rename-file-and-buffer)
  ("C-c I" . crux-find-user-init-file)
  ("C-c S" . crux-find-shell-init-file)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c ." . crux-cleanup-buffer-or-region)
  ("C-c e" . crux-eval-and-replace)
  ("C-S-<return>" . crux-smart-open-line-above)
  ("S-<return>" . crux-smart-open-line)
  ("C-k" . crux-smart-kill-line)
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
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))


(use-package powerline
  :config
  (powerline-default-theme))

(use-package powerline-evil
  :after (powerline evil))

(use-package dumb-jump
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

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker
  :bind ("C-c d" . docker))

(use-package docker-compose-mode)

(use-package groovy-mode)

(use-package fish-mode)

(use-package paradox
  :config
  (paradox-enable))

(use-package package-utils)

(use-package rg
  :config
  (rg-enable-default-bindings))


(use-package direnv
  :config
  (direnv-mode))

(use-package google-this
  :config
  (google-this-mode 1)
  (global-set-key (kbd "C-c /") 'google-this-mode-submap))

(provide 'base-extensions)
;;; base-extensions.el ends here
