;;; package --- summary
;;; setup for evil-mode

;;; Commentary:

;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (add-to-list 'desktop-locals-to-save 'evil-markers-alist)
  (define-key evil-normal-state-map " " 'save-buffer))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after (evil org)
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . (lambda () (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-smartparens
  :after smartparens
  :hook
  (smartparens-enabled . evil-smartparens-mode))

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "ü")
  (evil-leader/set-key
	"e" 'find-file
	"b" 'switch-to-buffer
	"k" 'kill-buffer))

(provide 'evil-setup)
;;; evil-setup ends here
