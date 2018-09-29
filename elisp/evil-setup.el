;;; package --- summary
;;; setup for evil-mode

;;; Commentary:

;;; Code:

(use-package evil
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1)
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-magit
  :after magit)

(use-package evil-org
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
			(lambda ()
			  (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(provide 'evil-setup)
;;; evil-setup ends here
