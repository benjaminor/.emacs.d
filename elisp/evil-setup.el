;;; package --- summary
;;; setup for evil-mode

;;; Commentary:

;;; Code:

(use-package evil
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map "\C-e" nil)
  (define-key evil-insert-state-map "\C-a" nil)
  :bind (("C-e" . move-end-of-line)
	 ("C-a" . beginning-of-line)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-magit
  :after magit)

(provide 'evil-setup)
;;; evil-setup ends here
