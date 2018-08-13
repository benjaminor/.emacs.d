;;; package --- summary
;;; setup for evil-mode

;;; Commentary:

;;; Code:

(use-package evil
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-magit
  :after magit)

(provide 'evil-setup)
;;; evil-setup ends here
