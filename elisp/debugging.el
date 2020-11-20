;;; Debugging

;;; Commentary:

;;; Code:

;; * Debug on error

(toggle-debug-on-error)
(add-hook 'after-init-hook 'toggle-debug-on-error)

;; * Find bugs in config files

(use-package bug-hunter
  :defer t)

(provide 'debugging)
;;; debugging.el ends here
