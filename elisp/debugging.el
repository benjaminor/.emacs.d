;;; Debugging -- using realgud as a debugger

;;; Commentary:

;;; Code:

;; * Debug on error

(toggle-debug-on-error)
(add-hook 'after-init-hook 'toggle-debug-on-error)

;; * Find bugs in config files

(use-package bug-hunter
  :ensure t
  :defer t)


(use-package realgud
  :ensure t
  :defer t
  :config
  ;; (load-library realgud)
  )

(provide 'debugging)
;;; debugging.el ends here
