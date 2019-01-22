;;; package -- support the language server protocol
;;; Commentary:
;;; Code:


;; -----------------------------
;; trying out eglot
;; -----------------------------

;; (use-package eglot)

(use-package lsp-mode
  :commands lsp
  :config
  (add-hook 'prog-mode-hook #'lsp))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp
  :after company
  :commands company-lsp
  :config
  (add-to-list 'company-backends (company-mode/backend-with-yas '(company-lsp))))

(use-package ccls
  :config
  (setq ccls-executable "/usr/local/bin/ccls"))

(provide 'language-server-setup)
;;; language-server-setup.el ends here
