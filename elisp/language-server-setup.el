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
  (setq lsp-prefer-flymake nil)
  (setq lsp-restart 'ignore)
  (add-hook 'prog-mode-hook #'lsp))

(use-package lsp-ui :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; (lsp-ui-peek-find-workspace-symbol "pattern 0")
  ;; If the server supports custom cross references
  ;; (lsp-ui-peek-find-custom 'base "$cquery/base")
  )
(use-package company-lsp
  :after company
  :commands company-lsp
  :config
  (add-to-list 'company-backends (company-mode/backend-with-yas '(company-lsp))))

(use-package ccls
  :config
  (setq ccls-executable "/usr/local/bin/ccls"))

(use-package dap-mode
  :commands dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-python))

(provide 'language-server-setup)
;;; language-server-setup.el ends here
