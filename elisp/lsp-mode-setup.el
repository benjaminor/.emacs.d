;;; package -- support the language server protocol
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :config
  (use-package lsp-flycheck
	:ensure f ; comes with lsp-mode
	:after flycheck)
  (use-package lsp-imenu
    :ensure f
    :config
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)))

(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-python
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'python-mode-hook #'lsp-python-enable))


(provide 'lsp-mode-setup)
;;; lsp-mode-setup.el ends here
