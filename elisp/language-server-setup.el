;;; package -- support the language server protocol
;;; Commentary:
;;; Code:


;; -----------------------------
;; trying out eglot
;; -----------------------------

;; (use-package eglot
;;   :ensure t)


(use-package lsp-mode
  :ensure t
  ;; :load-path "$~/.emacs.d/lisp/lsp-mode"
  :config
  (setq lsp-message-project-root-warning t)
  (use-package lsp-imenu
    :ensure f
    :config
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (use-package lsp-ui-flycheck
	:ensure f ; comes with lsp-mode
	:after (flycheck lsp-mode)
	:config
	(with-eval-after-load 'lsp-mode
	  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1)))))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-python
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'python-mode-hook #'lsp-python-enable)
  )


(provide 'language-server-setup)
;;; language-server-setup.el ends here