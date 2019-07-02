;;; package -- support the language server protocol
;;; Commentary:
;;; Code:


;; -----------------------------
;; trying out eglot
;; -----------------------------

(use-package lsp-mode
  :after yasnippet
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
		lsp-restart 'ignore
		lsp-auto-configure t
		lsp-log-io t
		lsp-auto-guess-root t
		lsp-enable-snippet t
		lsp-print-performance t
		)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'c-mode-common-hook #'lsp))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (global-set-key (kbd "M-#") 'xref-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; (defun my-python-flycheck-setup ()
  ;;	"my flycheck setup for `python-mode'."
  ;;	(flycheck-add-next-checker 'lsp-ui 'python-pycheckers)
  ;;	(flycheck-add-next-checker 'python-pycheckers 'lsp-ui))

  ;; (add-hook 'python-mode 'my-python-flycheck-setup)
  ;; (lsp-ui-peek-find-workspace-symbol "pattern 0")
  ;; If the server supports custom cross references
  ;; (lsp-ui-peek-find-custom 'base "$cquery/base")
  )
(use-package company-lsp
  :after (company lsp)
  :commands company-lsp)

(use-package ccls
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
		 (lambda () (require 'ccls) (lsp))))

(use-package dap-mode
  :disabled
  :commands dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-python))

(use-package helm-lsp
  :after (helm lsp-mode)
  :commands help-lsp-workspace-symbol)

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :commands lsp-treemacs-errors-list)

(provide 'language-server-setup)
;;; language-server-setup.el ends here
