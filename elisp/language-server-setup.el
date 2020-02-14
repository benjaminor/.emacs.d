;;; package -- support the language server protocol
;;; Commentary:
;;; Code:


(use-package lsp-mode
  :after yasnippet
  :commands (lsp lsp-deferred)
  :demand
  :config
  ;; Mainly for lsp readout
  (setq read-process-output-max (* (* 1024 1024) 4))
  (setq lsp-prefer-flymake nil
		lsp-restart 'ignore
		lsp-auto-configure t
		lsp-enable-on-type-formatting t
		lsp-enable-identation t
		lsp-before-save-edits t
		lsp-signature-auto-activate t
		lsp-signature-render-documentation t
		lsp-enable-semantic-highlighting nil
		lsp-enable-text-document-color t
		lsp-auto-guess-root t
		lsp-enable-snippet t
		lsp-idle-delay 0.5
		lsp-clients-clangd-args '("-background-index" "-log=error" "-clang-tidy")
		)
  :hook
  (lsp-enable-which-key-integration)
  (python-mode . lsp-deferred)
  (c-mode-common . lsp-deferred)
  (LaTeX-mode . lsp-deferred)
  (yaml-mode . lsp-deferred)
  (java-mode. lsp-deferred))

(use-package lsp-python-ms
  :if (boundp 'my-lsp-python-ms-executable)
  :init
  ;; load from custom.el
  (setq lsp-python-ms-executable my-lsp-python-ms-executable))

(use-package lsp-java
  :config
  (require 'dap-java))

(use-package lsp-ui
  :after lsp
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
			  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
			  ([remap xref-find-references] . lsp-ui-peek-find-references)
			  ("C-c u" . lsp-ui-imenu))
  :config

  (global-set-key (kbd "M-#") 'xref-find-definitions)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)

  ;; If the server supports custom cross references
  ;; (lsp-ui-peek-find-workspace-symbol "pattern 0")
  )
(use-package company-lsp
  :after (company lsp)
  :commands company-lsp
  :config
  (add-to-list 'company-lsp-filter-candidates '(digestif . nil)))

(use-package dap-mode
  :commands dap-mode
  :config
  (add-hook 'dap-stopped-hook
			(lambda (arg) (call-interactively #'dap-hydra)))
  (dap-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1)
  (require 'dap-python))

(use-package helm-lsp
  :after (helm lsp-mode)
  :commands help-lsp-workspace-symbol
  :config
  ;; got this from issue #1 in helm-lsp repo
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-origami
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-mode))

(provide 'language-server-setup)
;;; language-server-setup.el ends here
