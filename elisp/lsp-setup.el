;;; package -- support the language server protocol
;;; Commentary:
;;; Code:


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-ö")
  :after yasnippet
  :demand
  :config
  ;; Mainly for lsp readout
  (setq read-process-output-max (* (* 1024 1024) 4))
  (setq lsp-restart 'ignore
		lsp-before-save-edits t
		lsp-signature-auto-activate nil
		lsp-signature-render-documentation t
		lsp-enable-text-document-color t
		lsp-headerline-breadcrumb-enable t
		lsp-auto-guess-root t
		lsp-enable-snippet t
		lsp-idle-delay 0.3
		lsp-rust-server 'rust-analyzer
		lsp-clients-clangd-args '("-background-index" "-j=2" "-log=error" "-clang-tidy")
		)
  (add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf))))
  :hook
  (lsp-managed-mode . lsp-diagnostics-modeline-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  (python-mode . lsp-deferred)
  (rustic-mode . lsp-deferred)
  (c-mode-common . lsp-deferred)
  (latex-mode . lsp-deferred)
  (tex-mode . lsp-deferred)
  (yaml-mode . lsp-deferred))

(use-package lsp-python-ms
  :if (boundp 'my-lsp-python-ms-executable)
  :init
  ;; load from custom.el
  (setq lsp-python-ms-executable my-lsp-python-ms-executable))

(use-package lsp-java
  :hook
  (java-mode . lsp-deferred))


(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
			  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
			  ([remap xref-find-references] . lsp-ui-peek-find-references)
			  ("C-c u" . lsp-ui-imenu))
  :config
  (setq lsp-ui-doc-enable t
		lsp-ui-doc-header t
		lsp-ui-doc-include-signature t
		lsp-ui-doc-delay 0.5
		lsp-ui-doc-position 'top
		;; lsp-ui-doc-border (face-foreground 'default)
		lsp-ui-sideline-enable t
		;; lsp-ui-doc-use-childframe nil
		lsp-ui-sideline-ignore-duplicate t
		lsp-ui-sideline-show-code-actions t)
  ;; If the server supports custom cross references
  )

(use-package posframe)

(use-package dap-mode
  :commands dap-mode
  :after posframe
  :config

  (use-package dap-java
	:after lsp-java
	:ensure nil)

  (add-hook 'dap-stopped-hook
			(lambda (arg) (call-interactively #'dap-hydra)))
  (dap-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1)
  (tooltip-mode 1)
  ;; (dap-ui-controls-mode 1)

  ;; support for different protocols
  (require 'dap-python)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

(use-package helm-lsp
  :after (helm lsp-mode)
  :config
  ;; got this from issue #1 in helm-lsp repo
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package helm-xref
  :after helm)

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-origami
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-mode))

(provide 'lsp-setup)
;;; language-server-setup.el ends here
