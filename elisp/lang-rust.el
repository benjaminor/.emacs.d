;; lang-rust --- rust-mode, racer, cargo

;;; Commentary:
;; Taken from http://emacs-bootstrap.com/

;; rust-mode
;; https://github.com/rust-lang/rust-mode

;;; Code:

(use-package rust-mode
  :bind ( :map rust-mode-map
		  (("C-c C-t" . racer-describe)))
  :config
  ;; add flycheck support for rust
  ;; https://github.com/flycheck/flycheck-rust
  (use-package flycheck-rust
	:config
	(with-eval-after-load 'rust-mode
	  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

  ;; cargo-mode for all the cargo related operations
  (use-package cargo)

  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package rustic
  :after rust-mode
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(provide 'lang-rust)
;;; lang-rust.el ends here
