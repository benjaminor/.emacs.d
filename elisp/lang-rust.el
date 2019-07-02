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
  (progn
    ;; add flycheck support for rust
    ;; https://github.com/flycheck/flycheck-rust
    (use-package flycheck-rust
	  :config
	  (with-eval-after-load 'rust-mode
		(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

    ;; cargo-mode for all the cargo related operations
    (use-package cargo)

    ;; racer-mode for getting IDE like features for rust-mode
    ;; https://github.com/racer-rust/emacs-racer
    (use-package racer
	  :init (setq racer-rust-src-path
				  (concat (string-trim
						   (shell-command-to-string "rustc --print sysroot"))
						  "/lib/rustlib/src/rust/src"))
      :config
      ;; (progn
	  ;; set racer rust source path environment variable
	  ;; (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
	  (defun my-racer-mode-hook ()
		(set (make-local-variable 'company-backends)
			 '((company-capf company-files :with company-yasnippet))))

	  ;; enable company and eldoc minor modes in rust-mode
	  (add-hook 'racer-mode-hook 'my-racer-mode-hook)
	  (add-hook 'racer-mode-hook 'company-mode)
	  (add-hook 'racer-mode-hook 'eldoc-mode)))

  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'rust-mode-hook 'cargo-minor-mode)

  ;; format rust buffers on save using rustfmt
  ;; (add-hook 'before-save-hook
  ;; (lambda ()
  ;; (when (eq major-mode 'rust-mode)
  ;; (rust-format-buffer))))))
  )

(provide 'lang-rust)
;;; lang-rust.el ends here
