;; lang-rust --- rust-mode, racer, cargo

;;; Commentary:
;; Taken from http://emacs-bootstrap.com/

;;; Code:

(use-package rustic
  :after lsp
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq rustic-lsp-server 'rust-analyzer)
  (rustic-doc-mode))

(provide 'lang-rust)
;;; lang-rust.el ends here
