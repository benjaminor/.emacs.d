;;; package -- helper modes to make editing easier
;;; Commentary:
;; clean up whitespaces etc.
;;; Code:


(setq tramp-terminal-type "tramp")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
	  (format "%s\\|%s"
			  vc-ignore-dir-regexp
			  tramp-file-name-regexp))
(setq tramp-verbose 1)

(use-package hungry-delete
  :disabled
  :defer t
  :config
  (global-hungry-delete-mode 1))

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

(use-package format-all
  :hook
  ((latex-mode . format-all-mode)
   (emacs-lisp-mode . format-all-mode)
   (c++-mode . format-all-mode)
   (c-mode . format-all-mode)
   (python-mode . format-all-mode)
   (rustic-mode . format-all-mode)
   (rust-mode . format-all-mode)))

(use-package aggressive-indent
  :disabled
  :config
  (global-aggressive-indent-mode 1))

(use-package origami
  :config
  (global-origami-mode))

;; light following the cursor
(use-package beacon
  :defer 5
  :config
  (beacon-mode 1))



(provide 'editing)
;;; editing.el ends here
