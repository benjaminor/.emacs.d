;;; package -- support for C/C++ language
;;; Commentary:
;;; Code:

(use-package cc-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete)
  (setq-default c-basic-offset 4
				c-default-style "linux"
				gdb-many-windows t
				tab-width 4
				indent-tabs-mode t)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

(use-package cmake-mode
  :defer t
  :init             ; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
		(append
		 '(("CMakeLists\\.txt\\'" . cmake-mode))
		 '(("\\.cmake\\'" . cmake-mode))
		 auto-mode-alist)))

(use-package cmake-font-lock
  :defer t
  :after cmake-mode
  :hook
  (cmake-mode cmake-font-lock-activate)
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t))

(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode))

(use-package disaster)

(use-package helm-ctest
  :after helm)

(provide 'lang-c)
;;; lang-c.el ends here
