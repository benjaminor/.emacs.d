;;; package -- support for C/C++ language
;;; Commentary:
;;; Code:

(use-package cc-mode
  :defer t
  :config
  (setq-default c-basic-offset 4 c-default-style "linux")
  (setq-default tab-width 4 indent-tabs-mode t)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))




(use-package cmake-mode
  :defer t
  :init				; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
	(append
	 '(("CMakeLists\\.txt\\'" . cmake-mode))
	 '(("\\.cmake\\'" . cmake-mode))
	 auto-mode-alist)))




(provide 'lang-cc)
;;; lang-cc.el ends here
