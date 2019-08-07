;;; package -- support for C/C++ language
;;; Commentary:
;;; Code:

(use-package cc-mode
  :ensure nil
  :config
  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete)
  (setq-default c-basic-offset 4
				c-default-style "linux"
				gdb-many-windows t
				tab-width 4
				indent-tabs-mode t)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

(use-package semantic
  :disabled
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (semantic-mode 1))

(use-package ede
  :config
  ;; enable EDE only in C/C++
  (global-ede-mode))

;; tags for code navigation
(use-package ggtags
  :config
  (ggtags-mode 1)
  (add-hook 'c-mode-common-hook
			(lambda ()
			  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
				(ggtags-mode 1))))
  (dolist (map (list ggtags-mode-map))
    (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
    (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
    (define-key map (kbd "C-c g r") 'ggtags-find-reference)
    (define-key map (kbd "C-c g f") 'ggtags-find-file)
    (define-key map (kbd "C-c g c") 'ggtags-create-tags)
    (define-key map (kbd "C-c g u") 'ggtags-update-tags)
    (define-key map (kbd "M-.")     'ggtags-find-tag-dwim)
    (define-key map (kbd "M-,")     'pop-tag-mark)
    (define-key map (kbd "C-c <")   'ggtags-prev-mark)
    (define-key map (kbd "C-c >")   'ggtags-next-mark)))


(use-package cmake-mode
  :defer t
  :init				; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
		(append
		 '(("CMakeLists\\.txt\\'" . cmake-mode))
		 '(("\\.cmake\\'" . cmake-mode))
		 auto-mode-alist)))




(provide 'lang-c)
;;; lang-cc.el ends here
