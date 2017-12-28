;;; Lisp -- environment for writing code in lisp

;;; Commentary:

;;; Code:


;;;;;;;;;;;
;;;;Lisp;;;

(use-package highlight-defined
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

(use-package ielm
   :init (add-hook 'ielm-mode-hook '(lambda () (setq-local scroll-margin 0))))

(use-package lisp-mode
  :ensure nil
  :delight lisp-mode "Lisp")

(provide 'lisp-setup)
;;; lisp-setup.el ends here
