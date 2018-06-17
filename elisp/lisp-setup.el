;;; Lisp -- environment for writing code in lisp

;;; Commentary:

;;; Code:


(use-package highlight-defined
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

(use-package ielm
  :defer t
  :init (add-hook 'ielm-mode-hook '(lambda () (setq-local scroll-margin 0))))

(use-package lisp-mode
  :delight lisp-mode "Lisp")

(use-package slime
  :disabled t
  :defer t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))


(provide 'lisp-setup)
;;; lisp-setup.el ends here
