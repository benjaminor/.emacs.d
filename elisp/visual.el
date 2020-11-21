;;; package -- emacs visual stuff (highlighting)
;;; Commentary:
;;; Code:

;; highlight text
(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :config
  (rainbow-mode))

(use-package highlight-operators)

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))


(use-package doom-modeline
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 50)
  :config
  (column-number-mode)
  (doom-modeline-mode))


(provide 'visual)
;;; visual.el ends here
