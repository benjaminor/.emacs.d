;;; package -- setting up a proper python IDE in emacs

;;; Commentary:
;; Using anaconda, ipython etc.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;
;;;python
;;;;;;;;;;;;;;;;
;; (defun my/python-mode-hook ()
;;	(add-to-list 'company-backends 'company-jedi))

;;(add-hook 'python-mode-hook 'my/python-mode-hook)
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto completion is setup in text-completion with company-jedi ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package elpy
:ensure t
:config
(setq elpy-modules (delq 'elpy-module-company elpy-modules))
(elpy-enable)

  (setq elpy-rpc-python-command "/home/data/anaconda3/bin/python")
  (setq python-shell-interpreter "/home/data/anaconda3/bin/python")
)



(provide 'lang-python)
;;; lang-python.el ends here
