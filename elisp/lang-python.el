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
(setq elpy-rpc-python-command "/home/benjamin/anaconda3/bin/python")
(with-eval-after-load 'elpy (remove-hook 'elpy-modules 'elpy-module-flymake)))

(use-package python
  :ensure nil
  :delight python-mode "Python"
  :config
  ;; (setq python-shell-interpreter "/home/data/anaconda3/bin/python"
  ;;	python-shell-interpreter-args "--simple-prompt --pprint")
					;TODO: Setup ipython integration
  (when (executable-find "ipython")
    (setq-default
     python-shell-interpreter "ipython"
     python-shell-interpreter-args "--colors=Linux --profile=default"
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(use-package pip-requirements
  :delight pip-requirements-mode "PyPA Requirements"
  :preface
  (defun me/pip-requirements-ignore-case ()
    (setq-local completion-ignore-case t))
  :init (add-hook 'pip-requirements-mode-hook #'me/pip-requirements-ignore-case))


(provide 'lang-python)
;;; lang-python.el ends here
