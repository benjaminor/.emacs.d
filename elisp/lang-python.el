;;; package -- setting up a proper python IDE in emacs

;;; Commentary:
;; Using anaconda, ipython etc.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;
;;;python
;;;;;;;;;;;;;;;;
;; (use-package anaconda-mode
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'anaconda-eldoc-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto completion is setup in text-completion.el with company-jedi ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package elpy
;;  :defer nil
;;  :config
;;  ;; (setq elpy-modules (delq 'elpy-module-company elpy-modules))
;;  (elpy-enable)
;;  (setq elpy-rpc-python-command "python3")
;;  (with-eval-after-load 'elpy (remove-hook 'elpy-modules 'elpy-module-flymake)))

(use-package python
  :delight python-mode "Python"
  :interpreter ("python" . python-mode)
  :config

  (defun python-switch-interpreter()
	(interactive)
	(let* ((interp python-shell-interpreter)
		   (change (if (string= interp "ipython") "python" "ipython")))
	  (if (string= interp "ipython")
		  (setq python-shell-interpreter "python"
				python-shell-interpreter-args "-i")
		(when (executable-find "ipython")
		  (setq
		   python-shell-interpreter "ipython"
		   python-shell-interpreter-args "--simple-prompt -i")))
	  ;; (setq python-shell-interpreter change)
	  (message "Python interpreter switched from %s to %s" interp change)))
  :bind
  ("<f7>" . 'python-switch-interpreter))

(use-package python-pytest
  :after python
  :custom
  (python-pytest-arguments
   '("--color"          ;; colored output in the buffer
	 "--failed-first"   ;; run the previous failed tests first
	 "--maxfail=5"))    ;; exit in 5 continuous failures in a run
  :config
  (which-key-declare-prefixes-for-mode 'python-mode "SPC pt" "Testing")
  (evil-leader/set-key-for-mode 'python-mode
	"ptp" 'python-pytest-popup
	"ptt" 'python-pytest
	"ptf" 'python-pytest-file
	"ptF" 'python-pytest-file-dwim
	"ptm" 'python-pytest-function
	"ptM" 'python-pytest-function-dwim
	"ptl" 'python-pytest-last-failed)
  )

(use-package pip-requirements
  :delight pip-requirements-mode "PyPA Requirements"
  :preface
  (defun me/pip-requirements-ignore-case ()
	(setq-local completion-ignore-case t))
  :init (add-hook 'pip-requirements-mode-hook #'me/pip-requirements-ignore-case))


(use-package conda
  :config
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  ;; (conda-env-autoactivate-mode t)
  )

(use-package ein
  :config
  (setq ein:completion-backend "use-company-backend")
  (setq ein:jupyter-default-notebook-directory "$HOME/ipnb/"))

(use-package sphinx-doc
  :config
  (add-hook 'python-mode-hook (lambda ()
								(require 'sphinx-doc)
								(sphinx-doc-mode t))))


(provide 'lang-python)
;;; lang-python.el ends here
