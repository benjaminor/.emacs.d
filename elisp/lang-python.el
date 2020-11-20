;;; package -- setting up a proper python IDE in emacs

;;; Commentary:
;; don't use ipython, jedi, elpy anymore
;; only relying on lsp-mode

;;; Code:

;;;;;;;;;;;;;;;;;;;;;
;;;python
;;;;;;;;;;;;;;;;


(use-package python
  :ensure nil
  :delight python-mode "Python"
  :custom
  (python-shell-interpreter "python")
  :interpreter ("python" . python-mode))

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


(use-package sphinx-doc
  :config
  (add-hook 'python-mode-hook (lambda ()
								(require 'sphinx-doc)
								(sphinx-doc-mode t))))


(provide 'lang-python)
;;; lang-python.el ends here
