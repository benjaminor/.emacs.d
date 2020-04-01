;;; package --- text completion using company and yasnippet

;;; Commentary:
;; Combined use of yasnippet with company with company-mode/backend-with-yas function

;;; Code:

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(setq tab-always-indent 'complete)

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")


(defun company-mode/backend-with-yas (backend)
  "Add :with company-yasnippet to BACKEND if it is possible."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	  backend
	(append (if (consp backend) backend (list backend))
			'(:with company-yasnippet))))

(use-package company-math
  :config
  (setq company-math-allow-latex-symbols-in-faces t)
  )

(use-package company-auctex
  :config
  (company-auctex-init))

(use-package company-jedi
  :disabled
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-to-list 'company-backends 'company-jedi))

(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package  company-statistics
  :defer t
  :config
  (company-statistics-mode))

(use-package helm-company
  :after helm
  :config
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))

(use-package company-quickhelp
  :config
  (use-package pos-tip)
  (company-quickhelp-mode 1))


(use-package company
  :diminish company-mode
  :init
  (global-company-mode 1)
  (general-define-key
   :keymaps 'company-active-map
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   "C-l" 'company-complete-selection)
  :config
  (add-hook 'after-init-hook '(lambda() (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))) )
  (setq company-idle-delay              0.0
		company-minimum-prefix-length   1
		company-show-numbers            t
		company-tooltip-limit           20
		company-dabbrev-downcase        nil
		completion-styles               '(basic substring partial-completion)
		)
  (push 'company-elisp company-backends)
  :bind
  ("C-<tab>" . #'company-complete))

(use-package prescient)
(use-package company-prescient
  :after prescient
  :config
  (company-prescient-mode))

(use-package yasnippet
  :config
  (yas-global-mode t)
  (use-package yasnippet-snippets)
  :bind
  ("C-<return>" . yas-expand-from-trigger-key))


(provide 'text-completion)
;;; text-completion.el ends here
