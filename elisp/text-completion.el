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
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
	    '(:with company-yasnippet))))

(use-package company-elisp
  :ensure nil
  :after company
  :config
  (add-to-list 'company-backends (company-mode/backend-with-yas '(company-elisp))))

(use-package company-math
  :ensure t
  :config
  (setq company-math-allow-latex-symbols-in-faces t)
  )

(use-package company-auctex
  :ensure t
  :defer t
  :config
 ;;; (company-auctex-init)
  (add-to-list 'company-backends (company-mode/backend-with-yas '(company-math-symbols-latex company-math-symbols-unicode company-latex-commands company-auctex-labels company-auctex-bibs company-auctex-macros company-auctex-symbols company-auctex-environments))))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends (company-mode/backend-with-yas '(company-anaconda))))

(use-package company-jedi
  :ensure t
  :disabled
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-to-list 'company-backends 'company-jedi))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-lsp
  :ensure t
  :after lsp-mode
  :config
  (add-to-list 'company-backends (company-mode/backend-with-yas '(company-lsp))))


(use-package  company-statistics
  :ensure t
  :defer t
  :config
  (company-statistics-mode))

(use-package helm-company
  :ensure t
  :after helm
  :config
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))

(use-package company-quickhelp
  :ensure t
  :config
  (use-package pos-tip
    :ensure t)
  (company-quickhelp-mode 1))

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)
  ;;TODO: replace (bind-key with :bind)
  ;;(bind-key "C-<tab>" #'company-complete)
  (general-define-key
   :keymaps 'company-active-map
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   "C-l" 'company-complete-selection)
  :config
  (setq company-idle-delay              0.1
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	completion-styles               '(basic substring partial-completion)
	)
  (add-to-list 'company-backends (company-mode/backend-with-yas '(company-irony)))
  :bind
  ("C-<tab>" . #'company-complete))


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (use-package yasnippet-snippets
    :ensure t)
  :bind
  ("C-<return>" . yas-expand-from-trigger-key))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; Auctex completion::
;; (add-to-list 'company-backends (company-mode/backend-with-yas '()))

  ;; (add-to-list 'company-backends (company-mode/backend-with-yas '(company-math-symbols-latex comany-math-symbols-unicode company-latex-commands)))



(provide 'text-completion)
;;; text-completion.el ends here
