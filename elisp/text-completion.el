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

(use-package company-math
  :config
  (setq company-math-allow-latex-symbols-in-faces t)
  )

(use-package company-auctex
  :config
  (company-auctex-init))

(use-package company-irony
  :config
  (add-to-list 'company-backends 'company-irony))

;; (use-package company-anaconda
;;   :config
;;   (add-to-list 'company-backends (company-mode/backend-with-yas '(company-anaconda))))

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

(use-package company-box
  :hook (company-mode . company-box-mode)
  :after icons-in-terminal)
;; :config
;; (setq company-box-icons-unknown 'fa_question_circle)


;; (setq company-box-icons-elisp
;;		'((fa_tag :face font-lock-function-name-face) ;; Function
;;		  (fa_cog :face font-lock-variable-name-face) ;; Variable
;;		  (fa_cube :face font-lock-constant-face) ;; Feature
;;		  (md_color_lens :face font-lock-doc-face))) ;; Face

;; (setq company-box-icons-yasnippet 'fa_bookmark))

(use-package company
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
  (add-hook 'after-init-hook '(lambda() (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))) )
  (setq company-idle-delay              0.1
		company-minimum-prefix-length   2
		company-show-numbers            t
		company-tooltip-limit           20
		company-dabbrev-downcase        nil
		completion-styles               '(basic substring partial-completion)
		)
  ;; (push 'company-irony company-backends)
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



;; Auctex completion::
;; (add-to-list 'company-backends (company-mode/backend-with-yas '()))

;; (add-to-list 'company-backends (company-mode/backend-with-yas '(company-math-symbols-latex comany-math-symbols-unicode company-latex-commands)))

(provide 'text-completion)
;;; text-completion.el ends here
