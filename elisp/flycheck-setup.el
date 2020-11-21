;;; package -- setup for flycheck
;;; Commentary:
;;; Using vale and proselint as natural language linters

;;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  :custom
  (flycheck-checker-error-threshold 10000))


(use-package flycheck-color-mode-line
  :after flycheck
  :config
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-yamllint
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(use-package flycheck-pycheckers
  :after flycheck
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
  (setq flycheck-pycheckers-checkers '(pylint flake8 bandit mypy3)))

;; https://github.com/jyp/attrap
;; Fix the flycheck-error at point (currently for Lisp and Haskell ;;
(use-package attrap
  :bind (("C-x /" . attrap-attrap)))

;;;###autoload
(defun flycheck-proselint-setup ()
  "Add proselist to list of flycheck checkers."
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
			  (id (one-or-more (not (any " "))))
			  " "
			  (message (one-or-more not-newline)
					   (zero-or-more "\n" (any " ") (one-or-more not-newline)))
			  line-end))
    :modes (text-mode markdown-mode gfm-mode message-mode)
    (add-to-list 'flycheck-checkers 'proselint))

  (flycheck-proselint-setup))

(use-package flycheck-vale
  :defer t
  :after flycheck
  :config
  (flycheck-vale-setup)
  (flycheck-add-next-checker 'vale 'proselint)
  )

;;;;;;;;;;;;;;;;;;;;
;; Spell checking ;;
;;;;;;;;;;;;;;;;;;;;


(use-package ispell
  :ensure nil
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_US")
  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
		   (change (if (string= dic "de_DE") "en_US" "de_DE")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)
      ))

  (global-set-key (kbd "<f8>")   'fd-switch-dictionary))


(use-package flyspell
  :ensure nil
  :config
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  (dolist (mode '(emacs-lisp-mode-hook
				  inferior-lisp-mode-hook
				  clojure-mode-hook
				  python-mode-hook
				  js-mode-hook
				  R-mode-hook))
    (add-hook mode
			  '(lambda ()
				 (flyspell-prog-mode))))
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (global-set-key (kbd "<f6>") 'ispell-word)
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (global-set-key (kbd "M-<f6>") 'flyspell-check-next-highlighted-word))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/mhayashi1120/Emacs-langtool ;;
;; Languagetool setup				  ;;
;; this is outcommented now, will reconfigure it again when I need it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package langtool
;;   :defer t
;;   :config
;;   (setq langtool-default-language "en-US")
;;   (setq langtool-mother-tongue "de")
;;   (setq langtool-language-tool-jar "$HOME/LanguageTool-4.0/languagetool-commandline.jar")
;;   (defun langtool-autoshow-detail-popup (overlays)
;;     (when (require 'popup nil t)
;;       ;; Do not interrupt current popup
;;       (unless (or popup-instances
;; 				  ;; suppress popup after type `C-g` .
;; 				  (memq last-command '(keyboard-quit)))
;; 		(let ((msg (langtool-details-error-message overlays)))
;; 		  (popup-tip msg)))))
;;   (setq langtool-autoshow-message-function
;; 		'langtool-autoshow-detail-popup))

(provide 'flycheck-setup)
;;; flycheck-setup.el ends here
