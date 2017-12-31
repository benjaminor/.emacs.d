;;; package -- setup for flycheck
;;; Commentary:
;;; Using vale and proselint as natural language linters

;;; Code:



(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
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
  :modes (text-mode markdown-mode gfm-mode message-mode))

;;;###autoload
(defun flycheck-proselint-setup ()
  "Add proselist to list of flycheck checkers."
  (add-to-list 'flycheck-checkers 'proselint))

(flycheck-proselint-setup))


  ;; This is old stuff specifically for c++, don't know what to do with it
  ;; (if (string-equal system-type "gnu/linux")
  ;;     (progn
  ;;	(custom-set-variables
  ;;	 '(flycheck-c/c++-clang-executable "clang-3.5")
  ;;	 )))
  ;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  ;; (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))

(use-package flycheck-vale
  :ensure t
  :after flycheck
  :config
  (flycheck-vale-setup)
  (flycheck-add-next-checker 'vale 'proselint)
  ;; (setq flycheck-vale-executable "/usr/local/bin/vale")
  )








;;;;;;;;;;;;;;;;;;;;
;; Spell checking ;;
;;;;;;;;;;;;;;;;;;;;

(use-package ispell
  :ensure nil)

(use-package rw-language-and-country-codes
  :ensure t)

(use-package rw-ispell
  :ensure t)

(use-package rw-hunspell
  :ensure t
  :config
  (setq ispell-program-name "hunspell")
  (setenv "DICPATH" "/usr/share/hunspell/")
  (rw-hunspell-setup)
  (setq ispell-dictionary "en_US_hunspell")
  (defun fd-switch-dictionary()
      (interactive)
      (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "de_DE_hunspell") "en_US_hunspell" "de_DE_hunspell")))
	(ispell-change-dictionary change)
	(message "Dictionary switched from %s to %s" dic change)
	))

  (global-set-key (kbd "<f8>")   'fd-switch-dictionary))




			;TODO: maybe implement automatic switching with wiki mode

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

  (global-set-key (kbd "<f6>") 'ispell-word)
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (global-set-key (kbd "M-<f6>") 'flyspell-check-next-highlighted-word))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/d12frosted/flyspell-correct ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell-correct-helm
  :ensure t
  :config
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))

(provide 'flycheck-setup)
;;; flycheck-setup.el ends here
