;;; Package -- base-function
;;; Commentary:
;;; this is for my functions, my keybindings and my aliases

;;;;; Key bindings ;;;;;;

(global-set-key "\C-x/" 'point-to-register)
(global-set-key "\C-xj" 'jump-to-register)
;;(global-set-key "\C-xc" 'compile)


;;;;Open certain directories easy
(global-set-key "\C-xä" 'my-find-texfiles)
(defun my-find-texfiles ()
  "force a starting path"
  (interactive)
  (let ((default-directory "~/Documents/Latex/"))
    (call-interactively 'helm-find-files)))

(global-set-key "\C-xü" 'my-find-cfiles)
	(defun my-find-cfiles ()
  "force a starting path"
  (interactive)
  (let ((default-directory "~/Documents/c-files/"))
    (call-interactively 'helm-find-files)))

(global-set-key "\C-xö" 'my-find-pythonfile)
(defun my-find-pythonfile ()
  "force a starting path"
  (interactive)
  (let ((default-directory "~/Documents/Python/"))
    (call-interactively 'helm-find-files)))

;; Reload buffer with F5
(global-set-key (kbd "<f5>") 'revert-buffer)

;;
;; Aliases
;;
(defalias 'sh 'shell)
(defalias 'indr 'indent-region)

(setq tab-always-indent 'complete)


;; (defun something
;;    (do-something))

(provide 'base-functions)
;;; base-functions.el ends here
