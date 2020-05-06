;; package -- helm-setup
;;; Commentary:
;;; setup helm and connected packages
;;; Code:


(use-package helm
  :init

  ;; some nicer icons for helm
  (use-package helm-treemacs-icons
	:after (treemacs)
	:quelpa (helm-treemacs-icons :fetcher github :repo "yyoncho/helm-treemacs-icons")
	:config
	(helm-treemacs-icons-enable))

  (require 'helm-files)
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c")) ;; unset normal helm command prefix
  (global-set-key (kbd "C-x C-r") 'helm-recentf)

  :diminish helm-mode
  :defer 2
  :config
  (setq helm-split-window-inside-p          t
		helm-idle-delay                       0.0
		helm-input-idle-delay                 0.01
		helm-yas-display-key-on-candidate     t
		helm-quick-update                     t
		helm-move-to-line-cycle-in-source     t
		helm-ff-search-library-in-sexp        t
		helm-scroll-amount                    8
		helm-M-x-fuzzy-match                  t
		helm-buffers-fuzzy-matching           t
		helm-recentf-fuzzy-match              t
		helm-semantic-fuzzy-match             t
		helm-imenu-fuzzy-match                t
		helm-ff-file-name-history-use-recentf t
		helm-split-window-default-side        'below
		helm-ff-skip-boring-files             t)
  (helm-adaptive-mode t)
  (helm-mode 1)


  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'spacemacs//hide-cursor-in-helm-buffer)

  (if (string-equal system-type "gnu/linux")
      (setq helm-grep-default-command
			"grep --color=always -d skip %e -n%cH -e %p %f"
			helm-grep-default-recurse-command
			"grep --color=always -d recurse %e -n%cH -e %p %f"))

  :bind (([remap list-buffers] . helm-mini)
		 ([remap find-file] . helm-find-files)
		 ("C-x b" . helm-mini)
		 ("M-x" . helm-M-x)
		 ("C-h a" . helm-apropos)
		 ("M-y" . helm-show-kill-ring)
		 :map helm-command-map
		 ("x" . helm-register)
		 ("g" . helm-google-suggest)
		 :map helm-map
		 ("C-i" . helm-execute-persistent-action)
		 ("C-z" . helm-select-action)
		 ("C-j" . helm-next-line)
		 ("C-k" . helm-previous-line)
		 ("C-h" . helm-next-source)
		 ("C-S-h" . describe-key)
		 ;; ("C-e" . hydra-helm-menu/body)
		 :map helm-find-files-map
		 ("C-l" . helm-execute-persistent-action)
		 ("C-h" . helm-find-files-up-one-level)
		 :map helm-read-file-map
		 ("C-l" . helm-execute-persistent-action)
		 ("C-h" . helm-find-files-up-one-level)))

;; == ag ==
;; Note that 'ag' (the silver searcher) needs to be installed.
;; Ubuntu: sudo apt-get install silversearcher-ag
;; OSX: brew install ag
(use-package ag
  :defer t
  )
(use-package helm-ag
  :defer t
  :after helm
  :config
  (general-define-key :keymaps 'helm-ag-map
					  "C-c C-e" 'helm-ag-edit)
  (bind-key "C-c C-e" 'helm-ag-edit helm-ag-mode-map)
  )

(use-package helm-bibtex
  :after org-ref
  :config
  (setq bibtex-completion-bibliography org-ref-default-bibliography
		bibtex-completion-library-path (concat org-ref-pdf-directory "/")
		bibtex-completion-notes-path org-ref-bibliography-notes))

(use-package helm-rg)

(use-package helm-flx
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ;; t by default
		helm-flx-for-helm-locate t) ;; nil by default
  )


(use-package helm-swoop
  :after helm
  :bind
  ("M-i" . helm-swoop)
  ("M-I" . helm-swoop-back-to-last-point)
  ("C-c M-i" . helm-multi-swoop)
  ("C-x M-i" . helm-multi-swoop-all)
  :config
										; Change the keybinds to whatever you like :)
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; When doing evil-search, hand the word over to helm-swoop

  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  ;;    (setq helm-swoop-speed-or-color nil)

  ;; ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)

  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t)

  ;; If you prefer fuzzy matching
  ;;    (setq helm-swoop-use-fuzzy-match t)

  ;; Disable pre-input
  (setq helm-swoop-pre-input-function
		(lambda () "")))

(use-package helm-make
  :after helm)

(provide 'helm-setup)
;;; helm-setup.el ends here
