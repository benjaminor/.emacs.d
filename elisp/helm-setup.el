;;; package -- helm-setup
;;; Commentary:
;;; setup helm and connected packages
;;; Code:

(use-package helm
  :init
  (require 'helm-files)
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
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
	helm-ff-file-name-history-use-recentf t
	helm-split-window-default-side        'below
	helm-ff-skip-boring-files             t)
  (helm-adaptive-mode 1)
  (helm-mode 1)
  ;; (with-eval-after-load 'company
  ;;   (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;;   (define-key company-active-map (kbd "C-:") 'helm-company))


  ;;; does not work right now, how ot look over it later
  ;; TODO
  (defhydra hydra-helm-menu (:color pink
				    :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))

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

  :bind (("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 ("C-h a" . helm-apropos)
	 ("M-y" . helm-show-kill-ring)
	 :map helm-map
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 ("C-j" . helm-next-line)
	 ("C-k" . helm-previous-line)
	 ("C-h" . helm-next-source)
	 ("C-S-h" . describe-key)
	 ("C-e" . hydra-helm-menu/body)
	 :map helm-find-files-map
	 ("C-l" . helm-execute-persistent-action)
	 ("C-h" . helm-find-files-up-one-level)
	 :map helm-read-file-map
	 ("C-l" . helm-execute-persistent-action)
	 ("C-h" . helm-find-files-up-one-level)))

(use-package helm-google
  :after helm
  :config
  (global-set-key (kbd "C-h C--") 'helm-google))



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


(use-package helm-git-grep)


(use-package helm-flycheck
  :after (helm flycheck)
  :config
  (define-key flycheck-mode-map (kbd "C-c ! l") nil)
  (define-key flycheck-mode-map (kbd "C-c ! l") 'helm-flycheck))


(use-package helm-swoop
  :config
  (progn
					; Change the keybinds to whatever you like :)
    (global-set-key (kbd "M-i") 'helm-swoop)
    (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

    ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
    (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

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
	  (lambda () ""))))

(provide 'helm-setup)
;;; helm-setup.el ends here
