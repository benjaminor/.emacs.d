;;;;; My emacs configuration file;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delete-selection-mode 1)
					;(require 'iso-transl)
;; Tell emacs where is your personal elisp lib dir
(load-library "url-handlers")

(add-to-list 'load-path "~/.emacs.d/lisp/autopair")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/aribas")
(require 'package)


;;;;;;;;;;;;;;;; ELPA Sources ;;;;;;;;;;;;;;;;

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
		         ;; ("melpa_stable" . "https://stable.melpa.org/packages/")
			 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)


;;;set PATH variable
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))



;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))
;; load the packaged named xyz.




(autoload 'run-aribas "aribas" "Run ARIBAS." t)


;;;;; Key bindings ;;;;;;

(global-set-key "\C-x/" 'point-to-register)
(global-set-key "\C-xj" 'jump-to-register)
;;(global-set-key "\C-xc" 'compile)


;;;;Open certain directories easy
(global-set-key "\C-xä" 'my-find-file)
(defun my-find-file ()
  "force a starting path"
  (interactive)
  (let ((default-directory "~/Documents/Latex/"))
    (call-interactively 'helm-find-files)))

(global-set-key "\C-xü" 'my-find-cfiles)
(defun my-find-cfiles ()
  "force a starting path"
  (interactive)
  (let ((default-directory "~/Documents/c-files/"))
    (call-interactively 'find-file)))

(global-set-key "\C-xp" 'my-find-pythonfile)
(defun my-find-pythonfile ()
  "force a starting path"
  (interactive)
  (let ((default-directory "~/Documents/Python/"))
    (call-interactively 'find-file)))


;;
;; Aliases
;;
(defalias 'sh 'shell)
(defalias 'indr 'indent-region)

(setq tab-always-indent 'complete)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package configuration
;;;;;;;;;;;;;;;;;;;;;;;;;



(use-package autopair)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package try
  :ensure t)

(use-package tabbar
  :ensure t
  :config
  (tabbar-mode 1))

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))


(use-package general
  :ensure t
  )



;;;;;;;;;;;; AUTOCOMPLETION ;;;;;;;;;;;;;;;;;;;;;;

;; yasnippet
;;; should be loaded before auto complete so that they can work together
;; == YASnippet ==
(use-package yasnippet
  :ensure t
  :defer t
  :config (yas-global-mode t)
  )

;; == ws-butler ==
;; This cleans up any whitespace I have at the end of my lines.
(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode)
  :diminish ws-butler-mode
  )


;; ;; Remove Yasnippet's default tab key binding
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

;; ;; Set Yasnippet's key binding to shift+tab
;; (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
					;(load "autocomplete")
;; (use-package company-auctex
;;   :ensure t
;;   :config
;;   (company-auctex-init))


(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)
  (bind-key "C-<tab>" #'company-complete)
  ;; (general-define-key
  ;;  :keymaps 'company-active-map
  ;;  "C-j" 'company-select-next
  ;;  "C-k" 'company-select-previous
  ;;  "C-l" 'company-complete-selection)
  :config
  (setq company-idle-delay              0.1
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	)
   ;; (add-to-list 'company-backends 'company-c-headers)
   ;;    (add-to-list 'company-backends 'company-anaconda)
   ;;    (add-to-list 'company-backends 'company-math-symbols-unicode)
   ;;    (add-to-list 'company-backends 'company-dabbrev-code)
   ;;    (add-to-list 'company-backends 'company-yasnippet)
   ;;    (add-to-list 'company-backends 'company-files)



  )

(use-package  company-statistics
  :ensure t
  :config
  (company-statistics-mode))


;; (use-package auto-complete
;;   :diminish auto-complete-mode
;;   :config
;;   (progn
;;     (use-package go-autocomplete)
;; 	(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;     (setq ac-use-fuzzy t
;;           ac-disable-inline t
;;           ac-use-menu-map t
;;           ac-auto-show-menu t
;;           ac-auto-start t
;;           ac-ignore-case t
;; 		  ac-candidate-menu-min 0)
;; 	(ac-set-trigger-key "TAB")
;; 	(ac-set-trigger-key "<tab>")))






;; ;; (require 'auto-complete)
;; ;; (require 'auto-complete-config)
;; ;; (ac-config-default)

;; ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; ;; (setq-default ac-dwim nil)

;; (use-package ac-math
;;   :ensure t
;;   :config
;;   (add-to-list 'ac-modes 'latex-mode)
;;   (defun ac-LaTex-mode-setup ()
;; 	(setq ac-sources
;; 		  (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;; 				  ac-sources))
;; 	)

;;   (add-hook 'LaTeX-mode-hook 'ac-LaTex-mode-setup)
;;   (setq ac-math-unicode-in-math-p t))




;; (require 'ac-math)
;; (add-to-list 'ac-modes 'latex-mode) ; make auto-complete aware of latex

;; (defun ac-LaTex-mode-setup ()
;;   (setq ac-sources
;; 		(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;; 				ac-sources))
;;   )

;; (add-hook 'LaTeX-mode-hook 'ac-LaTex-mode-setup)

;; (setq ac-math-unicode-in-math-p t)

					;(ac-flyspell-workaround) ; fixes a known bug of delay due to flyspell (if it is there)
					;(add-to-list 'ac-modes 'org-mode) ; auto-complete for org-mode (optional)
					;(require 'auto-complete-config) ; should be after add-to-list 'ac-modes and hooks
					;(ac-config-default)
;; (setq ac-auto-start t)            ; if t starts ac at startup automatically
;; (setq ac-auto-show-menu t)
;; (global-auto-complete-mode t)

;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;, C-MODE ;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :defer t
  :config
  (setq-default c-basic-offset 4 c-default-style "linux")
  (setq-default tab-width 4 indent-tabs-mode t)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))




;; (defun turn-on-outline-minor-mode ()
;;   (outline-minor-mode 1))

;; (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)

;; (setq outline-minor-mode-prefix "\C-c C-o") ; Or something else


(use-package cmake-mode
  :ensure t
  :defer t
  :init
					; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
	(append
	 '(("CMakeLists\\.txt\\'" . cmake-mode))
	 '(("\\.cmake\\'" . cmake-mode))
	 auto-mode-alist))
  )


;; == Markdown ==
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  )

(use-package flyspell
  :defer t
  :diminish (flyspell-mode . " φ"))

;;;;;;;;;;;;;;;;; AUCTEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;For Auctex < 11.82 exchange ";;" in the following 2 lines
;;(require ’tex-site)
;; == LaTex / AucTeX ==
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))


  (defun my-latex-mode-setup ()
    (progn(setq-local company-backends
  		(append '((company-math-symbols-latex company-math-symbols-unicode company-latex-commands))
  			company-backends)))
    (company-auctex-init))
  (add-hook 'LaTeX-mode-hook 'my-latex-mode-setup)


  ;; Don't use Helm for the reftex-citation lookup
  (eval-after-load 'helm-mode
    '(add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil))
    )

  )







(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  ;; Changes the helm prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;; Supress warning
 (setq ad-redefinition-action 'accept)

  :config
  (require 'helm)
  (require 'helm-files)
  (require 'helm-config) ; Necessary for helm-mode

  ;; Additional key bindings
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key [escape] 'helm-keyboard-quit helm-map)
  (bind-key "C-l" (kbd "RET") helm-map)

  (setq helm-split-window-in-side-p           t
	helm-idle-delay                       0.0
	helm-input-idle-delay 0.01
	helm-yas-display-key-on-candidate t
	helm-quick-update t
	helm-move-to-line-cycle-in-source     t
	helm-ff-search-library-in-sexp        t
	helm-scroll-amount                    8
	helm-M-x-fuzzy-match                  t
	helm-ff-file-name-history-use-recentf t)

  (defhydra hydra-helm-menu (:color pink
				    :hint nil)
    " THIS IS INCOMPLETE
^^^^^^^^---------------
d: delete"
    ("d" helm-buffer-run-kill-persistent)
    ("j" helm-next-line)
    ("q" quit-window "quit" :color blue)
    )

  (if (string-equal system-type "gnu/linux")
      (setq helm-grep-default-command
	    "grep --color=always -d skip %e -n%cH -e %p %f"
	    helm-grep-default-recurse-command
	    "grep --color=always -d recurse %e -n%cH -e %p %f"))

  (helm-mode 1)

  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'spacemacs//hide-cursor-in-helm-buffer)

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
	 ("C-h" . helm-find-files-up-one-level)

	 )
  )




;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;; helm-company choose from company completions with C-:
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-:") 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company))




(use-package helm-swoop
  :ensure t
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
    (setq helm-swoop-speed-or-color nil)

    ;; ;; Go to the opposite side of line from the end or beginning of line
    (setq helm-swoop-move-to-line-cycle t)

    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)

    ;; If you prefer fuzzy matching
    (setq helm-swoop-use-fuzzy-match t)))







;; == Projectile ==
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  (projectile-mode)
  (use-package helm-projectile
    :ensure t
    :defer t
    :after helm
    :config
    (helm-projectile-on)
     ;;(general-define-key
     ;; :prefix gjs-leader-key
     ;; :states '(normal motion)
     ;; ;; Ensure (leader p) maps to the projectile bindings
     ;; "p" '(:keymap projectile-command-map :which-key "Projectile")
     ;; "s" '(helm-projectile-ag :which-key "projectile ag")
     ;; "p/" '(helm-projectile-ag)
     ;; )
    )
  )

;; == ag ==
;; Note that 'ag' (the silver searcher) needs to be installed.
;; Ubuntu: sudo apt-get install silversearcher-ag
;; OSX: brew install ag
(use-package ag
  :ensure t
  :defer t
  )
(use-package helm-ag
  :ensure t
  :defer t
  :after helm
  :config
  (general-define-key :keymaps 'helm-ag-map
		      "C-c C-e" 'helm-ag-edit)
  (bind-key "C-c C-e" 'helm-ag-edit helm-ag-mode-map)
  )

;; == compile ==

;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
  :ensure t
  :defer t
  :config (progn
	    (defun my/ansi-colorize-buffer ()
	      (let ((buffer-read-only nil))
		(ansi-color-apply-on-region (point-min) (point-max))))
	    (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))


;;Flymake;;;;;;;;;;;
;; (require 'flymake)
;; (defun flymake-get-tex-args (file-name)
;;   (list "pdflatex"
;; 		(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
;; (add-to-list
;;  `flymake-err-line-patterns
;;  '("Runaway argument?" nil nil nil)) ; fixes unbalanced braces in LaTeX files
;; ;;(add-hook 'LaTeX-mode-hook 'flymake-mode)
;; (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
;; (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;     (setq flymake-check-was-interrupted t))
;; (ad-activate 'flymake-post-syntax-check)
;; (delete-selection-mode 1)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;; 	(auto-complete-clang auto-complete-c-headers auto-complete-auctex ac-math))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;; 	(hydra yaml-mode use-package flycheck auto-complete-auctex auctex ac-math))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )



;; == undo-tree ==
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))



;; == magit ==
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-diff-options (quote ("--word-diff")))
  (setq magit-diff-refine-hunk 'all)
  )

;; == flycheck ==
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; check OS type
  (if (string-equal system-type "gnu/linux")
      (progn
	(custom-set-variables
	 '(flycheck-c/c++-clang-executable "clang-3.5")
	 )))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  );;;; Flycheck;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;
;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))
;;;;;;;;;;;;;;;;







;;; Commentary:
;; The Robot Operating System (ROS) requires a number of different disambiguated
;; tools for emacs editing. This file has some of the hooks and modes for
;; working with the various files.


;;; Code:

;; == File types ==
;; Web-mode for .launch files (effectively xml)
(add-to-list 'auto-mode-alist '("\\.launch?\\'" . web-mode))

;; == YAML Mode ==
(use-package yaml-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  )











;; old:::


;; (require 'tex)

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)


;; (setq TeX-PDF-mode t) ;; .pdf statt .dvi per default:
;; ;;Zeilenumbruch
;; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; ;(add-hook 'LaTeX-mode-hook 'visual-line-mode)

;; ;;Syntax Higlight
;; (add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
;; ;; Mathe Modus
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; ;; Reftex einflechten und laden
;; (setq reftex-plug-into-AUCTeX t)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; ;; Satzende ". " statt ". ". " f ̈ur M-k: l ̈oschen bis Satzende usw.
;; (setq sentence-end "[.?!][]\"’)}]*\\($\\| \\| \\)[
;; ;;]*") ;; Da ist ein "Newline in der Zeile!"
;; (setq sentence-end-double-space nil)
;; ;;direkte Rechtschreib Korrektur:
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (setq flyspell-default-dictionary "german8")
;; ;; Nur benutzen falls Auctex > 11.81 mit preview-latex:
;; ;(require 'preview-latex)
;; ;; aspell ist besser als ispell.
;; ;; Zeile kommentieren, falls nicht installiert:
;; (setq-default ispell-program-name "aspell")
;; ;; Deutsche Rechtschreibung falls \usepackage{ngerman}
;; ;; oder german benutzt wird
;; (add-hook 'TeX-language-de-hook
;; 		  (lambda () (ispell-change-dictionary "de")))
;; (add-hook 'TeX-language-en-hook
;; 		  (lambda () (ispell-change-dictionary "english")))



























(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-c/c++-clang-executable "clang-3.5")
 '(package-selected-packages
   (quote
    (company-statistics company-math helm-company company-anaconda helm-swoop magit cmake-mode with-editor magit-popup
			(\, git-commit)
			(\, general)
			(\, company-auctex)
			(\, cmake-mode)
			(\, undo-tree)
			(\, ace-window)
			try tabbar which-key helm-ag ag helm-projectile projectile ws-butler yaml-mode use-package markdown-mode hydra helm flycheck auto-complete-auctex auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

					;(provide '.emacs);;;



;;TODO: lern about flycheck, yasnippet, ac, helm
