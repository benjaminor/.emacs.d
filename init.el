;;; package --- summary
;;; My emacs configuration file;;;;;;;;;;;;;;;;;;

;; -*- lexical-binding: t -*-
(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "25.3"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; ;; Added by Package.el.  This must come before configurations of
;; ;; installed packages.  Don't delete this line.  If you don't want it,
;; ;; just comment it out by adding a semicolon to the start of the line.
;; ;; You may delete these explanatory comments.
;; (package-initialize)

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
	  `(lambda ()
	     (setq file-name-handler-alist file-name-handler-alist-old
		   gc-cons-threshold 800000
		   gc-cons-percentage 0.1)
	     (garbage-collect)) t)

;; (let ((file-name-handler-alist nil)))

;; (let ((normal-gc-cons-threshold 800000)
;;       (init-gc-cons-threshold most-positive-fixnum))
;;   (setq gc-cons-threshold most-positive-fixnum)
;;   (add-hook 'after-init-hook
;;	    (lambda () (setq gc-cons-threshold 800000))))

;;;;;Code from emacs-bootstrap
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


(require 'base)

(require 'base-theme)

(require 'base-extensions)

(require 'base-functions)

(require 'flycheck-setup)

(require 'text-completion)

(require 'lisp-setup)

(require 'lang-latex)

(require 'lang-python)

(require 'lang-cc)

(require 'debugging)

(require 'lsp-mode-setup)

;;;;;;;;;;;;;, C-MODE ;;;;;;;;;;;;;;;;;;;;;;


;; == Markdown ==
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.text\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode))
  )

;;; Commentary:
;; The Robot Operating System (ROS) requires a number of different disambiguated
;; tools for Emacs editing.  This file has some of the hooks and modes for
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
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))



;; tags for code navigation
(use-package ggtags
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))

;; Add more functionality to dired
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-dired.el
(setq-default dired-dwim-target t)
(use-package diredfl
  :ensure t
  :defer 4
  :config
  (diredfl-global-mode))

(use-package helpful
  :ensure t
  :defer 5
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

(use-package treemacs
  :ensure t
  :commands treemacs)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package dumb-jump
  :ensure t
  :config
  (global-set-key (kbd "C-M-p")
		  (defhydra dumb-jump-hydra (:color blue :columns 3 global-map )
		    "Dumb Jump"
		    ("j" dumb-jump-go "Go")
		    ("o" dumb-jump-go-other-window "Other (when )indow")
		    ("e" dumb-jump-go-prefer-external "Go external")
		    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
		    ("i" dumb-jump-go-prompt "Prompt")
		    ("l" dumb-jump-quick-look "Quick look")
		    ("b" dumb-jump-back "Back"))))

(global-set-key
 (kbd "C-n")
 (defhydra hydra-move
   (:body-pre (next-line))
   "move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-c/c++-clang-executable "clang-3.5")
 '(package-selected-packages
   (quote
    (pyenv-mode url-handlers company-c-headers company-yasnippet company-irony color-theme moe-theme ggtags expand-region hungry-delete beacon elpy undo-tree company-statistics company-math helm-company company-anaconda helm-swoop magit cmake-mode with-editor magit-popup
		(\, git-commit)
		(\, general)
		(\, company-auctex)
		(\, cmake-mode)
		(\, undo-tree)
		(\, ace-window)
		try tabbar which-key helm-ag ag helm-projectile projectile ws-butler yaml-mode use-package markdown-mode hydra helm flycheck auto-complete-auctex auctex yasnippet-snippets whitespace-cleanup-mode wgrep websocket test-simple solarized-theme smartparens slime skewer-mode simple-httpd rw-language-and-country-codes rw-ispell rw-hunspell request-deferred request realgud pos-tip pip-requirements page-break-lines org-projectile-helm org-projectile org-category-capture org-bullets org multiple-cursors macrostep lsp-ui lsp-python lsp-mode loc-changes load-relative latex-preview-pane langtool js2-mode hlinum highlight-defined helm-git-grep helm-flycheck general flyspell-correct-helm flyspell-correct flycheck-vale flycheck-pycheckers exec-path-from-shell ein diminish delight deferred conda company-quickhelp company-lsp company-auctex bug-hunter avy autopair auto-package-update auto-complete attrap ace-window ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(provide 'init)
;;; init.el ends here
