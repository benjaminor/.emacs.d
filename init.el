;;; package --- summary
;;; My emacs configuration file;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t -*-
(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

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


;;;;;Code from emacs-bootstrap
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


(require 'base)

(require 'base-theme)

(require 'base-extensions)

(require 'base-functions)

(require 'evil-setup)

(require 'helm-setup)

(require 'org-setup)

(require 'file-handler)

(require 'flycheck-setup)

(require 'text-completion)

(require 'lisp-setup)

(require 'lang-latex)

(require 'lang-python)

(require 'lang-cc)

(require 'debugging)

(require 'language-server-setup)



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
