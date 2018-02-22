;;; package --- summary
;;; My emacs configuration file;;;;;;;;;;;;;;;;;;

;; Improve startup time.
(let ((file-name-handler-alist nil))
(setf gc-cons-threshold 100000000)

;;;;;Code from emacs-bootstrap
(add-to-list 'load-path (concat user-emacs-directory "elisp"))


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
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  )



;; tags for code navigation
(use-package ggtags
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))

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


(setf gc-cons-threshold 800000)
(provide 'init))
;;; init.el ends here
