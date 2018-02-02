;;; package --- summary
;;; My emacs configuration file;;;;;;;;;;;;;;;;;;

;;; ouf of use as of 2017-12-22
;;; because outsourced in base.el
;;; stays commented for two weeks, the gets deleted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq inhibit-startup-message t)                                           ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                ;;
;;                                                                            ;;
;; ;;;write over marked code                                                  ;;
;; (delete-selection-mode 1)                                                  ;;
;;                                                                            ;;
;; (global-hl-line-mode t)                                                    ;;
;;                                                                            ;;
;; ;(require 'iso-transl)                                                     ;;
;; ;; Tell emacs where is your personal elisp lib dir                         ;;
;;                                                                            ;;
;; (load-library "url-handlers")                                              ;;
;;                                                                            ;;
;; (add-to-list 'load-path "~/.emacs.d/lisp/autopair")                        ;;
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/aribas")               ;;
;; (require 'package)                                                         ;;
;;                                                                            ;;
;;                                                                            ;;
;; ;;;;;;;;;;;;;;;; ELPA Sources ;;;;;;;;;;;;;;;;                             ;;
;;                                                                            ;;
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")        ;;
;;                       ("melpa" . "https://melpa.org/packages/")            ;;
;;                       ))                                                   ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;                                                 ;;
;; (package-initialize)                                                       ;;
;;                                                                            ;;
;;                                                                            ;;
;; ;;;set PATH variable                                                       ;;
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))                    ;;
;; (setq exec-path (append exec-path '("/usr/texbin")))                       ;;
;;                                                                            ;;
;;                                                                            ;;
;;                                                                            ;;
;; ;; Bootstrap `use-package'                                                 ;;
;; (unless (package-installed-p 'use-package)                                 ;;
;;   (package-refresh-contents)                                               ;;
;;   (package-install 'use-package))                                          ;;
;;                                                                            ;;
;;                                                                            ;;
;; (eval-when-compile                                                         ;;
;;   (require 'use-package))                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load the packaged named xyz.
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

(delete-selection-mode)

;; ;;;;; Key bindings ;;;;;;










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;, C-MODE ;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :defer t
  :config
  (setq-default c-basic-offset 4 c-default-style "linux")
  (setq-default tab-width 4 indent-tabs-mode t)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))




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


;;; Should be replaced by flycheck
;; (use-package flyspell
;;   :defer t
;;   :diminish (flyspell-mode . " φ"))








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



;; tags for code navigation
(use-package ggtags
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
  )


;; (use-package moe-theme
;;   :ensure t)
;; (moe-light)





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
		try tabbar which-key helm-ag ag helm-projectile projectile ws-butler yaml-mode use-package markdown-mode hydra helm flycheck auto-complete-auctex auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))


(setf gc-cons-threshold 800000)
(provide 'init))
;;; init.el ends here
