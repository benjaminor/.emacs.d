;;; package --- summary
;;; My emacs configuration file;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t -*-

(package-initialize)

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
				   gc-cons-threshold 100000000
				   gc-cons-percentage 0.3)
			 (garbage-collect)) t)


;;;;;Code from emacs-bootstrap
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


(require 'base)

(require 'base-theme)

(require 'base-extensions)

(require 'base-functions)

(require 'navigation)

(require 'visual)

(require 'editing)

(require 'evil-setup)

(require 'helm-setup)

(require 'projectile-setup)

(require 'org-setup)

(require 'file-handler)

(require 'flycheck-setup)

(require 'text-completion)

(require 'lisp-setup)

(require 'clojure-setup)

(require 'lang-latex)

(require 'lang-python)

(require 'lang-c)

(require 'lang-rust)

(require 'lang-haskell)

(require 'lang-go)

(require 'debugging)

(require 'language-server-setup)

(require 'fun-stuff)

(require 'git-setup)

(provide 'init)
;;; init.el ends here
