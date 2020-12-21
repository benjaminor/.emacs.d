;;; package --- summary
;;; My emacs configuration file;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t -*-

(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------

(add-hook 'after-init-hook
		  '(lambda ()
			 (setq file-name-handler-alist file-name-handler-alist-old
				   gc-cons-threshold (* 100 1024 1024)
				   gc-cons-percentage 0.3)
			 (garbage-collect)) t)


(let ((package-enable-at-startup nil)
      (file-name-handler-alist nil)
      (message-log-max 16384)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6))

  (add-to-list 'load-path (locate-user-emacs-file "elisp"))
  (let ((default-directory (locate-user-emacs-file "lisp/")))
	(normal-top-level-add-subdirs-to-load-path))

  (org-babel-load-file (locate-user-emacs-file "config.org")))

(provide 'init)
;;; init.el ends here
