;;; package -- Fun stuff in emacs

;;; Commentary:
										; it's not really necessary

;;; Code:

(use-package xkcd)

(use-package selectric-mode)

(use-package nyan-mode
  :disabled
  :config
  (nyan-mode)
  (setq nyan-animate-nyancat t))

(use-package fireplace)

(provide 'fun-stuff)
;;; fun-stuff.el ends here
