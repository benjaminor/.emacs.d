;;; latex --- environment for writing latex documents

;;; Commentary:

;;; Code:



(use-package tex
  :ensure auctex
  :defer t
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

  ;; company and yasnippet setup is handled in text-completion

  ;; Don't use Helm for the reftex-citation lookup
  (eval-after-load 'helm-mode
    '(add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil))
    )

  )

(use-package latex-preview-pane
  :ensure t
  :config
  (latex-preview-pane-enable))


(use-package reftex
  :after auctex
  :hook (LaTeX-mode . reftex-mode))

(provide 'lang-latex)
;;; latex.el ends here