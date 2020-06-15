;;; latex --- environment for writing latex documents

;;; Commentary:

;;; Code:



(use-package tex
  :quelpa (auctex)
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook
  ((LaTeX-mode . visual-line-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . turn-on-reftex))
  :config
  (setq TeX-auto-save t
		TeX-parse-self t
		TeX-save-query nil
		TeX-PDF-mode t)
  (setq bibtex-dialect 'biblatex)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (TeX-global-PDF-mode t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))


  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-engine 'luatex)))

  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
		TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
			#'TeX-revert-document-buffer)

  ;; " expands into csquotes macros
  (setq LaTeX-csquotes-close-quote "}"
		LaTeX-csquotes-open-quote "\\enquote{")
  ;; company and yasnippet setup is handled in text-completion
  (add-hook 'TeX-mode-hook 'prettify-symbols-mode)
  ;; Don't use Helm for the reftex-citation lookup
  (eval-after-load 'helm-mode
	'(add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil))))

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))

(use-package auctex-latexmk
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(use-package reftex
  :ensure nil
  :after auctex
  :hook (LaTeX-mode . reftex-mode))


(provide 'lang-latex)
;;; lang-latex.el ends here
