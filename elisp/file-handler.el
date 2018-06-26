;;; package --- file-handler
;;; Commentary:

;;; Code:


;; == File types ==
;; Web-mode for .launch files (effectively xml)
(add-to-list 'auto-mode-alist '("\\.launch?\\'" . web-mode))

;; == YAML Mode ==
(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))


;; == Markdown ==
(use-package markdown-mode
  :defer t
  :mode (("\\.text\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode))
  )


;; == JSON Mode ==
(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

;; XML-mode
(setq auto-mode-alist (cons '("\\.xml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xsl$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xhtml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.page$" . nxml-mode) auto-mode-alist))

(autoload 'xml-mode "nxml" "XML editing mode" t)

(use-package csv-mode)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(provide 'file-handler)
;;; file-handler.el ends here
