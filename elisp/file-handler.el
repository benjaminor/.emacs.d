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
(use-package nxml-mode
  :ensure nil
  :mode (("\\.xml$" . nxml-mode)
	 ("\\.xsl$" . nxml-mode)
	 ("\\.xhtml$" . nxml-mode)
	 ("\\.page$" . nxml-mode)
	 ("\\.xslt$" .  nxml-mode)))

(autoload 'xml-mode "nxml" "XML editing mode" t)

(use-package csv-mode)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(provide 'file-handler)
;;; file-handler.el ends here
