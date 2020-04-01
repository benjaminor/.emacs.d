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
		 ("\\.md\\'" . markdown-mode)))


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
  :config
  (autoload 'xml-mode "nxml" "XML editing mode" t))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  :mode (;; ("\\.xml$" . web-mode)
		 ("\\.xsl$" . web-mode)
		 ("\\.xhtml$" . web-mode)
		 ("\\.page$" . web-mode)
		 ("\\.xslt$" .  web-mode)
		 ("\\.launch?\\'" . web-mode)
		 ("\\.html?\\'" . web-mode)
		 ("\\.phtml\\'" . web-mode)
		 ("\\.tpl\\.php\\'" . web-mode)
		 ("\\.[agj]sp\\'" . web-mode)
		 ("\\.as[cp]x\\'" . web-mode)
		 ("\\.erb\\'" . web-mode)
		 ("\\.mustache\\'" . web-mode)
		 ("\\.djhtml\\'" . web-mode)))

(use-package csv-mode)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package pandoc-mode
  :if (executable-find "pandoc")
  :config
  (use-package ox-pandoc)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))



(provide 'file-handler)
;;; file-handler.el ends here
